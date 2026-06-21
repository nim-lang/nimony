#
#
#           Hexer Compiler
#        (c) Copyright 2026 Andreas Rumpf
#
#    See the file "license.txt", included in this
#    distribution, for details about the copyright.
#

## NIFC-stage call inliner used by dce2.
##
## Restrictions in this first cook:
##   - statement-position calls only (call as a direct stmts child);
##   - single-return procs only (no `(ret …)` mid-body);
##   - `.inline` pragma callees only (threshold == 0 in `InlineInfo`);
##   - same-module callees only — cross-module body fetch via the NIF
##     index is a follow-up.
##
## The splice introduces a `(scope …)` block, declares one fresh `(var)`
## per parameter initialised from the argument, renames every local in
## the inlined body to fresh symbols, and drops the trailing `(ret X)`
## (its result is discarded at statement position).

import std / [tables, assertions, os, sets, hashes]
include ".." / lib / nifprelude
include ".." / lib / compat2
import ".." / lib / symparser
import ".." / lengc / [leng_model]

type
  InlineWeights* = seq[int]
  InlineInfo* = object
    threshold*: int
    weights*: InlineWeights
    guardThreshold*: int
    guards*: InlineWeights
  ModuleAnalysis* = object
    inlineInfo*: Table[SymId, InlineInfo]

const
  DefaultInlineInfo* = InlineInfo(threshold: 100, weights: @[],
                                  guardThreshold: 100, guards: @[])

proc shouldInline*(info: InlineInfo; argScores: openArray[int]): bool =
  var sum = 0
  for i, score in argScores:
    if i < info.weights.len:
      sum += (info.weights[i] * score) div 100
  result = sum >= info.threshold

proc readModuleAnalysis*(infile: string): ModuleAnalysis =
  discard infile
  result = ModuleAnalysis(inlineInfo: initTable[SymId, InlineInfo]())

type
  ForeignModule* = object
    buf*: TokenBuf
    bodies*: Table[SymId, int]              # sym → offset of its (proc …) in `buf`

  InlinerCtx* = object
    moduleSuffix*: string
    counter: int                            # fresh-name suffix
    counterPrefix: string                   # disambiguates passes (hexer vs dce2)
    bodies: Table[SymId, int]               # same-module callee → offset in `src`
    src: ptr TokenBuf                       # the module's parsed buffer
    infos: ptr Table[string, ModuleAnalysis] # cross-module sidecars
    xnifDir: string                         # directory holding `.x.nif`/`.dce.nif`
    maxDepth*: int                          # 0 = unlimited; cross-module mode sets a cap
    foreign: Table[string, ref ForeignModule]
      # Cached cross-module bodies. `ref` so growing the table doesn't
      # invalidate cursors that point into a previously-fetched buffer.
    inProgress*: HashSet[SymId]
      # Currently-being-spliced procs. Recursive `.inline` (direct or
      # mutual) would otherwise cause the splice + re-tr loop in dce2 to
      # recurse forever. dce2 adds the callee sym before the recursive
      # `tr` and removes it after; `trySplice`/`trySpliceVarInit` bail
      # when the sym they were asked to splice is already in this set.
      # `maxDepth`, when non-zero, additionally caps the chain length:
      # cross-module bodies are pre-expanded against their same-module
      # inlines (by the hexer-stage `intraModuleInline` pass), so a deep
      # cross-module cascade is rarely a win and risks runaway growth.

proc initInlinerCtx*(moduleSuffix: string; src: ptr TokenBuf;
                     infos: ptr Table[string, ModuleAnalysis];
                     xnifDir = ""; maxDepth = 0;
                     counterPrefix = "i"): InlinerCtx =
  ## `counterPrefix` is woven into fresh local sym names (`base.0i<n>`,
  ## `returnLabel.0i<n>`). The hexer same-module pass uses `"h"` and
  ## dce2's cross-module pass uses `"d"` so freshly-minted dce2 syms
  ## can never collide with hexer-minted syms that survive in the
  ## `.x.nif` body dce2 is rewriting.
  InlinerCtx(moduleSuffix: moduleSuffix, src: src, infos: infos,
             bodies: initTable[SymId, int](),
             xnifDir: xnifDir,
             maxDepth: maxDepth,
             counterPrefix: counterPrefix,
             foreign: initTable[string, ref ForeignModule](),
             inProgress: initHashSet[SymId]())

proc indexProcBodies(buf: var TokenBuf; bodies: var Table[SymId, int]) =
  ## Walks the top-level `(stmts …)` and records `(proc :sym …)` decls
  ## by sym → byte offset into `buf`.
  var n = beginRead(buf)
  if n.stmtKind == StmtsS:
    n.into:
      while n.hasMore:
        if n.kind == ParLe and n.stmtKind == ProcS:
          let nameCur = n.firstSon            # the (proc :sym …) name child
          if nameCur.kind == SymbolDef:
            bodies[nameCur.symId] = cursorToPosition(buf, n)
        skip n
  endRead(buf)

proc collectProcBodies*(c: var InlinerCtx) =
  ## Index the current module's own bodies.
  indexProcBodies(c.src[], c.bodies)

proc findForeignFile(c: InlinerCtx; modul, ext: string): string =
  ## Search the caller's dir first, then the parent — system modules
  ## land in the top-level nimcache while per-test build subdirs hold
  ## only the user module's own artefacts. Returns "" when not found.
  ## (Once the build system passes module→path explicitly we can drop
  ## the search and look up by name.)
  if c.xnifDir.len == 0: return ""
  let direct = c.xnifDir / modul & ext
  if fileExists(direct): return direct
  let parent = c.xnifDir / ".." / modul & ext
  if fileExists(parent): return parent
  return ""

proc loadForeignAnalysis(c: var InlinerCtx; modul: string): bool =
  ## Cheap: load only the `.dce.nif` sidecar (inline info, uses, offers).
  ## Used by `lookupInlineInfo` to decide whether a call is splice-worthy
  ## before paying the cost of parsing the full `.x.nif`. For a typical
  ## module that calls many non-`.inline` procs from `sysvq0asl`, the
  ## sidecar is ~17× smaller than the `.x.nif`; parsing the latter eagerly
  ## here used to dominate dceEmit wall time on large builds.
  if modul == c.moduleSuffix: return true
  if c.infos == nil: return false
  if modul in c.infos[]: return true
  let dpath = findForeignFile(c, modul, ".dce.nif")
  if dpath.len == 0: return false
  c.infos[][modul] = readModuleAnalysis(dpath)
  result = true

proc loadForeign(c: var InlinerCtx; modul: string): bool =
  ## Lazy-load the foreign `.x.nif` (procedure bodies) — only called from
  ## the actual splice path (`lookupBody`), after `shouldInlineCall` has
  ## already approved the inline. The matching `.dce.nif` is loaded by
  ## `loadForeignAnalysis`; we reuse it via the same cache.
  if modul == c.moduleSuffix: return true
  if modul in c.foreign: return true
  let xpath = findForeignFile(c, modul, ".x.nif")
  if xpath.len == 0: return false
  var fm: ref ForeignModule
  new fm
  fm.buf = parseFromFile(xpath)
  indexProcBodies(fm.buf, fm.bodies)
  c.foreign[modul] = fm
  if c.infos != nil and modul notin c.infos[]:
    discard loadForeignAnalysis(c, modul)
  result = true

proc lookupBody(c: var InlinerCtx; calleeSym: SymId; outCur: var Cursor): bool =
  ## Resolves a callee sym to a cursor pointing at its `(proc …)` decl.
  ## The cursor's refcount keeps the underlying buffer alive for as
  ## long as the cursor is held — `c.foreign` stores `ref
  ## ForeignModule`, so subsequent table growth can't move the
  ## TokenBuf out from under us. Returns false when we don't have a
  ## body for `calleeSym` (extern decl, missing `.x.nif`, etc.).
  let modul = extractModule(pool.syms[calleeSym])
  if modul == c.moduleSuffix:
    if calleeSym in c.bodies:
      outCur = cursorAt(c.src[], c.bodies.getOrQuit(calleeSym))
      return true
    return false
  if not loadForeign(c, modul): return false
  let fm = c.foreign.getOrQuit(modul)
  if calleeSym notin fm.bodies: return false
  outCur = cursorAt(fm.buf, fm.bodies.getOrQuit(calleeSym))
  result = true

proc freshSym(c: var InlinerCtx; orig: SymId): SymId =
  ## Mint a fresh local sym for an inlined body's local. Local names must
  ## have ≤ 1 dot (per `isLocalName`) so dce2's per-module rewrite emits
  ## them unconditionally instead of consulting the global live set —
  ## these syms were minted post-`markLive` and aren't tracked there.
  ## The `0<prefix>` prefix on the counter avoids colliding with existing
  ## numeric-suffixed locals like `result.26`; the `prefix` further
  ## disambiguates between the hexer-stage same-module pass and the
  ## dce2-stage cross-module pass so the latter's fresh syms can't
  ## collide with hexer-minted ones already baked into the `.x.nif`.
  inc c.counter
  let original = pool.syms[orig]
  var base = original
  let dotPos = base.find('.')
  if dotPos >= 0: base.setLen dotPos
  base.add ".0"
  base.add c.counterPrefix
  base.addInt c.counter
  result = pool.syms.getOrIncl(base)

proc scoreArg(a: Cursor): int =
  ## Argument score for the inline heuristic (planned in dce1: 0-100).
  ## A higher score means substituting this argument into the inlined
  ## body is likely to expose more optimisation (constant folding,
  ## branch elimination, etc.).
  case a.kind
  of IntLit, UIntLit, FloatLit, CharLit, StringLit: return 100
  of Symbol: return 50  # treat sym refs as immutable bindings
  of ParLe:
    case a.exprKind
    of TrueC, FalseC, NilC, InfC, NeginfC, NanC: return 100
    of NegC:
      var inner = a
      inc inner
      if inner.kind in {IntLit, UIntLit, FloatLit}: return 100
      return 0
    of DotC, AtC, PatC: return 30  # simple field / index read
    else: return 0                  # complex expression
  else: return 0

proc computeArgScores(callNode: Cursor): seq[int] =
  ## Walks the args of a `(call f arg…)` and builds the per-arg score
  ## vector for `shouldInline`. Uses `into` so the walk is bounded by the
  ## call's own subtree (the closing `)` is virtual under `virtualParRi`).
  result = @[]
  var a = callNode
  a.into:
    skip a                                # past the callee sym
    while a.hasMore:
      result.add scoreArg(a)
      skip a

proc lookupInlineInfo(c: var InlinerCtx; calleeSym: SymId): InlineInfo =
  ## Lazy-loads the foreign module's analysis sidecar (cheap — `.dce.nif`
  ## only, no `.x.nif`) and returns the InlineInfo (or `DefaultInlineInfo`
  ## if unknown). The full `.x.nif` is loaded later by `lookupBody`, only
  ## once we've decided this call is actually splice-worthy.
  result = DefaultInlineInfo
  if c.infos == nil: return
  let modul = extractModule(pool.syms[calleeSym])
  if modul != c.moduleSuffix and modul notin c.infos[]:
    discard loadForeignAnalysis(c, modul)
  if not c.infos[].hasKey(modul): return
  result = c.infos[].getOrQuit(modul).inlineInfo.getOrDefault(calleeSym, DefaultInlineInfo)

proc shouldInlineCall(c: var InlinerCtx; calleeSym: SymId;
                      callNode: Cursor): bool =
  ## Decides whether to splice a call to `calleeSym` at this call site.
  ## `.inline` (threshold 0) always wins; `.noinline` (threshold
  ## ≥ 10000) always loses; everything else goes through the per-call
  ## weighted-score heuristic against the proc's `InlineInfo`.
  let info = lookupInlineInfo(c, calleeSym)
  if info.threshold == 0: return true
  if info.threshold >= 10000: return false
  let scores = computeArgScores(callNode)
  result = shouldInline(info, scores)

proc collectParamSyms(params: Cursor): seq[SymId] =
  result = @[]
  if params.kind != ParLe: return @[]
  var p = params
  p.into:
    while p.hasMore:
      if p.substructureKind == ParamU:
        var q = p
        inc q
        if q.kind == SymbolDef:
          result.add q.symId
      skip p

proc weightOfUse(n: Cursor): int =
  case n.exprKind
  of EqC, NeqC, LeC, LtC: 30
  of AddC, SubC, MulC, DivC, ModC, ShrC, ShlC,
     BitandC, BitorC, BitxorC, BitnotC, NegC,
     AndC, OrC, NotC: 20
  of AtC, PatC: 40
  of CallC: 10
  else:
    case n.stmtKind
    of IfS, WhileS, CaseS, IteS, ItecS, LoopS: 50
    of CallS: 10
    else: 0

proc walkInlineWeights(n: var Cursor; params: Table[SymId, int];
                       weights: var seq[int]; inherited: int) =
  case n.kind
  of Symbol:
    if params.hasKey(n.symId):
      weights[params.getOrQuit(n.symId)] += inherited
    inc n
  of ParLe:
    let w = max(inherited, weightOfUse(n))
    n.into:
      while n.hasMore:
        walkInlineWeights(n, params, weights, w)
  of ParRi:
    discard
  else:
    inc n

proc subtreeTokenCount(n: Cursor; limit: int): int =
  result = 0
  var n = n
  if n.kind != ParLe:
    return 1
  var nested = 0
  while true:
    inc result
    if result > limit:
      return result
    case n.kind
    of ParLe:
      inc nested
      inc n
    of ParRi:
      dec nested
      inc n
      if nested == 0:
        return result
    of EofToken:
      return result
    else:
      inc n

proc computeInlineInfo*(procDecl: Cursor): InlineInfo =
  result = DefaultInlineInfo
  var p = procDecl
  let pd = takeProcDecl(p)
  let params = collectParamSyms(pd.params)
  result.weights = newSeq[int](params.len)

  var hasInline = false
  if pd.pragmas.kind == ParLe:
    var pr = pd.pragmas
    pr.into:                            # scan all pragmas (no early break: the
      while pr.hasMore:                 # `into` epilogue needs the scope drained)
        if pr.kind == ParLe and pr.pragmaKind == InlineP:
          hasInline = true
        skip pr
  if not hasInline:
    return

  result.threshold = 0
  var lookup = initTable[SymId, int]()
  for i, s in params:
    lookup[s] = i
  if lookup.len > 0 and pd.body.kind == ParLe:
    var body = pd.body
    walkInlineWeights(body, lookup, result.weights, 0)

proc analyzeModule*(buf: var TokenBuf): ModuleAnalysis =
  result = ModuleAnalysis(inlineInfo: initTable[SymId, InlineInfo]())
  var n = beginRead(buf)
  if n.stmtKind == StmtsS:
    n.into:
      while n.hasMore:
        if n.kind == ParLe and n.stmtKind == ProcS:
          let nameCur = n.firstSon
          if nameCur.kind == SymbolDef:
            let info = computeInlineInfo(n)
            if info.threshold == 0:
              result.inlineInfo[nameCur.symId] = info
        skip n
  endRead(buf)

proc collectParams(params: Cursor; outSyms: var seq[SymId];

                   outTypes: var seq[Cursor]) =
  outSyms.setLen 0
  outTypes.setLen 0
  if params.kind != ParLe: return
  var p = params
  p.into:
    while p.hasMore:
      if p.substructureKind == ParamU:
        var inner = p
        inc inner                      # past `param` tag
        if inner.kind != SymbolDef:
          skip p
          continue
        outSyms.add inner.symId
        inc inner                      # past symdef
        skip inner                     # pragmas
        outTypes.add inner             # NIFC (param :name <pragmas> <type>)
      skip p

type
  Bindings = object
    rename: Table[SymId, SymId]      ## original param/local sym -> fresh sym
    subst: Table[SymId, Cursor]      ## read-only param sym -> argument subtree
                                     ## to splice at uses (no copy emitted)

proc isSubstitutableArg(c: Cursor): bool =
  ## A literal or nullary constant — stable across the whole body, so it can be
  ## spliced at every use instead of bound to a parameter copy. (Symbol args are
  ## NOT included: the inliner cannot prove the caller's variable is unmodified
  ## during the body without alias info.)
  case c.kind
  of IntLit, UIntLit, FloatLit, CharLit, StringLit: true
  of ParLe: c.exprKind in {TrueC, FalseC, NilC, InfC, NeginfC, NanC}
  else: false

proc scanParamUsage(c: Cursor; params: HashSet[SymId];
                    assigned, addrTaken: var HashSet[SymId]) =
  ## Record which parameters are an assignment target's root, or have their
  ## address taken, anywhere in the body — those cannot be replaced by a value.
  if c.kind != ParLe: return
  if c.stmtKind in {AsgnS, StoreS}:
    var dst = c.firstSon
    if c.stmtKind == StoreS: skip dst        # `(store value dest)` — dest is 2nd
    let s = rootOf(dst)
    if s in params: assigned.incl s
  elif c.exprKind == AddrC:
    let s = rootOf(c.firstSon)
    if s in params: addrTaken.incl s
  var n = c
  n.into:
    while n.hasMore:
      scanParamUsage(n, params, assigned, addrTaken)
      skip n

proc emitRenamed(dest: var TokenBuf; body: var Cursor;
                 bnd: Bindings) =
  ## Copy `body` (one subtree) into `dest`, applying `rename` to every
  ## SymbolDef and Symbol that is in the table.
  ##
  ## Special-cases the field-name slot of `(kv field value …)` (object
  ## constructor key) and the field slot of `(dot obj field depth)` —
  ## those Symbol refs point at `(fld :name …)` decls in a type, *not*
  ## at body-locals. Nimony's name mangling sometimes lets a param and
  ## a field share a one-dot name (e.g. `proc always(sym: SymId)` whose
  ## body builds `Implication(sym: sym)` — the field `sym.0` and the
  ## param `sym.0` collide in `pool.syms`), so a generic rename would
  ## rewrite the field reference too and break the constructor.
  case body.kind
  of SymbolDef:
    if bnd.rename.hasKey(body.symId):
      dest.addSymDef bnd.rename.getOrQuit(body.symId), body.info
    else:
      dest.add body
    inc body
  of Symbol:
    if bnd.subst.hasKey(body.symId):
      var s = bnd.subst.getOrQuit(body.symId)
      dest.addSubtree s
    elif bnd.rename.hasKey(body.symId):
      dest.addSymUse bnd.rename.getOrQuit(body.symId), body.info
    else:
      dest.add body
    inc body
  of ParLe:
    # `into` bounds `body` to this scope so the child loop terminates at the
    # real-or-virtual `)`; `addParRi` emits a fresh closer (the source `)` is
    # elided under `-d:virtualParRi`).
    if body.substructureKind == KvU:
      # `(kv field value [depth])` — field name slot is verbatim.
      dest.add body
      into body:                            # past `kv` tag
        if body.hasMore:
          dest.add body                     # field name — no rename
          inc body
        while body.hasMore:
          emitRenamed(dest, body, bnd)
      dest.addParRi()
      return
    if body.exprKind == DotC:
      # `(dot obj field [depth])` — field slot (the 2nd child) is
      # verbatim; obj and the optional depth are renameable.
      dest.add body
      into body:                            # past `dot` tag
        if body.hasMore:
          emitRenamed(dest, body, bnd)   # obj
        if body.hasMore:
          dest.add body                     # field — no rename
          inc body
        while body.hasMore:
          emitRenamed(dest, body, bnd)   # depth, etc.
      dest.addParRi()
      return
    dest.add body
    into body:
      while body.hasMore:
        emitRenamed(dest, body, bnd)
    dest.addParRi()
  of ParRi:
    discard
  else:
    dest.add body
    inc body

proc emitRenamedWithRet(dest: var TokenBuf; body: var Cursor;
                        bnd: Bindings;
                        targetSym: SymId; returnLabel: SymId) =
  ## Like `emitRenamed`, but rewrites every `(ret X)` found anywhere in
  ## the subtree to either:
  ##   - `(asgn targetSym X) (jmp returnLabel)` when X has a value and
  ##     `targetSym != SymId(0)`;
  ##   - `(jmp returnLabel)` alone (void return, or void splice — the
  ##     value is discarded).
  ## The matching `(lab :returnLabel)` is appended by the caller after
  ## the body's last statement.
  case body.kind
  of SymbolDef:
    if bnd.rename.hasKey(body.symId):
      dest.addSymDef bnd.rename.getOrQuit(body.symId), body.info
    else:
      dest.add body
    inc body
  of Symbol:
    if bnd.subst.hasKey(body.symId):
      var s = bnd.subst.getOrQuit(body.symId)
      dest.addSubtree s
    elif bnd.rename.hasKey(body.symId):
      dest.addSymUse bnd.rename.getOrQuit(body.symId), body.info
    else:
      dest.add body
    inc body
  of ParLe:
    # See `emitRenamed`: `into` bounds the scope (the closing `)` may be
    # virtual under `-d:virtualParRi`), `addParRi` emits a fresh closer.
    if body.stmtKind == RetS:
      let info = body.info
      into body:                            # enter (ret …)
        if body.hasMore and body.kind != DotToken and targetSym != SymId(0):
          dest.addParLe TagId(AsgnS), info
          dest.addSymUse targetSym, info
          emitRenamed(dest, body, bnd)  # the returned expression
          dest.addParRi()
        else:
          while body.hasMore: skip body     # discard the value
      dest.addParLe TagId(JmpS), info
      dest.addSymUse returnLabel, info
      dest.addParRi()
      return
    if body.substructureKind == KvU:
      # See `emitRenamed`: field-name slot of `(kv …)` is verbatim to
      # avoid collisions with body-locals that share the field name.
      # Inner subtrees still get the ret-rewrite treatment in case the
      # value side hides a `(ret …)`.
      dest.add body
      into body:
        if body.hasMore:
          dest.add body                     # field name — verbatim
          inc body
        while body.hasMore:
          emitRenamedWithRet(dest, body, bnd, targetSym, returnLabel)
      dest.addParRi()
      return
    if body.exprKind == DotC:
      dest.add body
      into body:
        if body.hasMore:
          emitRenamedWithRet(dest, body, bnd, targetSym, returnLabel)
        if body.hasMore:
          dest.add body                     # field — verbatim
          inc body
        while body.hasMore:
          emitRenamedWithRet(dest, body, bnd, targetSym, returnLabel)
      dest.addParRi()
      return
    dest.add body
    into body:
      while body.hasMore:
        emitRenamedWithRet(dest, body, bnd, targetSym, returnLabel)
    dest.addParRi()
  of ParRi:
    discard
  else:
    dest.add body
    inc body

proc emitBody(c: var InlinerCtx; dest: var TokenBuf; body: var Cursor;
              bnd: Bindings; targetSym: SymId) =
  ## Emit the proc body's outer `(stmts …)` with renames, rewriting
  ## every `(ret X)` into a `(jmp returnLabel)` (optionally preceded by
  ## `(asgn targetSym X)` for a bound non-void splice), and append the
  ## matching `(lab :returnLabel)` after the body's last statement.
  ##
  ## A `.inline` proc with a single tail return produces a redundant
  ## `(jmp) (lab)` pair which a C compiler trivially optimises away —
  ## that cost buys us uniform handling of arbitrarily-many `(ret X)`
  ## points so no body shape disqualifies the splice.
  if body.kind != ParLe or body.stmtKind != StmtsS:
    emitRenamed(dest, body, bnd)
    return
  let info = body.info
  inc c.counter
  let returnLabel = pool.syms.getOrIncl(
    "returnLabel.0" & c.counterPrefix & $c.counter)
  dest.addParLe TagId(StmtsS), info
  into body:                                # bounded: source `)` may be virtual
    while body.hasMore:
      emitRenamedWithRet(dest, body, bnd, targetSym, returnLabel)
  dest.addParLe TagId(LabS), info
  dest.addSymDef returnLabel, info
  dest.addParRi()
  dest.addParRi()                           # close (stmts …)

proc bindingsFor(pSyms: seq[SymId]; argCursors: seq[Cursor];
                 body: Cursor; rename: Table[SymId, SymId]): Bindings =
  ## Bundle the param→fresh-sym rename with the set of params that can be
  ## replaced by their argument value (read-only, not addr-taken, substitutable
  ## argument) — those need no `(var :p = arg)` copy.
  result = Bindings(rename: rename, subst: initTable[SymId, Cursor]())
  var paramSet = initHashSet[SymId]()
  for s in pSyms: paramSet.incl s
  var assigned = initHashSet[SymId]()
  var addrTaken = initHashSet[SymId]()
  scanParamUsage(body, paramSet, assigned, addrTaken)
  for i in 0 ..< pSyms.len:
    if isSubstitutableArg(argCursors[i]) and
       pSyms[i] notin assigned and pSyms[i] notin addrTaken:
      result.subst[pSyms[i]] = argCursors[i]

proc seedRenameWalk(c: var InlinerCtx; n: var Cursor;
                    rename: var Table[SymId, SymId]) =
  ## Descend one ParLe subtree, minting a fresh sym for each SymbolDef,
  ## advancing `n` past the subtree. `into` makes this bound-safe whether
  ## the closing `)` is real or virtual (`virtualParRi`).
  n.into:
    while n.hasMore:
      case n.kind
      of SymbolDef:
        if not rename.hasKey(n.symId):
          rename[n.symId] = c.freshSym(n.symId)
        inc n
      of ParLe:
        seedRenameWalk(c, n, rename)
      else:
        inc n

proc seedRenameFromBody(c: var InlinerCtx; body: Cursor;
                       rename: var Table[SymId, SymId]) =
  ## Walk every token in the body and mint a fresh sym for each
  ## SymbolDef found, so local var declarations don't collide between
  ## inline copies in the same scope.
  if body.kind != ParLe: return
  var n = body
  seedRenameWalk(c, n, rename)

proc trySplice*(c: var InlinerCtx; dest: var TokenBuf; n: var Cursor): int =
  ## If `n` points at a `(call f arg…)` statement we can inline, emit
  ## the splice into `dest`, advance `n` past the call, and return the
  ## number of top-level subtrees emitted (one `(scope …)` here).
  ## Otherwise leave `n` and `dest` untouched and return 0.
  if n.kind != ParLe or n.stmtKind != CallS: return 0

  let entry = n
  var probe = n
  inc probe                                # past `call` tag
  if probe.kind != Symbol: return 0
  let calleeSym = probe.symId
  if calleeSym in c.inProgress: return 0     # cycle guard
  if c.maxDepth > 0 and c.inProgress.len >= c.maxDepth: return 0
  if not shouldInlineCall(c, calleeSym, entry): return 0
  var pcur = default(Cursor)
  if not lookupBody(c, calleeSym, pcur): return 0
  let pd = takeProcDecl(pcur)

  var pSyms: seq[SymId] = @[]
  var pTypes: seq[Cursor] = @[]
  collectParams(pd.params, pSyms, pTypes)

  # Arity check against the call's actual arguments. `into` bounds the walk
  # to the call's own subtree (its closing `)` is virtual under virtualParRi).
  var argScan = entry
  var argCount = 0
  argScan.into:
    skip argScan                           # past callee sym
    while argScan.hasMore:
      skip argScan
      inc argCount
  if argCount != pSyms.len: return 0

  # All checks passed — build the rename table and emit unconditionally.
  var rename = initTable[SymId, SymId]()
  for s in pSyms:
    rename[s] = c.freshSym(s)
  seedRenameFromBody(c, pd.body, rename)

  let info = entry.info

  # Capture each argument cursor, then decide which params can be substituted
  # directly (read-only, not addr-taken, substitutable arg) instead of copied.
  var argCursors: seq[Cursor] = @[]
  var ac = probe
  inc ac                                   # past callee sym
  for i in 0 ..< pSyms.len:
    argCursors.add ac
    skip ac
  let bnd = bindingsFor(pSyms, argCursors, pd.body, rename)

  dest.addParLe TagId(ScopeS), info

  # Param bindings: NIFC `(var :p_fresh <pragmas> <type> <value>)`. Substituted
  # params get no binding — their argument is spliced at each use.
  for i in 0 ..< pSyms.len:
    if bnd.subst.hasKey(pSyms[i]): continue
    dest.addParLe TagId(VarS), info
    dest.addSymDef rename.getOrQuit(pSyms[i]), info
    dest.addDotToken()                     # pragmas
    var t = pTypes[i]
    dest.takeTree t                        # parameter type
    dest.addSubtree argCursors[i]          # initializer
    dest.addParRi()                        # close (var …)

  # Splice the body. Void splice: every (ret …) becomes (jmp returnLabel).
  var body = pd.body
  emitBody(c, dest, body, bnd, SymId(0))

  dest.addParRi()                          # close (scope …)

  # Advance the caller's cursor past the original call.
  n = entry
  skip n
  result = 1                               # one `(scope …)` emitted

proc trySpliceVarInit*(c: var InlinerCtx; dest: var TokenBuf; n: var Cursor): int =
  ## If `n` is `(var :tmp <pragmas> <type> (call f arg…))` (the bound
  ## form xelim produces for non-void calls in expression position) and
  ## the call is inlinable, emit:
  ##
  ##   (var :tmp <pragmas> <type> .)
  ##   (scope
  ##     (var :p_fresh <pragmas> <ptype> <arg>)…
  ##     …body with `(ret X)` rewritten to `(asgn tmp X)`…)
  ##
  ## Advances `n` past the original `(var …)` and returns the number of
  ## top-level subtrees emitted (2 here). Otherwise leaves `n` and `dest`
  ## untouched and returns 0.
  if n.kind != ParLe or n.stmtKind != VarS: return 0
  let entry = n
  var probe = n
  inc probe                                # past `var` tag
  if probe.kind != SymbolDef: return 0
  let tmpSym = probe.symId
  # Local syms only — global vars with call initializers are out of
  # scope for this splice (their lifetime / module placement differs).
  if not isLocalName(pool.syms[tmpSym]): return 0
  inc probe                                # past name
  let pragmasCursor = probe
  skip probe                               # past pragmas slot
  let typeCursor = probe
  skip probe                               # past type slot
  if probe.kind != ParLe or probe.stmtKind != CallS: return 0
  let valueCursor = probe

  var callProbe = valueCursor
  inc callProbe                            # past `call` tag
  if callProbe.kind != Symbol: return 0
  let calleeSym = callProbe.symId
  if calleeSym in c.inProgress: return 0     # cycle guard
  if c.maxDepth > 0 and c.inProgress.len >= c.maxDepth: return 0
  if not shouldInlineCall(c, calleeSym, valueCursor): return 0
  var pcur = default(Cursor)
  if not lookupBody(c, calleeSym, pcur): return 0
  let pd = takeProcDecl(pcur)

  var pSyms: seq[SymId] = @[]
  var pTypes: seq[Cursor] = @[]
  collectParams(pd.params, pSyms, pTypes)

  # `into` bounds the arity walk to the call's own subtree (its closing `)`
  # is virtual under virtualParRi).
  var argScan = valueCursor
  var argCount = 0
  argScan.into:
    skip argScan                           # past callee sym
    while argScan.hasMore:
      skip argScan
      inc argCount
  if argCount != pSyms.len: return 0

  # All checks passed — emit unconditionally.
  var rename = initTable[SymId, SymId]()
  for s in pSyms:
    rename[s] = c.freshSym(s)
  seedRenameFromBody(c, pd.body, rename)

  let info = entry.info

  # 1. Re-emit the var, but with `.` as initializer.
  dest.addParLe TagId(VarS), info
  dest.addSymDef tmpSym, info
  var pragmasC = pragmasCursor
  dest.takeTree pragmasC                   # pragmas
  var typeC = typeCursor
  dest.takeTree typeC                      # type
  dest.addDotToken()                       # no initializer
  dest.addParRi()

  # 2. Emit the inlined body wrapped in a (scope …) with param bindings.
  var argCursors: seq[Cursor] = @[]
  var ac = callProbe
  inc ac                                   # past callee sym
  for i in 0 ..< pSyms.len:
    argCursors.add ac
    skip ac
  let bnd = bindingsFor(pSyms, argCursors, pd.body, rename)

  dest.addParLe TagId(ScopeS), info

  for i in 0 ..< pSyms.len:
    if bnd.subst.hasKey(pSyms[i]): continue
    dest.addParLe TagId(VarS), info
    dest.addSymDef rename.getOrQuit(pSyms[i]), info
    dest.addDotToken()                     # pragmas
    var t = pTypes[i]
    dest.takeTree t
    dest.addSubtree argCursors[i]
    dest.addParRi()

  # Splice the body; every `(ret X)` becomes `(asgn tmpSym X) (jmp …)`,
  # with a matching `(lab …)` appended at the tail.
  var body = pd.body
  emitBody(c, dest, body, bnd, tmpSym)

  dest.addParRi()                          # close (scope …)

  # Advance past the original var decl.
  n = entry
  skip n
  result = 2                               # `(var …)` + `(scope …)`

# ---- Same-module inliner pass (called from hexer.nim) ----

proc trIntra*(c: var InlinerCtx; dest: var TokenBuf; n: var Cursor) =
  ## Walks `n` and splices `.inline` calls in-place. Mirrors `dce2.tr`'s
  ## splice paths but without liveness / generic-instance resolution.
  ##
  ## Cross-module behaviour: callees in another module are picked up
  ## automatically when the context's `xnifDir` is non-empty —
  ## `lookupInlineInfo` / `lookupBody` then lazy-load the foreign
  ## `.dce.nif` / `.x.nif`. With `xnifDir == ""` foreign callees are
  ## naturally skipped (their `InlineInfo` defaults to threshold 100, so
  ## `shouldInlineCall` declines).
  case n.kind
  of ParLe:
    let sk = n.stmtKind
    case sk
    of StmtsS, ScopeS:
      # Bound the cursor to this scope with `into` so the child loop stops at
      # the (real or virtual) closing `)`; under `-d:virtualParRi` a sealed
      # scope has no `ParRi` token, so a raw `while n.hasMore` over an
      # unbounded `rem` would walk into siblings. Emit a fresh closer with
      # `addParRi` (the source `)` may be elided).
      dest.add n
      into n:
        while n.hasMore:
          if n.kind == ParLe and n.stmtKind == CallS:
            var probe = n
            inc probe
            let calleeSym =
              if probe.kind == Symbol: probe.symId else: SymId(0)
            var spliced = createTokenBuf(32)
            let nEmitted = trySplice(c, spliced, n)
            if nEmitted > 0:
              if calleeSym != SymId(0):
                c.inProgress.incl calleeSym
              var inner = beginRead(spliced)
              for _ in 0 ..< nEmitted:
                trIntra(c, dest, inner)
              endRead(inner)
              if calleeSym != SymId(0):
                c.inProgress.excl calleeSym
              continue
          trIntra(c, dest, n)
      dest.addParRi()
    of VarS, GvarS, TvarS, ConstS, ProcS:
      # `(var :tmp <pragmas> <type> (call …))` is the bound form `xelim`
      # and the new nifcgen complex-init path emit; route it through the
      # var-init splice. Other locals copy verbatim.
      if sk == VarS:
        var probe = n
        inc probe
        var calleeSym = SymId(0)
        if probe.kind == SymbolDef:
          inc probe          # name
          skip probe         # pragmas
          skip probe         # type
          if probe.kind == ParLe and probe.stmtKind == CallS:
            inc probe
            if probe.kind == Symbol:
              calleeSym = probe.symId
        if calleeSym != SymId(0):
          var spliced = createTokenBuf(32)
          let nEmitted = trySpliceVarInit(c, spliced, n)
          if nEmitted > 0:
            c.inProgress.incl calleeSym
            var inner = beginRead(spliced)
            for _ in 0 ..< nEmitted:
              trIntra(c, dest, inner)
            endRead(inner)
            c.inProgress.excl calleeSym
            return
      dest.add n
      into n:
        while n.hasMore:
          trIntra(c, dest, n)
      dest.addParRi()
    else:
      dest.add n
      into n:
        while n.hasMore:
          trIntra(c, dest, n)
      dest.addParRi()
  of ParRi:
    raiseAssert "ParRi should not be encountered here"
  else:
    dest.takeToken n

proc emitPragmasWithInlineInfo(dest: var TokenBuf; pragmas: Cursor; info: InlineInfo) =
  var p = pragmas
  if p.kind != ParLe:
    dest.addSubtree p
    return

  dest.add p
  inc p
  while p.kind != ParRi:
    if p.kind == ParLe and p.pragmaKind == InlineP:
      dest.add p
      inc p
      dest.addIntLit info.threshold, p.info
      for w in info.weights:
        dest.addIntLit w, p.info
      while p.kind != ParRi:
        skip p
      dest.takeToken p
    else:
      dest.takeTree p
  dest.takeToken p

proc annotateInlinePragmas(dest: var TokenBuf; n: var Cursor;
                           infos: Table[SymId, InlineInfo]) =
  case n.kind
  of ParLe:
    if n.stmtKind == ProcS:
      let tag = n.tagId
      let info = n.info
      let d = takeProcDecl(n)
      dest.addParLe(tag, info)
      dest.addSubtree d.name
      let sym = d.name.symId
      dest.addSubtree d.params
      dest.addSubtree d.returnType
      if infos.hasKey(sym):
        emitPragmasWithInlineInfo(dest, d.pragmas, infos.getOrQuit(sym))
      else:
        dest.addSubtree d.pragmas
      dest.addSubtree d.body
      dest.addParRi()
    else:
      dest.takeToken n
      while n.hasMore:
        annotateInlinePragmas(dest, n, infos)
      dest.takeToken n
  of ParRi:
    raiseAssert "ParRi should not be encountered here"
  else:
    dest.takeToken n

proc annotateInlinePragmas(buf: var TokenBuf; infos: Table[SymId, InlineInfo]) =
  if infos.len == 0: return
  var n = beginRead(buf)
  var dest = createTokenBuf(buf.len)
  annotateInlinePragmas(dest, n, infos)
  endRead(buf)
  buf = ensureMove(dest)

proc intraModuleInline*(moduleSuffix: string; buf: var TokenBuf) =
  ## Same-module inliner pass run as the last step of hexer's `expand`,
  ## so the `.x.nif` we publish has each `.inline` proc body already
  ## cascaded against its same-module callees. Cross-module splicing
  ## (dce2) then needs at most one level of work per call site, since
  ## the foreign body it pulls is already flat. This eliminates the
  ## per-importer redundancy where the same cascade was re-walked once
  ## per dceEmit process.
  var ma = analyzeModule(buf)
  var graphs = initTable[string, ModuleAnalysis]()
  graphs[moduleSuffix] = ensureMove ma
  annotateInlinePragmas(buf, graphs.getOrQuit(moduleSuffix).inlineInfo)
