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
##   - `.inline` pragma callees only (threshold == 0 in `InlineInfo`).
##
## Cross-module callees are supported: `loadForeign` lazy-loads the callee
## module's post-DCE `.c.nif` (see there for why it must be that and not the
## `.x.nif`) and `InlinerCtx.maxTokens` caps how big a foreign body may be.
## Both halves of the decision come out of that one file — the body itself and
## the callee's `(inline THRESHOLD w…)` pragma annotation, which
## `intraModuleInline` wrote at the hexer stage.
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
  ModuleAnalysis = object
    ## Hexer-stage scratch: what `analyzeModule` computes so
    ## `annotateInlinePragmas` can write it into the procs. Importers never
    ## see this type — they read the annotation back with `readInlinePragma`.
    ## (Not exported: `dce1` has an unrelated `ModuleAnalysis` and both
    ## modules are imported together by `pipeline`.)
    inlineInfo: Table[SymId, InlineInfo]

const
  DefaultInlineInfo* = InlineInfo(threshold: 100, weights: @[],
                                  guardThreshold: 100, guards: @[])

proc shouldInline*(info: InlineInfo; argScores: openArray[int]): bool =
  var sum = 0
  for i, score in argScores:
    if i < info.weights.len:
      sum += (info.weights[i] * score) div 100
  result = sum >= info.threshold

proc readInlinePragma*(pragmas: Cursor; outInfo: var InlineInfo): bool =
  ## Recover the `InlineInfo` `intraModuleInline` wrote into the proc's own
  ## `.inline` pragma as `(inline THRESHOLD w…)`. Exactly the transport
  ## `funcsummary`'s `(smry …)` uses and `readSummaryPragma` reads back: the
  ## per-proc information travels *with the proc*, so an importer recovers it
  ## from the same module file it already parsed for the body — no sidecar
  ## section, and no way for the two to disagree.
  ##
  ## Returns false when the proc has no `.inline` pragma, which leaves the
  ## caller's `DefaultInlineInfo` (threshold 100) in place.
  if not pragmas.isTagLit: return false
  result = false
  var p = pragmas
  p.peekInto:                               # early-out on the first `.inline`
    while p.hasMore:
      if p.isTagLit and p.pragmaKind == InlineP:
        # `.inline` alone already means threshold 0 ("always"); the annotation
        # overrides that and appends one weight per parameter.
        outInfo = InlineInfo(threshold: 0, weights: @[],
                             guardThreshold: DefaultInlineInfo.guardThreshold,
                             guards: @[])
        var seenThreshold = false
        p.into:
          while p.hasMore:
            if p.kind == IntLit:
              if not seenThreshold:
                outInfo.threshold = int(p.intVal)
                seenThreshold = true
              else:
                outInfo.weights.add int(p.intVal)
            skip p
        result = true
        break
      skip p

type
  ForeignModule* = object
    buf*: TokenBuf
    bodies*: Table[SymId, int]              # sym → offset of its (proc …) in `buf`
    inlineInfo*: Table[SymId, InlineInfo]   # sym → its `(inline …)` annotation

  InlinerCtx* = object
    moduleSuffix*: string
    counter: int                            # fresh-name suffix
    counterPrefix: string                   # disambiguates passes (hexer vs dce2)
    bodies: Table[SymId, int]               # same-module callee → offset in `src`
    ownInfo: Table[SymId, InlineInfo]       # same-module `(inline …)` annotations
    symUses: Table[SymId, int]
      # How many times each symbol is *read* across the whole module (defs
      # excluded). `trySpliceCond` drops a local's definition, which is only
      # sound when nothing else reads it; a use count taken from the single
      # statement it inspects cannot see the rest of the scope. Kept current
      # as the pass splices: freshly-minted syms are tallied when their
      # subtree is handed back to `trIntra`.
    src: ptr TokenBuf                       # the module's parsed buffer
    xnifDir: string                         # directory holding the `.c.nif`s
    maxDepth*: int                          # 0 = unlimited; cross-module mode sets a cap
    foreign: Table[string, ref ForeignModule]
      # Cached cross-module bodies. `ref` so growing the table doesn't
      # invalidate cursors that point into a previously-fetched buffer.
    maxTokens: int
      # Size cap on a *cross-module* callee body, in tokens (0 = unlimited).
      # `.inline` means threshold 0, i.e. "always", so without a cap a chain of
      # `.inline` accessors cascades up to `maxDepth` and produces basic blocks
      # big enough to exhaust arkham's register allocator ("no staging register
      # available for a spill"). The cap is what makes this pass mean "inline
      # the tiny accessors" rather than "inline everything marked inline".
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
                     xnifDir = ""; maxDepth = 0; maxTokens = 0;
                     counterPrefix = "i"): InlinerCtx =
  ## `counterPrefix` is woven into fresh local sym names (`base.0i<n>`,
  ## `returnLabel.0i<n>`). The hexer same-module pass uses `"h"` and
  ## dce2's cross-module pass uses `"d"` so freshly-minted dce2 syms
  ## can never collide with hexer-minted syms that survive in the
  ## `.x.nif` body dce2 is rewriting.
  InlinerCtx(moduleSuffix: moduleSuffix, src: src,
             bodies: initTable[SymId, int](),
             ownInfo: initTable[SymId, InlineInfo](),
             symUses: initTable[SymId, int](),
             xnifDir: xnifDir,
             maxDepth: maxDepth,
             maxTokens: maxTokens,
             counterPrefix: counterPrefix,
             foreign: initTable[string, ref ForeignModule](),
             inProgress: initHashSet[SymId]())

proc indexProcBodies(buf: var TokenBuf; bodies: var Table[SymId, int];
                     infos: var Table[SymId, InlineInfo]) =
  ## Walks the top-level `(stmts …)` and records `(proc :sym …)` decls
  ## by sym → byte offset into `buf`, along with the `(inline THRESHOLD w…)`
  ## annotation each inlinable proc carries in its pragmas. Reading the
  ## annotation costs nothing extra here — we are already at the decl and
  ## `takeProcDecl` only skips subtrees, it does not walk the body.
  var n = beginRead(buf)
  if n.stmtKind == StmtsS:
    n.into:
      while n.hasMore:
        if n.isTagLit and n.stmtKind == ProcS:
          let nameCur = n.childCursor            # the (proc :sym …) name child
          if nameCur.isSymbolDef:
            bodies[nameCur.symId] = cursorToPosition(buf, n)
            var probe = n
            let d = takeProcDecl(probe)
            var info = DefaultInlineInfo
            if readInlinePragma(d.pragmas, info):
              infos[nameCur.symId] = info
        skip n

proc tallySymUses(n: Cursor; counts: var Table[SymId, int]) =
  ## Add every `Symbol` (use, not `SymbolDef`) occurrence in the subtree to
  ## `counts`. Additive on purpose: the table spans the module buffer plus
  ## every subtree the inliner splices into it during the pass.
  if n.isSymbol:
    counts[n.symId] = counts.getOrDefault(n.symId, 0) + 1
    return
  if not n.isTagLit: return
  var it = n
  it.into:
    while it.hasMore:
      case it.kind
      of Symbol:
        counts[it.symId] = counts.getOrDefault(it.symId, 0) + 1
        inc it
      of TagLit:
        tallySymUses(it, counts)
        skip it
      else:
        inc it

proc collectProcBodies*(c: var InlinerCtx) =
  ## Index the current module's own bodies and inline annotations, and take
  ## the module-wide use count `trySpliceCond` needs.
  indexProcBodies(c.src[], c.bodies, c.ownInfo)
  var n = beginRead(c.src[])
  tallySymUses(n, c.symUses)
  endRead(n)

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

proc loadForeign(c: var InlinerCtx; modul: string): bool =
  ## Lazy-load a foreign module: its proc bodies *and* their inline
  ## annotations come out of the same parse, so `lookupInlineInfo` and
  ## `lookupBody` share one file per module.
  if modul == c.moduleSuffix: return true
  if modul in c.foreign: return true
  let xpath = findForeignFile(c, modul, ".c.nif")
  if xpath.len == 0: return false
  var fm: ref ForeignModule
  new fm
  fm.buf = parseFromFile(xpath)
  indexProcBodies(fm.buf, fm.bodies, fm.inlineInfo)
  c.foreign[modul] = fm
  result = true

proc subtreeTokenCount(n: Cursor; limit: int): int =
  ## Number of tokens the subtree occupies (the physical span; elided
  ## closes do not count). `limit` is only relevant for the comparison the
  ## callers do, the count is exact.
  if not n.isTagLit:
    result = 1
  else:
    result = subtreeWidth(n)

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
  let cur = cursorAt(fm.buf, fm.bodies.getOrQuit(calleeSym))
  if c.maxTokens > 0 and subtreeTokenCount(cur, c.maxTokens) > c.maxTokens:
    return false
  outCur = cur
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
  of IntLit, UIntLit, FloatLit, CharLit, StrLit: return 100
  of Symbol: return 50  # treat sym refs as immutable bindings
  of TagLit:
    case a.exprKind
    of TrueC, FalseC, NilC, InfC, NeginfC, NanC: return 100
    of NegC:
      var inner = a
      inc inner
      if inner.isIntLit or inner.isUIntLit or inner.isFloatLit: return 100
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
  ## The callee's `(inline THRESHOLD w…)` annotation, or `DefaultInlineInfo`
  ## (threshold 100 — never inline) when it has none, is in another module we
  ## cannot find, or is an extern with no body at all.
  let modul = extractModule(pool.syms[calleeSym])
  if modul == c.moduleSuffix:
    return c.ownInfo.getOrDefault(calleeSym, DefaultInlineInfo)
  if not loadForeign(c, modul): return DefaultInlineInfo
  result = c.foreign.getOrQuit(modul).inlineInfo.getOrDefault(calleeSym,
                                                              DefaultInlineInfo)

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
  if not params.isTagLit: return @[]
  var p = params
  p.into:
    while p.hasMore:
      if p.substructureKind == ParamU:
        var q = p
        inc q
        if q.isSymbolDef:
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
  of TagLit:
    let w = max(inherited, weightOfUse(n))
    n.into:
      while n.hasMore:
        walkInlineWeights(n, params, weights, w)
  else:
    inc n

proc computeInlineInfo*(procDecl: Cursor): InlineInfo =
  result = DefaultInlineInfo
  var p = procDecl
  let pd = takeProcDecl(p)
  let params = collectParamSyms(pd.params)
  result.weights = newSeq[int](params.len)

  var hasInline = false
  if pd.pragmas.isTagLit:
    var pr = pd.pragmas
    pr.into:                            # scan all pragmas (no early break: the
      while pr.hasMore:                 # `into` epilogue needs the scope drained)
        if pr.isTagLit and pr.pragmaKind == InlineP:
          hasInline = true
        skip pr
  if not hasInline:
    return

  result.threshold = 0
  var lookup = initTable[SymId, int]()
  for i, s in params:
    lookup[s] = i
  if lookup.len > 0 and pd.body.isTagLit:
    var body = pd.body
    walkInlineWeights(body, lookup, result.weights, 0)

proc analyzeModule(buf: var TokenBuf): ModuleAnalysis =
  result = ModuleAnalysis(inlineInfo: initTable[SymId, InlineInfo]())
  var n = beginRead(buf)
  if n.stmtKind == StmtsS:
    n.into:
      while n.hasMore:
        if n.isTagLit and n.stmtKind == ProcS:
          let nameCur = n.childCursor
          if nameCur.isSymbolDef:
            let info = computeInlineInfo(n)
            if info.threshold == 0:
              result.inlineInfo[nameCur.symId] = info
        skip n

proc collectParams(params: Cursor; outSyms: var seq[SymId];

                   outTypes: var seq[Cursor]) =
  outSyms.setLen 0
  outTypes.setLen 0
  if not params.isTagLit: return
  var p = params
  p.into:
    while p.hasMore:
      if p.substructureKind == ParamU:
        var inner = p
        inc inner                      # past `param` tag
        if not inner.isSymbolDef:
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
    dropDecl: SymId                  ## the callee's RESULT local (every `(ret X)`
                                     ## returns it): renamed to the splice
                                     ## destination, so its `(var …)` decl folds
                                     ## to an assignment (or vanishes) and the
                                     ## ret's `dest = result'` self-copy is
                                     ## elided. SymId(0) = no forwarding.

proc isSubstitutableArg(c: Cursor): bool =
  ## A literal or nullary constant — stable across the whole body, so it can be
  ## spliced at every use instead of bound to a parameter copy. (Symbol args are
  ## NOT included: the inliner cannot prove the caller's variable is unmodified
  ## during the body without alias info.)
  case c.kind
  of IntLit, UIntLit, FloatLit, CharLit, StrLit: true
  of TagLit: c.exprKind in {TrueC, FalseC, NilC, InfC, NeginfC, NanC}
  else: false

proc slotRootOf(c: Cursor): SymId =
  ## Like `rootOf`, but a spine that crosses a pointer dereference targets the
  ## *pointee*, not the named slot: `(*p).f = x` writes through `p`, leaving
  ## `p`'s own value and address untouched. Such through-pointer lvalues yield
  ## `SymId(0)`; a plain slot (`x`, `x.f`, `x[i]`, `conv(T, x)`) yields its base
  ## symbol. This is the deref-aware analogue used by copyprop's `addrRootOf`:
  ## it lets a param that is only ever *written through* still count as
  ## value-stable (its pointer value never changes), so its argument can be
  ## substituted instead of copied.
  result = SymId(0)
  var n = c
  while true:
    case n.kind
    of Symbol: return n.symId
    of TagLit:
      case n.exprKind
      of DerefC, PatC: return SymId(0)       # through-pointer: pointee, not the slot
      of DotC, AtC: inc n                     # field / index: base is the first child
      of ConvC, CastC:
        inc n; skip n                         # `(conv/cast Type operand)` — skip the type
      else: return rootOf(n)                  # unmodelled spine: stay conservative
    else: return SymId(0)

proc scanParamUsage(c: Cursor; params: HashSet[SymId];
                    assigned, addrTaken: var HashSet[SymId]) =
  ## Record which parameters have their *slot* reassigned (a bare `p = …`) or
  ## their *slot* address taken (`addr p`) anywhere in the body — those cannot
  ## be replaced by their argument value. Writes and address-of that go
  ## *through* the pointer (`(*p).f = …`, `addr (*p)`) leave the slot's value
  ## and address intact, so `slotRootOf` deliberately ignores them.
  if not c.isTagLit: return
  if c.stmtKind in {AsgnS, StoreS}:
    var dst = c.childCursor
    if c.stmtKind == StoreS: skip dst        # `(store value dest)` — dest is 2nd
    let s = slotRootOf(dst)
    if s in params: assigned.incl s
  elif c.exprKind in AddrKinds:
    let s = slotRootOf(c.childCursor)
    if s in params: addrTaken.incl s
  var n = c
  n.into:
    while n.hasMore:
      scanParamUsage(n, params, assigned, addrTaken)
      skip n

proc scanRets(n: var Cursor; resultSym: var SymId; found, ok: var bool) =
  ## Walk one subtree looking for `(ret X)`. `ok` stays true iff EVERY ret
  ## returns the same bare symbol, reported in `resultSym`; `found` records
  ## that at least one ret exists. Nested proc decls (if any) are skipped.
  case n.kind
  of TagLit:
    if n.stmtKind == RetS:
      var hasVal = false
      var valIsSym = false
      var valSym = SymId(0)
      into n:
        if n.hasMore and not n.isDotToken:
          hasVal = true
          if n.isSymbol:
            valIsSym = true
            valSym = n.symId
        while n.hasMore: skip n
      if not hasVal or not valIsSym:
        ok = false
      elif not found:
        resultSym = valSym
        found = true
      elif resultSym != valSym:
        ok = false
    elif n.stmtKind == ProcS:
      skip n
    else:
      into n:
        while n.hasMore: scanRets(n, resultSym, found, ok)
  else:
    inc n

proc resultLocalOf(body: Cursor; pSyms: seq[SymId]): SymId =
  ## The callee's result LOCAL for destination forwarding: every `(ret X)` in
  ## the body returns the same bare, body-local, non-param symbol (nimsem's
  ## implicit `result` after lowering). SymId(0) when the pattern doesn't hold.
  var resultSym = SymId(0)
  var found = false
  var ok = true
  var b = body
  scanRets(b, resultSym, found, ok)
  if not (found and ok) or resultSym == SymId(0): return SymId(0)
  if resultSym in pSyms: return SymId(0)          # a param: bound to its arg
  if not isLocalName(pool.syms[resultSym]): return SymId(0)
  result = resultSym

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
      dest.addSubtree body
    inc body
  of Symbol:
    if bnd.subst.hasKey(body.symId):
      var s = bnd.subst.getOrQuit(body.symId)
      dest.addSubtree s
    elif bnd.rename.hasKey(body.symId):
      dest.addSymUse bnd.rename.getOrQuit(body.symId), body.info
    else:
      dest.addSubtree body
    inc body
  of TagLit:
    # `into` bounds `body` to this scope so the child loop terminates at the
    # real-or-virtual `)`; `addParRi` emits a fresh closer (the source `)` is
    # elided under `-d:virtualParRi`).
    if body.substructureKind == KvU:
      # `(kv field value [depth])` — field name slot is verbatim.
      dest.addParLe(body.cursorTagId, body.info)
      into body:                            # past `kv` tag
        if body.hasMore:
          dest.addSubtree body              # field name — no rename
          inc body
        while body.hasMore:
          emitRenamed(dest, body, bnd)
      dest.addParRi()
      return
    if body.exprKind == DotC:
      # `(dot obj field [depth])` — field slot (the 2nd child) is
      # verbatim; obj and the optional depth are renameable.
      dest.addParLe(body.cursorTagId, body.info)
      into body:                            # past `dot` tag
        if body.hasMore:
          emitRenamed(dest, body, bnd)   # obj
        if body.hasMore:
          dest.addSubtree body              # field — no rename
          inc body
        while body.hasMore:
          emitRenamed(dest, body, bnd)   # depth, etc.
      dest.addParRi()
      return
    dest.addParLe(body.cursorTagId, body.info)
    into body:
      while body.hasMore:
        emitRenamed(dest, body, bnd)
    dest.addParRi()
  else:
    dest.addSubtree body
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
      dest.addSubtree body
    inc body
  of Symbol:
    if bnd.subst.hasKey(body.symId):
      var s = bnd.subst.getOrQuit(body.symId)
      dest.addSubtree s
    elif bnd.rename.hasKey(body.symId):
      dest.addSymUse bnd.rename.getOrQuit(body.symId), body.info
    else:
      dest.addSubtree body
    inc body
  of TagLit:
    # See `emitRenamed`: `into` bounds the scope (the closing `)` may be
    # virtual under `-d:virtualParRi`), `addParRi` emits a fresh closer.
    if body.stmtKind == RetS:
      let info = body.info
      into body:                            # enter (ret …)
        if body.hasMore and not body.isDotToken and targetSym != SymId(0):
          if body.isSymbol and
             (body.symId == targetSym or
              bnd.rename.getOrDefault(body.symId) == targetSym):
            skip body                       # `ret result`: dest IS result (forwarded)
          else:
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
    if bnd.dropDecl != SymId(0) and body.stmtKind == VarS:
      var probe = body
      inc probe                             # past the `var` tag
      if probe.isSymbolDef and probe.symId == bnd.dropDecl:
        # The callee's result var: its storage IS the splice destination
        # (renamed), so the decl folds away — an initializer becomes a plain
        # assignment to the destination.
        let vinfo = body.info
        into body:
          skip body                         # name
          skip body                         # pragmas
          skip body                         # type
          if body.hasMore and not body.isDotToken:
            dest.addParLe TagId(AsgnS), vinfo
            dest.addSymUse targetSym, vinfo
            emitRenamed(dest, body, bnd)    # the initializer expression
            dest.addParRi()
          while body.hasMore: skip body     # drain (`.` initializer / extras)
        return
    if body.substructureKind == KvU:
      # See `emitRenamed`: field-name slot of `(kv …)` is verbatim to
      # avoid collisions with body-locals that share the field name.
      # Inner subtrees still get the ret-rewrite treatment in case the
      # value side hides a `(ret …)`.
      dest.addParLe(body.cursorTagId, body.info)
      into body:
        if body.hasMore:
          dest.addSubtree body              # field name — verbatim
          inc body
        while body.hasMore:
          emitRenamedWithRet(dest, body, bnd, targetSym, returnLabel)
      dest.addParRi()
      return
    if body.exprKind == DotC:
      dest.addParLe(body.cursorTagId, body.info)
      into body:
        if body.hasMore:
          emitRenamedWithRet(dest, body, bnd, targetSym, returnLabel)
        if body.hasMore:
          dest.addSubtree body              # field — verbatim
          inc body
        while body.hasMore:
          emitRenamedWithRet(dest, body, bnd, targetSym, returnLabel)
      dest.addParRi()
      return
    dest.addParLe(body.cursorTagId, body.info)
    into body:
      while body.hasMore:
        emitRenamedWithRet(dest, body, bnd, targetSym, returnLabel)
    dest.addParRi()
  else:
    dest.addSubtree body
    inc body

proc emitTailStmt(dest: var TokenBuf; body: var Cursor; bnd: Bindings;
                  targetSym: SymId; returnLabel: SymId) =
  ## Emit a statement that is in TAIL position — the last statement reached
  ## before the inline body's `(lab :returnLabel)`. A tail `(ret X)` becomes its
  ## `(asgn targetSym X)` with NO `(jmp returnLabel)` (control falls straight into
  ## the label). A tail `stmts`/`scope` recurses so ITS last statement inherits
  ## the tail position; anything else is handled by the general `(ret)`-rewriter
  ## (interior returns still jump). Mirrors the arkham value-core's tail handling.
  if body.isTagLit and body.stmtKind == RetS:
    let rinfo = body.info
    into body:
      if body.hasMore and not body.isDotToken and targetSym != SymId(0):
        if body.isSymbol and
           (body.symId == targetSym or
            bnd.rename.getOrDefault(body.symId) == targetSym):
          skip body                          # `ret result`: dest IS result (forwarded)
        else:
          dest.addParLe TagId(AsgnS), rinfo
          dest.addSymUse targetSym, rinfo
          emitRenamed(dest, body, bnd)       # the returned expression
          dest.addParRi()
      else:
        while body.hasMore: skip body        # void return: discard the value
  elif body.isTagLit and body.stmtKind in {StmtsS, ScopeS}:
    dest.addParLe(body.cursorTagId, body.info)       # copy the (stmts/(scope opener
    into body:
      while body.hasMore:
        var nx = body; skip nx
        if not nx.hasMore: emitTailStmt(dest, body, bnd, targetSym, returnLabel)
        else: emitRenamedWithRet(dest, body, bnd, targetSym, returnLabel)
    dest.addParRi()
  else:
    emitRenamedWithRet(dest, body, bnd, targetSym, returnLabel)

proc emitBody(c: var InlinerCtx; dest: var TokenBuf; body: var Cursor;
              bnd: Bindings; targetSym: SymId) =
  ## Emit the proc body's outer `(stmts …)` with renames, rewriting
  ## every `(ret X)` into a `(jmp returnLabel)` (optionally preceded by
  ## `(asgn targetSym X)` for a bound non-void splice), and append the
  ## matching `(lab :returnLabel)` after the body's last statement.
  ##
  ## A tail `(ret X)` — the last statement of the body — is emitted as its
  ## `(asgn targetSym X)` alone: control then falls straight through to the
  ## trailing `(lab :returnLabel)`, so the `(jmp returnLabel)` would be dead.
  ## gcc drops that dead jump for free, but arkham renders `(jmp)`/`(lab)`
  ## verbatim, so eliding it at the source keeps the native output tight while
  ## preserving uniform handling of any interior `(ret X)` points. The label is
  ## still emitted unconditionally (it costs zero machine bytes) so interior
  ## returns' jumps always resolve.
  if not body.isTagLit or body.stmtKind != StmtsS:
    emitRenamed(dest, body, bnd)
    return
  let info = body.info
  inc c.counter
  let returnLabel = pool.syms.getOrIncl(
    "returnLabel.0" & c.counterPrefix & $c.counter)
  # Emit the inlined body as a real variable SCOPE, not a bare `(stmts)`: the
  # callee's fresh locals then belong to *this* scope frame, so the backend frees
  # their registers at the inlined body's end instead of leaking their live range
  # to the end of the *caller* (arkham measures a local's `freeAfter` at its
  # enclosing scope; a `stmts` is not a scope). Without this, N sequential inline
  # sites keep N sets of locals simultaneously live → false register pressure.
  # The `returnLabel` stays INSIDE the scope so every early-return path reaches the
  # scope's exit (its variable kills), never jumping out of a still-open scope.
  dest.addParLe TagId(ScopeS), info
  into body:                                # bounded: source `)` may be virtual
    while body.hasMore:
      var nx = body; skip nx
      if not nx.hasMore:                     # last statement → tail position
        emitTailStmt(dest, body, bnd, targetSym, returnLabel)
      else:
        emitRenamedWithRet(dest, body, bnd, targetSym, returnLabel)
  dest.addParLe TagId(LabS), info
  dest.addSymDef returnLabel, info
  dest.addParRi()
  dest.addParRi()                           # close (scope …)

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
    if pSyms[i] in assigned or pSyms[i] in addrTaken:
      continue                               # slot mutated / addr observed → copy
    let arg = argCursors[i]
    # A read-only param (value-stable per `scanParamUsage`) may be replaced by
    # its argument at every use instead of bound to a fresh `(var)` copy.
    # Pool are always stable. A bare *local* symbol is stable too: the
    # inlined body only ever assigns fresh-renamed locals, never a caller local,
    # so the substituted symbol's value cannot change across the body. Globals
    # are excluded — a nested call in the body could mutate one between uses,
    # whereas the copy captured its entry value.
    if isSubstitutableArg(arg) or
       (arg.isSymbol and isLocalName(pool.syms[arg.symId])):
      result.subst[pSyms[i]] = arg

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
      of TagLit:
        seedRenameWalk(c, n, rename)
      else:
        inc n

proc seedRenameFromBody(c: var InlinerCtx; body: Cursor;
                       rename: var Table[SymId, SymId]) =
  ## Walk every token in the body and mint a fresh sym for each
  ## SymbolDef found, so local var declarations don't collide between
  ## inline copies in the same scope.
  if not body.isTagLit: return
  var n = body
  seedRenameWalk(c, n, rename)

proc trySplice*(c: var InlinerCtx; dest: var TokenBuf; n: var Cursor): int =
  ## If `n` points at a `(call f arg…)` statement we can inline, emit
  ## the splice into `dest`, advance `n` past the call, and return the
  ## number of top-level subtrees emitted (one `(scope …)` here).
  ## Otherwise leave `n` and `dest` untouched and return 0.
  if not n.isTagLit or n.stmtKind != CallS: return 0

  let entry = n
  var probe = n
  inc probe                                # past `call` tag
  if not probe.isSymbol: return 0
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
  if not n.isTagLit or n.stmtKind != VarS: return 0
  let entry = n
  var probe = n
  inc probe                                # past `var` tag
  if not probe.isSymbolDef: return 0
  let tmpSym = probe.symId
  # Local syms only — global vars with call initializers are out of
  # scope for this splice (their lifetime / module placement differs).
  if not isLocalName(pool.syms[tmpSym]): return 0
  inc probe                                # past name
  let pragmasCursor = probe
  skip probe                               # past pragmas slot
  let typeCursor = probe
  skip probe                               # past type slot
  if not probe.isTagLit or probe.stmtKind != CallS: return 0
  let valueCursor = probe

  var callProbe = valueCursor
  inc callProbe                            # past `call` tag
  if not callProbe.isSymbol: return 0
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
  # Result-var forwarding: when every `(ret X)` returns the same body local
  # (nimsem's implicit `result` after lowering), that local's storage IS the
  # splice destination — rename it to `tmpSym` (overriding the fresh sym the
  # seed walk minted), fold its decl away (`dropDecl`) and let the ret rewrite
  # elide the `dest = result'` self-copy. This is the residue shoggoth's
  # copyprop cannot clean (it is assignment-shaped under control flow), so it
  # must not be produced in the first place.
  let resultLocal = resultLocalOf(pd.body, pSyms)
  if resultLocal != SymId(0):
    rename[resultLocal] = tmpSym

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
  var bnd = bindingsFor(pSyms, argCursors, pd.body, rename)
  bnd.dropDecl = resultLocal

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

# ---- Condition-splice: inline body straight into an `if`/`elif` guard ----

proc countSymUses(n: Cursor; sym: SymId): int =
  ## Count `Symbol` (use, not `SymbolDef`) occurrences of `sym` within the
  ## single subtree rooted at `n` (which may be a leaf token).
  result = 0
  if n.isSymbol:
    if n.symId == sym: result = 1
    return
  if not n.isTagLit:
    return
  var it = n
  it.into:
    while it.hasMore:
      case it.kind
      of Symbol:
        if it.symId == sym: inc result
        inc it
      of TagLit:
        result += countSymUses(it, sym)
        skip it
      else:
        inc it

proc effectiveReturnExpr(body: Cursor; outVal: var Cursor): bool =
  ## True when `body` computes a single value with no side effect other than
  ## producing it, in one of the shapes nimony emits for `result = X` /
  ## `return X` inline bodies:
  ##
  ##   (ret X)
  ##   (var :R T X) (ret R)
  ##   (var :R T .) (asgn R X) (ret R)
  ##
  ## (each optionally wrapped in length-1 `(stmts …)`/`(scope …)`). On success
  ## `outVal` is the cursor at `X`. The result var `R` must not appear inside
  ## `X`, so splicing `X` where `R` would have been read is sound.
  var b = body
  # Peel single-child stmts/scope wrappers to reach the real statement list.
  while b.isTagLit and b.stmtKind in {StmtsS, ScopeS}:
    var cnt = 0
    var only = default(Cursor)
    var inner = b
    inner.into:
      while inner.hasMore:
        inc cnt
        if cnt == 1: only = inner
        skip inner
    if cnt == 1:
      b = only
    else:
      break
  # Gather the (up to 3) statements of the reached list.
  var stmts: seq[Cursor] = @[]
  if b.isTagLit and b.stmtKind in {StmtsS, ScopeS}:
    var it = b
    it.into:
      while it.hasMore:
        stmts.add it
        skip it
        if stmts.len > 3: return false     # too many statements → not the idiom
  else:
    stmts.add b                            # a bare `(ret …)` etc.

  proc retSym(s: Cursor; outR: var SymId): bool =
    # `(ret R)` returning a plain symbol → R.
    if not s.isTagLit or s.stmtKind != RetS: return false
    let v = s.childCursor
    if v.isSymbol:
      outR = v.symId
      return true
    return false

  case stmts.len
  of 1:
    # (ret X) with X a value expression (not a bare `(ret sym)` handled below).
    let s = stmts[0]
    if s.isTagLit and s.stmtKind == RetS:
      let v = s.childCursor
      if not v.isDotToken and not v.isSymbol:
        outVal = v
        return true
    return false
  of 2:
    # (var :R T X) (ret R)
    var R = SymId(0)
    if not retSym(stmts[1], R): return false
    let vdecl = stmts[0]
    if not vdecl.isTagLit or vdecl.stmtKind != VarS: return false
    var p = vdecl
    inc p                                  # past `var`
    if not p.isSymbolDef or p.symId != R: return false
    inc p                                  # past name
    skip p                                 # past pragmas
    skip p                                 # past type
    if p.isDotToken: return false    # no initializer here
    if countSymUses(p, R) != 0: return false
    outVal = p
    return true
  of 3:
    # (var :R T .) (asgn R X) (ret R)
    var R = SymId(0)
    if not retSym(stmts[2], R): return false
    let vdecl = stmts[0]
    if not vdecl.isTagLit or vdecl.stmtKind != VarS: return false
    var p = vdecl
    inc p
    if not p.isSymbolDef or p.symId != R: return false
    inc p                                  # past name
    skip p                                 # past pragmas
    skip p                                 # past type
    # The result var must have NO initializer — otherwise `X` is not the sole
    # value of `R` and a side-effecting initializer would be dropped.
    if not p.isDotToken: return false
    let asgn = stmts[1]
    if not asgn.isTagLit or asgn.stmtKind != AsgnS: return false
    var a = asgn
    inc a                                  # past `asgn`
    if not a.isSymbol or a.symId != R: return false
    skip a                                 # past LHS (R)
    if countSymUses(a, R) != 0: return false
    outVal = a
    return true
  else:
    return false

proc trySpliceCond*(c: var InlinerCtx; dest: var TokenBuf; n: var Cursor;
                    calleeSym: var SymId): int =
  ## Fuse xelim's condition-temp lowering back into the guard when the call
  ## inlines to a single expression. Matches the *adjacent* pair
  ##
  ##   (var :tmp <pragmas> <type> (call f arg…))
  ##   (if (elif tmp BODY) …rest…)
  ##
  ## where `f`'s body is exactly `result = X`, every parameter is
  ## substitutable, and `tmp` is read *only* as that first `elif`'s
  ## condition — checked against the module-wide `symUses`, not just the
  ## adjacent `if`, because the definition is about to be dropped and any
  ## read further down the scope would be left dangling. It then emits
  ##
  ##   (if (elif X' BODY) …rest…)
  ##
  ## dropping the temp entirely, so arkham's `emitCond2` fuses the compare
  ## into the branch (`cmp; jcc`) instead of materialising a boolean and
  ## re-testing it. This is a purely local, single-use rewrite: correctness
  ## follows from `var t = X; if t:` ≡ `if X:` (the first `elif` condition is
  ## evaluated unconditionally, exactly where the temp was), so no purity or
  ## dataflow analysis is needed. Returns the number of top-level subtrees
  ## emitted (1: the rewritten `if`) or 0 (leaving `n`/`dest` untouched).
  if not n.isTagLit or n.stmtKind != VarS: return 0
  let entry = n

  # --- parse `(var :tmp <pragmas> <type> (call f arg…))` ---
  var probe = n
  inc probe                                # past `var` tag
  if not probe.isSymbolDef: return 0
  let tmpSym = probe.symId
  let tmpName = pool.syms[tmpSym]
  if not isLocalName(tmpName): return 0
  # The whole module must read `tmp` exactly once. The `countSymUses` check
  # below only looks inside the adjacent `if`, which is sound for a temp
  # xelim mints solely to hold that guard — but a plain user local
  # (`var negative = isNegative(x)`) matches the very same shape and may be
  # read further down the scope, and dropping its definition then leaves
  # those reads dangling.
  if c.symUses.getOrDefault(tmpSym, 0) != 1: return 0
  inc probe                                # past name
  skip probe                               # past pragmas
  skip probe                               # past type
  if not probe.isTagLit or probe.stmtKind != CallS: return 0
  let valueCursor = probe
  var callProbe = valueCursor
  inc callProbe                            # past `call` tag
  if not callProbe.isSymbol: return 0
  let cSym = callProbe.symId

  # --- peek the next sibling: must be `(if (elif tmp …) …)` guarding on tmp ---
  var nextCur = entry
  skip nextCur                             # past the whole var decl
  if not nextCur.isTagLit or nextCur.stmtKind != IfS: return 0
  let firstElif = nextCur.childCursor
  if not firstElif.isTagLit or firstElif.substructureKind != ElifU: return 0
  let condCur = firstElif.childCursor
  if not condCur.isSymbol or condCur.symId != tmpSym: return 0
  # `tmp` must be used *only* here — otherwise dropping its def is unsound.
  if countSymUses(nextCur, tmpSym) != 1: return 0

  # --- heavier inline eligibility checks (only after the shape matched) ---
  if cSym in c.inProgress: return 0
  if c.maxDepth > 0 and c.inProgress.len >= c.maxDepth: return 0
  if not shouldInlineCall(c, cSym, valueCursor): return 0
  var pcur = default(Cursor)
  if not lookupBody(c, cSym, pcur): return 0
  let pd = takeProcDecl(pcur)

  var pSyms: seq[SymId] = @[]
  var pTypes: seq[Cursor] = @[]
  collectParams(pd.params, pSyms, pTypes)

  var argScan = valueCursor
  var argCount = 0
  argScan.into:
    skip argScan                           # past callee sym
    while argScan.hasMore:
      skip argScan
      inc argCount
  if argCount != pSyms.len: return 0

  # Body must reduce to a single returned expression `result = X`.
  var retVal = default(Cursor)
  if not effectiveReturnExpr(pd.body, retVal): return 0

  # Bind params; require *every* param substitutable so the whole inline
  # collapses to `X` with no `(var :p = arg)` prologue to emit before the if.
  var rename = initTable[SymId, SymId]()
  for s in pSyms:
    rename[s] = c.freshSym(s)
  var argCursors: seq[Cursor] = @[]
  var ac = callProbe
  inc ac                                   # past callee sym
  for i in 0 ..< pSyms.len:
    argCursors.add ac
    skip ac
  let bnd = bindingsFor(pSyms, argCursors, pd.body, rename)
  for i in 0 ..< pSyms.len:
    if not bnd.subst.hasKey(pSyms[i]): return 0

  # --- emit the rewritten `if`, splicing X into the first elif's condition ---
  var ifOpener = nextCur
  dest.addParLe(ifOpener.cursorTagId, ifOpener.info)  # copy `(if` opener
  ifOpener.into:
    var elifn = ifOpener
    dest.addParLe(elifn.cursorTagId, elifn.info)   # copy `(elif` opener
    elifn.into:
      emitRenamed(dest, retVal, bnd)       # X' replaces the tmp condition
      skip elifn                           # drop the original tmp condition
      while elifn.hasMore:
        dest.takeTree elifn                # elif body verbatim
    dest.addParRi()                        # close (elif …)
    skip ifOpener                          # past the elif we just consumed
    while ifOpener.hasMore:
      dest.takeTree ifOpener               # else / further elifs verbatim
  dest.addParRi()                          # close (if …)

  # Advance past BOTH the var decl and the if we folded into it.
  n = entry
  skip n                                   # past var
  skip n                                   # past if
  calleeSym = cSym
  result = 1

# ---- Same-module inliner pass (called from hexer.nim) ----

proc tallySpliced(c: var InlinerCtx; spliced: var TokenBuf; count: int) =
  ## A splice brings in freshly-minted locals that were not in the module
  ## when `collectProcBodies` counted. Fold their uses into `symUses` before
  ## the spliced code is re-walked, so a `trySpliceCond` firing *inside* it
  ## reads a real count rather than an absent (zero) entry. Module-level
  ## syms get counted twice over; only locals are ever splice candidates,
  ## so an inflated count for the rest costs nothing.
  var scan = beginRead(spliced)
  for _ in 0 ..< count:
    tallySymUses(scan, c.symUses)
    skip scan
  endRead(scan)

proc trIntra*(c: var InlinerCtx; dest: var TokenBuf; n: var Cursor) =
  ## Walks `n` and splices `.inline` calls in-place. Mirrors `dce2.tr`'s
  ## splice paths but without liveness / generic-instance resolution.
  ##
  ## Cross-module behaviour: callees in another module are picked up
  ## automatically when the context's `xnifDir` is non-empty —
  ## `lookupInlineInfo` / `lookupBody` then lazy-load the foreign `.c.nif`.
  ## With `xnifDir == ""` foreign callees are naturally skipped (their
  ## `InlineInfo` defaults to threshold 100, so `shouldInlineCall` declines).
  case n.kind
  of TagLit:
    let sk = n.stmtKind
    case sk
    of StmtsS, ScopeS:
      # Bound the cursor to this scope with `into` so the child loop stops at
      # the (real or virtual) closing `)`; under `-d:virtualParRi` a sealed
      # scope has no `ParRi` token, so a raw `while n.hasMore` over an
      # unbounded `rem` would walk into siblings. Emit a fresh closer with
      # `addParRi` (the source `)` may be elided).
      dest.addParLe(n.cursorTagId, n.info)
      into n:
        while n.hasMore:
          if n.isTagLit and n.stmtKind == CallS:
            var probe = n
            inc probe
            let calleeSym =
              if probe.isSymbol: probe.symId else: SymId(0)
            var spliced = createTokenBuf(32)
            let nEmitted = trySplice(c, spliced, n)
            if nEmitted > 0:
              tallySpliced(c, spliced, nEmitted)
              if calleeSym != SymId(0):
                c.inProgress.incl calleeSym
              var inner = beginRead(spliced)
              for _ in 0 ..< nEmitted:
                trIntra(c, dest, inner)
              endRead(inner)
              if calleeSym != SymId(0):
                c.inProgress.excl calleeSym
              continue
          if n.isTagLit and n.stmtKind == VarS:
            # `(var :tmp T (call …))` immediately guarding an `if` — fold the
            # inlined condition straight into the guard so no boolean temp is
            # materialised (see `trySpliceCond`). Peeks the following sibling.
            var condCallee = SymId(0)
            var spliced = createTokenBuf(32)
            let nEmitted = trySpliceCond(c, spliced, n, condCallee)
            if nEmitted > 0:
              tallySpliced(c, spliced, nEmitted)
              c.inProgress.incl condCallee
              var inner = beginRead(spliced)
              for _ in 0 ..< nEmitted:
                trIntra(c, dest, inner)
              endRead(inner)
              c.inProgress.excl condCallee
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
        if probe.isSymbolDef:
          inc probe          # name
          skip probe         # pragmas
          skip probe         # type
          if probe.isTagLit and probe.stmtKind == CallS:
            inc probe
            if probe.isSymbol:
              calleeSym = probe.symId
        if calleeSym != SymId(0):
          var spliced = createTokenBuf(32)
          let nEmitted = trySpliceVarInit(c, spliced, n)
          if nEmitted > 0:
            tallySpliced(c, spliced, nEmitted)
            c.inProgress.incl calleeSym
            var inner = beginRead(spliced)
            for _ in 0 ..< nEmitted:
              trIntra(c, dest, inner)
            endRead(inner)
            c.inProgress.excl calleeSym
            return
      dest.addParLe(n.cursorTagId, n.info)
      into n:
        while n.hasMore:
          trIntra(c, dest, n)
      dest.addParRi()
    else:
      dest.addParLe(n.cursorTagId, n.info)
      into n:
        while n.hasMore:
          trIntra(c, dest, n)
      dest.addParRi()
  else:
    dest.takeTree n

proc emitPragmasWithInlineInfo(dest: var TokenBuf; pragmas: Cursor; info: InlineInfo) =
  var p = pragmas
  if not p.isTagLit:
    dest.addSubtree p
    return

  dest.addParLe(p.cursorTagId, p.info)
  p.into:
    while p.hasMore:
      if p.isTagLit and p.pragmaKind == InlineP:
        dest.addParLe(p.cursorTagId, p.info)
        p.into:
          dest.addIntLit info.threshold, p.endInfo
          for w in info.weights:
            dest.addIntLit w, p.endInfo
          while p.hasMore:
            skip p
        dest.addParRi()
      else:
        dest.takeTree p
  dest.addParRi()

proc annotateInlinePragmas(dest: var TokenBuf; n: var Cursor;
                           infos: Table[SymId, InlineInfo]) =
  case n.kind
  of TagLit:
    if n.stmtKind == ProcS:
      let tag = n.cursorTagId
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
      dest.addParLe(n.cursorTagId, n.info)
      n.into:
        while n.hasMore:
          annotateInlinePragmas(dest, n, infos)
      dest.addParRi()
  else:
    dest.takeTree n

proc annotateInlinePragmas(buf: var TokenBuf; infos: Table[SymId, InlineInfo]) =
  if infos.len == 0: return
  var n = beginRead(buf)
  var dest = createTokenBuf(buf.len)
  annotateInlinePragmas(dest, n, infos)
  buf = ensureMove(dest)

proc intraModuleInline*(moduleSuffix: string; buf: var TokenBuf) =
  ## Same-module inliner pass run as the last step of hexer's `expand`,
  ## so the `.x.nif` we publish has each `.inline` proc body already
  ## cascaded against its same-module callees. Cross-module splicing
  ## (dce2) then needs at most one level of work per call site, since
  ## the foreign body it pulls is already flat. This eliminates the
  ## per-importer redundancy where the same cascade was re-walked once
  ## per dceEmit process.
  ##
  ## Also writes each `.inline` proc's computed `InlineInfo` into its own
  ## pragma (`(inline THRESHOLD w…)`). That annotation is what importers read
  ## back — see `readInlinePragma` — so no sidecar has to carry it.
  let ma = analyzeModule(buf)
  annotateInlinePragmas(buf, ma.inlineInfo)
