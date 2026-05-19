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
import ".." / nifc / [nifc_model]
import dce1

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
  if n.stmtKind != StmtsS:
    endRead(buf)
    return
  inc n
  while n.kind != ParRi:
    if n.kind == ParLe and n.stmtKind == ProcS:
      let off = cursorToPosition(buf, n)
      var p = n
      inc p
      if p.kind == SymbolDef:
        bodies[p.symId] = off
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

proc computeArgScores(argsStart: Cursor): seq[int] =
  ## Walks the args of a `(call f arg…)` (starting *past* the callee
  ## sym) and builds the per-arg score vector for `shouldInline`.
  result = @[]
  var a = argsStart
  while a.kind != ParRi:
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
  result = c.infos[][modul].inlineInfo.getOrDefault(calleeSym, DefaultInlineInfo)

proc shouldInlineCall(c: var InlinerCtx; calleeSym: SymId;
                      argsStart: Cursor): bool =
  ## Decides whether to splice a call to `calleeSym` at this call site.
  ## `.inline` (threshold 0) always wins; `.noinline` (threshold
  ## ≥ 10000) always loses; everything else goes through the per-call
  ## weighted-score heuristic against the proc's `InlineInfo`.
  let info = lookupInlineInfo(c, calleeSym)
  if info.threshold == 0: return true
  if info.threshold >= 10000: return false
  let scores = computeArgScores(argsStart)
  result = shouldInline(info, scores)

# Old name kept for the splice's eligibility precheck — semantically
# "is this proc ever considered inlinable?", used by the cycle guard
# and the dce_inliner's body lookup. The per-callsite decision goes
# through `shouldInlineCall` instead.
proc isInlinable(c: var InlinerCtx; calleeSym: SymId): bool =
  let info = lookupInlineInfo(c, calleeSym)
  result = info.threshold == 0

proc collectParams(params: Cursor; outSyms: var seq[SymId];

                   outTypes: var seq[Cursor]) =
  outSyms.setLen 0
  outTypes.setLen 0
  if params.kind != ParLe: return
  var p = params
  inc p
  while p.kind != ParRi:
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

proc emitRenamed(dest: var TokenBuf; body: var Cursor;
                 rename: Table[SymId, SymId]) =
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
    if rename.hasKey(body.symId):
      dest.addSymDef rename.getOrQuit(body.symId), body.info
    else:
      dest.add body
    inc body
  of Symbol:
    if rename.hasKey(body.symId):
      dest.addSymUse rename.getOrQuit(body.symId), body.info
    else:
      dest.add body
    inc body
  of ParLe:
    if body.substructureKind == KvU:
      # `(kv field value [depth])` — field name slot is verbatim.
      dest.add body
      inc body                              # past `kv` tag
      if body.kind != ParRi:
        dest.add body                       # field name — no rename
        inc body
      while body.kind != ParRi:
        emitRenamed(dest, body, rename)
      dest.add body
      inc body
      return
    if body.exprKind == DotC:
      # `(dot obj field [depth])` — field slot (the 2nd child) is
      # verbatim; obj and the optional depth are renameable.
      dest.add body
      inc body                              # past `dot` tag
      if body.kind != ParRi:
        emitRenamed(dest, body, rename)    # obj
      if body.kind != ParRi:
        dest.add body                       # field — no rename
        inc body
      while body.kind != ParRi:
        emitRenamed(dest, body, rename)    # depth, etc.
      dest.add body
      inc body
      return
    dest.add body
    inc body
    while body.kind != ParRi:
      emitRenamed(dest, body, rename)
    dest.add body
    inc body
  of ParRi:
    discard
  else:
    dest.add body
    inc body

proc emitRenamedWithRet(dest: var TokenBuf; body: var Cursor;
                        rename: Table[SymId, SymId];
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
    if rename.hasKey(body.symId):
      dest.addSymDef rename.getOrQuit(body.symId), body.info
    else:
      dest.add body
    inc body
  of Symbol:
    if rename.hasKey(body.symId):
      dest.addSymUse rename.getOrQuit(body.symId), body.info
    else:
      dest.add body
    inc body
  of ParLe:
    if body.stmtKind == RetS:
      let info = body.info
      inc body                              # past `ret` tag
      if body.kind != DotToken and targetSym != SymId(0):
        dest.addParLe TagId(AsgnS), info
        dest.addSymUse targetSym, info
        emitRenamed(dest, body, rename)    # the returned expression
        dest.addParRi()
      else:
        skip body                           # discard the value
      dest.addParLe TagId(JmpS), info
      dest.addSymUse returnLabel, info
      dest.addParRi()
      inc body                              # past `(ret …)`'s closing ParRi
      return
    if body.substructureKind == KvU:
      # See `emitRenamed`: field-name slot of `(kv …)` is verbatim to
      # avoid collisions with body-locals that share the field name.
      # Inner subtrees still get the ret-rewrite treatment in case the
      # value side hides a `(ret …)`.
      dest.add body
      inc body
      if body.kind != ParRi:
        dest.add body                       # field name — verbatim
        inc body
      while body.kind != ParRi:
        emitRenamedWithRet(dest, body, rename, targetSym, returnLabel)
      dest.add body
      inc body
      return
    if body.exprKind == DotC:
      dest.add body
      inc body
      if body.kind != ParRi:
        emitRenamedWithRet(dest, body, rename, targetSym, returnLabel)
      if body.kind != ParRi:
        dest.add body                       # field — verbatim
        inc body
      while body.kind != ParRi:
        emitRenamedWithRet(dest, body, rename, targetSym, returnLabel)
      dest.add body
      inc body
      return
    dest.add body
    inc body
    while body.kind != ParRi:
      emitRenamedWithRet(dest, body, rename, targetSym, returnLabel)
    dest.add body
    inc body
  of ParRi:
    discard
  else:
    dest.add body
    inc body

proc emitBody(c: var InlinerCtx; dest: var TokenBuf; body: var Cursor;
              rename: Table[SymId, SymId]; targetSym: SymId) =
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
    emitRenamed(dest, body, rename)
    return
  let info = body.info
  inc c.counter
  let returnLabel = pool.syms.getOrIncl(
    "returnLabel.0" & c.counterPrefix & $c.counter)
  dest.addParLe TagId(StmtsS), info
  inc body
  while body.kind != ParRi:
    emitRenamedWithRet(dest, body, rename, targetSym, returnLabel)
  dest.addParLe TagId(LabS), info
  dest.addSymDef returnLabel, info
  dest.addParRi()
  dest.addParRi()                           # close (stmts …)
  inc body                                  # past body's ParRi

proc seedRenameFromBody(c: var InlinerCtx; body: Cursor;
                       rename: var Table[SymId, SymId]) =
  ## Walk every token in the body and mint a fresh sym for each
  ## SymbolDef found, so local var declarations don't collide between
  ## inline copies in the same scope.
  if body.kind != ParLe: return
  var n = body
  var nesting = 0
  while true:
    case n.kind
    of ParLe:
      inc nesting
      inc n
    of ParRi:
      dec nesting
      inc n
      if nesting == 0: return
    of SymbolDef:
      if not rename.hasKey(n.symId):
        rename[n.symId] = c.freshSym(n.symId)
      inc n
    of EofToken:
      return
    else:
      inc n

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
  var argsStart = probe
  inc argsStart                            # past callee sym, at first arg
  if not shouldInlineCall(c, calleeSym, argsStart): return 0
  var pcur = default(Cursor)
  if not lookupBody(c, calleeSym, pcur): return 0
  let pd = takeProcDecl(pcur)

  var pSyms: seq[SymId] = @[]
  var pTypes: seq[Cursor] = @[]
  collectParams(pd.params, pSyms, pTypes)

  # Arity check against the call's actual arguments.
  var argScan = probe
  inc argScan                              # past callee sym
  var argCount = 0
  while argScan.kind != ParRi:
    skip argScan
    inc argCount
  if argCount != pSyms.len: return 0

  # All checks passed — build the rename table and emit unconditionally.
  var rename = initTable[SymId, SymId]()
  for s in pSyms:
    rename[s] = c.freshSym(s)
  seedRenameFromBody(c, pd.body, rename)

  let info = entry.info
  dest.addParLe TagId(ScopeS), info

  # Param bindings: NIFC `(var :p_fresh <pragmas> <type> <value>)`.
  var arg = probe
  inc arg                                  # past callee sym
  for i in 0 ..< pSyms.len:
    dest.addParLe TagId(VarS), info
    dest.addSymDef rename.getOrQuit(pSyms[i]), info
    dest.addDotToken()                     # pragmas
    var t = pTypes[i]
    dest.takeTree t                        # parameter type
    dest.takeTree arg                      # initializer
    dest.addParRi()                        # close (var …)

  # Splice the body. Void splice: every (ret …) becomes (jmp returnLabel).
  var body = pd.body
  emitBody(c, dest, body, rename, SymId(0))

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
  var argsStart = callProbe
  inc argsStart                            # past callee sym, at first arg
  if not shouldInlineCall(c, calleeSym, argsStart): return 0
  var pcur = default(Cursor)
  if not lookupBody(c, calleeSym, pcur): return 0
  let pd = takeProcDecl(pcur)

  var pSyms: seq[SymId] = @[]
  var pTypes: seq[Cursor] = @[]
  collectParams(pd.params, pSyms, pTypes)

  var argScan = callProbe
  inc argScan                              # past callee sym
  var argCount = 0
  while argScan.kind != ParRi:
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
  dest.addParLe TagId(ScopeS), info

  var arg = callProbe
  inc arg                                  # past callee sym
  for i in 0 ..< pSyms.len:
    dest.addParLe TagId(VarS), info
    dest.addSymDef rename.getOrQuit(pSyms[i]), info
    dest.addDotToken()                     # pragmas
    var t = pTypes[i]
    dest.takeTree t
    dest.takeTree arg
    dest.addParRi()

  # Splice the body; every `(ret X)` becomes `(asgn tmpSym X) (jmp …)`,
  # with a matching `(lab …)` appended at the tail.
  var body = pd.body
  emitBody(c, dest, body, rename, tmpSym)

  dest.addParRi()                          # close (scope …)

  # Advance past the original var decl.
  n = entry
  skip n
  result = 2                               # `(var …)` + `(scope …)`

# ---- Same-module inliner pass (called from hexer.nim) ----

proc trIntra(c: var InlinerCtx; dest: var TokenBuf; n: var Cursor) =
  ## Walks `n` and splices same-module `.inline` calls in-place.
  ## Mirrors `dce2.tr`'s splice paths but without liveness / generic-
  ## instance resolution — those are dce2's job. Cross-module callees
  ## are naturally skipped: `c.infos` only holds this module, and
  ## `c.xnifDir == ""` keeps `loadForeignAnalysis` from pulling foreign
  ## sidecars in, so `lookupInlineInfo` returns `DefaultInlineInfo`
  ## (threshold 100) for them and `shouldInlineCall` declines.
  case n.kind
  of ParLe:
    let sk = n.stmtKind
    case sk
    of StmtsS, ScopeS:
      dest.takeToken n
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
      dest.takeToken n
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
      dest.takeToken n
      while n.hasMore:
        trIntra(c, dest, n)
      dest.takeToken n
    else:
      dest.takeToken n
      while n.hasMore:
        trIntra(c, dest, n)
      dest.takeToken n
  of ParRi:
    raiseAssert "ParRi should not be encountered here"
  else:
    dest.takeToken n

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
  # `xnifDir = ""` ⇒ `findForeignFile` returns "" ⇒ foreign analysis
  # never gets loaded ⇒ shouldInlineCall sees DefaultInlineInfo for
  # cross-module callees and declines them. So `trySplice` only fires
  # on same-module `.inline` procs even without a sameModuleOnly flag.
  var c = initInlinerCtx(moduleSuffix, addr buf, addr graphs,
                         counterPrefix = "h")
  collectProcBodies c
  var n = beginRead(buf)
  var dest = createTokenBuf(buf.len)
  trIntra(c, dest, n)
  endRead(buf)
  buf = ensureMove(dest)
