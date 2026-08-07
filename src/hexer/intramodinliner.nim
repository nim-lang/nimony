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
##   - single-return procs only (no `(ret …)` mid-body).
##
## The inlining POLICY is size-driven, not annotation-driven (see
## `computeInlineInfo`): a body of at most `InlineTinyBound` tokens is always
## spliced — that covers forwarders, accessors and hooks whether or not the
## author wrote `.inline` — a `.noinline` proc never is, and anything bigger
## goes through the per-call-site weighted-score heuristic (`shouldInline`)
## whose threshold grows with the body size, so a big body needs ever juicier
## arguments (literals feeding conditions) to be worth its bulk. The
## `.inline` annotation itself is deliberately IGNORED here: it keeps its
## emission meaning (body shipped to importers, `static inline` in C) but no
## longer forces the splice, so an ill-considered `.inline` on a fat proc
## cannot blow the program up anymore, and a proc nobody thought to annotate
## still inlines when it is trivially cheap.
##
## Two passes share this machinery, one per pipeline stage, and each stays in
## the file format of its stage:
##   - `intraModuleInline` runs inside hexer, pre-DCE, on the tree that becomes
##     this module's `.x.nif`. Its own module only (`xnifDir` unset).
##   - `runInterModuleInliner` (shoggoth) runs after DCE, on the `.c.nif`s, and
##     is the one that crosses module borders: `loadForeign` lazy-loads the
##     callee's `.c.nif`.
##
## Either way the decision is derived from the same file the body comes out
## of: `indexProcBodies` measures each proc right after the module is parsed,
## so what is scored is exactly what would be spliced (hexer's flattening of
## tiny bodies happens *before* the `.c.nif` is written — a proc that grows
## past the bound by having its own callees spliced into it is re-measured,
## and demoted, by every importer).
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
    size*: int                 ## body token count; what a splice would cost
  ModuleAnalysis = object
    ## Hexer-stage scratch: the threshold-0 procs `analyzeModule` found, so
    ## `intraModuleInline` knows which bodies to flatten. Importers never see
    ## this type — they re-derive the same information from the `.c.nif` they
    ## parse anyway (`indexProcBodies`).
    ## (Not exported: `dce1` has an unrelated `ModuleAnalysis` and both
    ## modules are imported together by `pipeline`.)
    inlineInfo: Table[SymId, InlineInfo]

const
  DefaultInlineInfo* = InlineInfo(threshold: 100, weights: @[],
                                  guardThreshold: 100, guards: @[])
  InlineTinyBound* = 100
    ## Bodies of at most this many tokens are spliced unconditionally: at that
    ## size the body is on the order of the call sequence it replaces (a
    ## forwarder, an accessor with its assert, a hook's nil-test-and-call), so
    ## inlining cannot lose. Measured evidence for the value: capping splices
    ## at 100 tokens on a full nimsem build shrank the optimized IR 6x and the
    ## binary 4x while the produced compiler ran slightly FASTER — beyond this
    ## size, inlining pays icache, not wins.
  InlineNeverBound* = 10000
    ## Thresholds at or above this mean "never" (`.noinline`).
  InlineWeightCap* = 150
    ## Ceiling for a single parameter's weight. The weight walk adds the use
    ## context's value per occurrence, so an uncapped weight grows with the
    ## body — and since the threshold also grows with the body (`size div 4`),
    ## the two cancel and ANY body whose params appear in conditions more than
    ## ~once per 100 tokens would inline at every call site. The benefit of
    ## substituting one argument does not scale with body size (folding a
    ## branch is worth the branch, not the whole proc), so the estimate must
    ## not either: with the cap, a body of size S needs on the order of
    ## S/(4*150) max-weight literal arguments to inline — big bodies need
    ## several genuinely decisive arguments, huge bodies effectively never
    ## qualify.

proc shouldInline*(info: InlineInfo; argScores: openArray[int]): bool =
  var sum = 0
  for i, score in argScores:
    if i < info.weights.len:
      sum += (info.weights[i] * score) div 100
  result = sum >= info.threshold

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

proc hasVarargsParam(params: Cursor): bool =
  ## A `(varargs)` parameter cannot be bound to a `(var …)` at a splice site
  ## (the type has no size), so such procs are never inlined.
  result = false
  if not params.isTagLit: return false
  var p = params
  p.into:
    while p.hasMore:
      if p.substructureKind == ParamU:
        var q = p
        inc q                       # into the param: at the name
        if q.isSymbolDef:
          inc q                     # past name
          skip q                    # past pragmas
          if q.typeKind == VarargsT: result = true
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

proc tokenCountAux(n: var Cursor): int =
  case n.kind
  of TagLit:
    result = 1
    n.into:
      while n.hasMore:
        result += tokenCountAux(n)
  else:
    result = 1
    inc n

proc tokenCount(n: Cursor): int =
  ## Tokens in the subtree rooted at `n` (closing parens not counted — they
  ## may be virtual anyway). A stable cost measure for the policy below.
  var c = n
  result = tokenCountAux(c)

proc computeInlineInfo*(procDecl: Cursor): InlineInfo =
  ## The whole inlining policy, derived from the proc decl itself:
  ##   - no body / `.noinline` → never (an `InlineNeverBound` threshold);
  ##   - body ≤ `InlineTinyBound` tokens → always (threshold 0);
  ##   - anything bigger → the weighted-score heuristic, with a threshold
  ##     that grows with the body size (`max(100, size div 4)`), so only a
  ##     moderately-sized body with high-value arguments (literals feeding
  ##     conditions or index expressions) clears the bar.
  ## The `.inline` annotation is NOT consulted — see the module docs.
  result = DefaultInlineInfo
  var p = procDecl
  let pd = takeProcDecl(p)
  if not pd.body.isTagLit:
    result.threshold = InlineNeverBound     # extern/no body: nothing to splice
    return
  if pd.pragmas.isTagLit:
    var pr = pd.pragmas
    pr.into:                            # scan all pragmas (no early break: the
      while pr.hasMore:                 # `into` epilogue needs the scope drained)
        if pr.isTagLit and pr.pragmaKind in {NoinlineP, ImportcP, ImportcppP,
                                             AssemblerP}:
          # importc: the decl's `(stmts .)` "body" is a PLACEHOLDER — the real
          # code is external. Splicing it deletes the call (measured: memfiles
          # inlined posix `open`'s empty shell and never called open(2)).
          # assembler: the body is machine-level (register-pinned locals, 1:1
          # instructions) — meaningless spliced into ordinary code, and the
          # splice strands `{.register.}` pragmas where no backend accepts
          # them (measured: tcbackend's firstBit spliced + DCE'd, so the C
          # backend saw a bare register-pinned local instead of rejecting the
          # assembler proc).
          result.threshold = InlineNeverBound
        skip pr
  if result.threshold >= InlineNeverBound:
    return
  if hasVarargsParam(pd.params):
    result.threshold = InlineNeverBound
    return

  let params = collectParamSyms(pd.params)
  result.weights = newSeq[int](params.len)
  let size = tokenCount(pd.body)
  result.size = size
  if size <= InlineTinyBound:
    result.threshold = 0
  else:
    result.threshold = max(DefaultInlineInfo.threshold, size div 4)
    var lookup = initTable[SymId, int]()
    for i, s in params:
      lookup[s] = i
    if lookup.len > 0:
      var body = pd.body
      walkInlineWeights(body, lookup, result.weights, 0)
      for w in mitems(result.weights):
        w = min(w, InlineWeightCap)

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
    src: ptr TokenBuf                       # the module's parsed buffer
    xnifDir: string                         # directory holding the `.c.nif`s
    maxDepth*: int                          # 0 = unlimited; cross-module mode sets a cap
    growthLeft*: int
      # Remaining tokens the proc currently being walked may gain from
      # splices. Set per top-level `(proc …)` from `growthBudget` (a caller
      # may roughly double), decremented by each committed splice — including
      # the splices `trIntra` performs while re-walking spliced content, so a
      # depth-N cascade draws from the same pot. This is the hard backstop
      # that keeps program growth linear no matter what the per-call
      # heuristic thinks: without it a chain of individually-approved
      # splices compounds multiplicatively (measured: 8.4x IR blowup and
      # multi-GB hexer RSS on nimsem).
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
                     xnifDir = ""; maxDepth = 0;
                     counterPrefix = "i"): InlinerCtx =
  ## `counterPrefix` is woven into fresh local sym names (`base.0i<n>`,
  ## `returnLabel.0i<n>`). The hexer same-module pass uses `"h"` and
  ## dce2's cross-module pass uses `"d"` so freshly-minted dce2 syms
  ## can never collide with hexer-minted syms that survive in the
  ## `.x.nif` body dce2 is rewriting.
  InlinerCtx(moduleSuffix: moduleSuffix, src: src,
             bodies: initTable[SymId, int](),
             ownInfo: initTable[SymId, InlineInfo](),
             xnifDir: xnifDir,
             maxDepth: maxDepth,
             growthLeft: high(int),
             counterPrefix: counterPrefix,
             foreign: initTable[string, ref ForeignModule](),
             inProgress: initHashSet[SymId]())

proc growthBudget*(bodySize: int): int =
  ## How many spliced tokens a proc of `bodySize` may absorb: it may about
  ## double, and small procs get a floor so a forwarder can still swallow a
  ## couple of tiny callees.
  max(1000, bodySize)

proc indexProcBodies(buf: var TokenBuf; bodies: var Table[SymId, int];
                     infos: var Table[SymId, InlineInfo]) =
  ## Walks the top-level `(stmts …)` and records `(proc :sym …)` decls
  ## by sym → byte offset into `buf`, along with each proc's `InlineInfo`,
  ## computed right here from the body we are indexing (`computeInlineInfo`
  ## walks it once — a linear pass over a buffer we just parsed anyway). No
  ## pragma transport is involved, so own-module and foreign bodies go
  ## through the identical policy, and the size that is scored is the size
  ## of the exact body a splice would copy.
  var n = beginRead(buf)
  if n.stmtKind == StmtsS:
    n.into:
      while n.hasMore:
        if n.isTagLit and n.stmtKind == ProcS:
          let nameCur = n.childCursor            # the (proc :sym …) name child
          if nameCur.isSymbolDef:
            bodies[nameCur.symId] = cursorToPosition(buf, n)
            let info = computeInlineInfo(n)
            if info.threshold == 0 or
               (info.threshold < InlineNeverBound and info.weights.len > 0):
              infos[nameCur.symId] = info
        skip n

proc collectProcBodies*(c: var InlinerCtx) =
  indexProcBodies(c.src[], c.bodies, c.ownInfo)

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
  ##
  ## The `.c.nif`, because only this pass runs that late: it is post-DCE, so
  ## its generic instances already name the module that won the merge and a
  ## body copies into any other module unchanged. The `.x.nif` still names the
  ## callee's own module for instances the merge later moves elsewhere; that
  ## is the intra-module pass's world, not this one's.
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
  # No further vetting here: the only bodies that reach this point already
  # passed `shouldInlineCall`, i.e. the size-driven policy in
  # `computeInlineInfo` (tiny → always, big → scored, `.noinline` → never).
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

proc argContainsConstructor(callNode: Cursor): bool =
  ## `(oconstr/aconstr …)` anywhere in an argument. The C backend renders an
  ## address-taken aggregate constructor as a block-scope compound literal;
  ## a splice wraps its param bindings in a `(scope …)` — a C block — cutting
  ## that literal's lifetime short whenever its address escapes the splice
  ## (measured: `static Shape[N]` params — the openArray built over
  ## `&(Shape){…}.bounds` read dead stack after the scope closed). Until the
  ## splicer hoists such temporaries out of its scope, decline the site.
  proc walk(n: var Cursor): bool =
    case n.kind
    of TagLit:
      if n.exprKind in {OconstrC, AconstrC}:
        skip n
        return true
      result = false
      n.into:
        while n.hasMore:
          if walk(n): result = true
    else:
      result = false
      inc n
  var a = callNode
  result = false
  a.into:
    skip a                                # past the callee sym
    while a.hasMore:
      if walk(a): result = true

proc shouldInlineCall(c: var InlinerCtx; calleeSym: SymId;
                      callNode: Cursor): bool =
  ## Decides whether to splice a call to `calleeSym` at this call site.
  ## Tiny bodies (threshold 0) always win; `.noinline` / bodiless procs
  ## (threshold ≥ `InlineNeverBound`, or no stored info at all) always lose;
  ## everything else goes through the per-call weighted-score heuristic
  ## against the proc's `InlineInfo`.
  let info = lookupInlineInfo(c, calleeSym)
  if info.threshold >= InlineNeverBound: return false
  if info.size > c.growthLeft: return false   # caller's growth budget is spent
  if argContainsConstructor(callNode): return false
  if info.threshold == 0: return true
  let scores = computeArgScores(callNode)
  result = shouldInline(info, scores)

proc chargeSplice(c: var InlinerCtx; calleeSym: SymId) =
  ## Book the committed splice against the current caller's growth budget.
  let size = lookupInlineInfo(c, calleeSym).size
  c.growthLeft = max(0, c.growthLeft - max(size, 1))

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

when defined(inlinerStats):
  import std / [algorithm, syncio]
  var inlinerStats*: Table[string, tuple[count, tokens: int]]

  proc recordSplice(calleeSym: SymId; tokens: int) =
    let nm = pool.syms[calleeSym]
    var e = inlinerStats.getOrDefault(nm)
    inc e.count
    e.tokens += tokens
    inlinerStats[nm] = e

  proc dumpInlinerStats*(label: string) =
    var rows: seq[(int, int, string)] = @[]
    for k, v in inlinerStats:
      rows.add (v.tokens, v.count, k)
    rows.sort(SortOrder.Descending)
    stderr.writeLine "--- inliner stats " & label & " ---"
    for (t, cnt, k) in rows:
      stderr.writeLine $t & "\t" & $cnt & "\t" & k

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
  chargeSplice c, calleeSym
  when defined(inlinerStats): recordSplice(calleeSym, dest.len)
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
  chargeSplice c, calleeSym
  when defined(inlinerStats): recordSplice(calleeSym, dest.len)
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
  ## substitutable, and `tmp` is read as that first `elif`'s condition and
  ## nowhere else in its scope. It then emits
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
  if not isLocalName(pool.syms[tmpSym]): return 0
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
  # The definition is about to disappear, so `tmp` must be read here and
  # nowhere else. A local's reads live between its declaration and the end of
  # its scope, and `n` is bounded to that scope (`trIntra` enters `stmts` /
  # `scope` with `into`), so "the `if` plus the siblings that follow it" is
  # the whole live range — no dataflow needed, and no guess from the name
  # about who minted the local either.
  if countSymUses(nextCur, tmpSym) != 1: return 0
  var rest = nextCur
  skip rest                                # past the whole `if`
  while rest.hasMore:
    if countSymUses(rest, tmpSym) != 0: return 0
    skip rest

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
  chargeSplice c, cSym
  calleeSym = cSym
  result = 1

# ---- Splice-time branch pruning ----

type
  CondVal = enum
    condUnknown, condFalse, condTrue

proc negated(v: CondVal): CondVal =
  case v
  of condTrue: condFalse
  of condFalse: condTrue
  of condUnknown: condUnknown

proc litSame(a, b: Cursor): CondVal =
  ## Literal identity over the operand kinds `isSubstitutableArg` splices;
  ## anything else — including mixed literal kinds — stays `condUnknown`.
  if a.kind == IntLit and b.kind == IntLit:
    (if a.intVal == b.intVal: condTrue else: condFalse)
  elif a.kind == UIntLit and b.kind == UIntLit:
    (if a.uintVal == b.uintVal: condTrue else: condFalse)
  elif a.kind == CharLit and b.kind == CharLit:
    (if a.charLit == b.charLit: condTrue else: condFalse)
  elif a.isTagLit and b.isTagLit and
       a.exprKind in {NilC, TrueC, FalseC} and
       b.exprKind in {NilC, TrueC, FalseC}:
    (if a.exprKind == b.exprKind: condTrue else: condFalse)
  else:
    condUnknown

proc condVal(n: Cursor): CondVal =
  ## What a guard evaluates to once argument substitution made it literal:
  ## `(neq (nil) (nil))` from a spliced `if c != nil` with `c := nil`, or the
  ## `(not (eq …))` a nested `!=` forwarder splice leaves behind. `and`/`or`
  ## fold only when BOTH operands decide, so no operand whose evaluation the
  ## fold would discard is ever left unjudged.
  result = condUnknown
  if not n.isTagLit: return
  case n.exprKind
  of TrueC: result = condTrue
  of FalseC: result = condFalse
  of NotC:
    let arg = n.childCursor
    if arg.hasMore:
      result = negated(condVal(arg))
  of EqC, NeqC:
    let a = n.childCursor
    if a.hasMore:
      var b = a
      skip b
      if b.hasMore:
        let same = litSame(a, b)
        result = (if n.exprKind == EqC: same else: negated(same))
  of AndC, OrC:
    let a = n.childCursor
    if a.hasMore:
      var b = a
      skip b
      if b.hasMore:
        let l = condVal(a)
        let r = condVal(b)
        if l != condUnknown and r != condUnknown:
          if n.exprKind == AndC:
            result = (if l == condTrue and r == condTrue: condTrue else: condFalse)
          else:
            result = (if l == condTrue or r == condTrue: condTrue else: condFalse)
  else: discard

type
  PruneCtx = object
    symUses: Table[SymId, int]  ## splice-wide Symbol use counts, for label
                                ## pinning; left empty when the splice holds
                                ## no `(lab …)` at all (the common case)

proc hasLabelDef(n: Cursor): bool =
  ## Any `(lab …)` in the subtree — the trigger for building the pinning
  ## context at all.
  if not n.isTagLit: return false
  if n.stmtKind == LabS: return true
  result = false
  var it = n.childCursor
  while it.hasMore:
    if hasLabelDef(it): return true
    skip it

proc collectSymUses(c: var Cursor; uses: var Table[SymId, int]) =
  case c.kind
  of Symbol:
    uses.mgetOrPut(c.symId, 0) += 1
    inc c
  of TagLit:
    c.into:
      while c.hasMore:
        collectSymUses(c, uses)
  else:
    inc c

proc collectBranchLabels(c: var Cursor; localUses: var Table[SymId, int];
                         defs: var seq[SymId]) =
  case c.kind
  of Symbol:
    localUses.mgetOrPut(c.symId, 0) += 1
    inc c
  of TagLit:
    if c.stmtKind == LabS:
      var lc = c
      inc lc                              # into the lab: at the symbol def
      if lc.kind == SymbolDef: defs.add lc.symId
    c.into:
      while c.hasMore:
        collectBranchLabels(c, localUses, defs)
  else:
    inc c

proc hasAnyDef(n: Cursor): bool =
  ## Any SymbolDef in the subtree: a `(lab :L)` someone may jump to, or a
  ## `(var :v …)` declaration later reachable code may reference. Either
  ## makes a dead statement unsafe to drop.
  case n.kind
  of SymbolDef:
    result = true
  of TagLit:
    result = false
    var it = n.childCursor
    while it.hasMore:
      if hasAnyDef(it): return true
      skip it
  else:
    result = false

proc branchPinned(px: PruneCtx; branch: Cursor): bool =
  ## Does the branch define a `(lab :name)` some OUTSIDE code jumps to? Such
  ## a branch is reachable however its guard folds: hexer's try/except
  ## lowering parks the handler in an `(elif (false) (stmts (lab :`exlab.N)
  ## …))` entered only via `(jmp …)` from the try body, so "guard is (false)"
  ## does NOT mean "dead" for it. A label whose every use sits INSIDE the
  ## branch (this inliner's own returnLabel: `(jmp L)` + trailing `(lab :L)`
  ## in the same spliced body) pins nothing — the branch takes the label and
  ## its jumps with it. Compares the branch's own use counts against the
  ## splice-wide ones collected up front.
  var localUses = initTable[SymId, int]()
  var defs: seq[SymId] = @[]
  var c = branch
  collectBranchLabels(c, localUses, defs)
  for L in defs:
    if px.symUses.getOrDefault(L, 0) > localUses.getOrDefault(L, 0):
      return true                         # someone outside jumps in
  false

proc emitPruned(px: PruneCtx; dest: var TokenBuf; n: var Cursor) =
  ## Copy one subtree, deleting every `(elif …)` arm whose guard `condVal`
  ## decided. This is a CORRECTNESS duty, not an optimization: the false arm
  ## of a spliced body may no longer type-check at all — `if c != nil: …c.f…`
  ## inlined with `c := nil` keeps a `(deref (nil))` there — and a typed
  ## backend (arkham) must never see it, so the splice that manufactured the
  ## constant guard deletes the arm too. An `(elif (true) …)` arm demotes to
  ## the `if`'s final `(else …)` (the arms after it can never run); an `if`
  ## with no live arm left contributes its `else` body, or nothing. A pinned
  ## branch (see `branchPinned`) bails the whole `if` out to a verbatim copy.
  case n.kind
  of TagLit:
    if n.stmtKind == IfS:
      # Peek pass over the arms: what survives? The cursors index into the
      # buffer `n` reads, which outlives the re-emit below.
      var kept: seq[Cursor] = @[]         # elifs with undecided guards
      var taken = default(Cursor)         # first `(true)` elif, or the else
      var takenIsElif = false
      var haveTaken = false
      var dropped = false                 # anything decided at all?
      var bailout = false                 # a to-be-dropped branch is pinned
      var probe = n
      probe.into:
        while probe.hasMore:
          let sk = probe.substructureKind
          if haveTaken:
            # Dead branch after a taken one — droppable only when no outside
            # code jumps into it.
            if px.branchPinned(probe): bailout = true
            dropped = true
          elif sk == ElifU:
            case condVal(probe.childCursor)
            of condTrue:
              taken = probe; takenIsElif = true; haveTaken = true; dropped = true
            of condFalse:
              if px.branchPinned(probe): bailout = true
              dropped = true
            of condUnknown:
              kept.add probe
          elif sk == ElseU:
            taken = probe; takenIsElif = false; haveTaken = true
          else:
            kept.add probe                # unexpected shape: keep verbatim
          skip probe
      if bailout or not dropped:
        # Nothing decided at this level: keep the `if`, but still recurse
        # into the branch bodies (they may contain prunable ifs).
        dest.addParLe(n.cursorTagId, n.info)
        n.into:
          while n.hasMore:
            emitPruned(px, dest, n)
        dest.addParRi()
        return
      if kept.len == 0:
        # No undecided elifs before the taken branch: the whole `if`
        # collapses to the taken branch's body (or to nothing).
        if haveTaken:
          var b = taken
          b.into:
            if takenIsElif and b.hasMore: skip b    # past the guard
            while b.hasMore:
              emitPruned(px, dest, b)
        skip n
        return
      # Some undecided elifs survive: rebuild the `if` from them, a taken
      # `(true)` elif demoted to the terminal `(else …)`.
      dest.addParLe(n.cursorTagId, n.info)
      for arm in kept:
        var a = arm
        dest.addParLe(a.cursorTagId, a.info)
        a.into:
          while a.hasMore:
            emitPruned(px, dest, a)
        dest.addParRi()
      if haveTaken:
        let btag = (if takenIsElif: TagId(ElseU) else: taken.cursorTagId)
        dest.addParLe(btag, taken.info)
        var b = taken
        b.into:
          if takenIsElif and b.hasMore: skip b      # past the guard
          while b.hasMore:
            emitPruned(px, dest, b)
        dest.addParRi()
      dest.addParRi()
      skip n
    elif n.stmtKind in {StmtsS, ScopeS}:
      # Drop UNREACHABLE statements: after an unconditional `(jmp …)`/`(ret …)`
      # nothing executes until the next `(lab …)`, so def-free statements in
      # between are dead. The value-splice epilogue produces exactly this —
      # a callee whose every path returns via `(asgn dest X) (jmp RL)` leaves
      # the trailing `dest = result` self-copy dead with `result` never
      # written — and a typed backend verifier rightly rejects the dead read.
      # A statement that defines anything is kept and ends the dead region
      # (something can jump into it and fall out of it).
      dest.addParLe(n.cursorTagId, n.info)
      var unreachable = false
      n.into:
        while n.hasMore:
          let sk = n.stmtKind
          if sk == LabS:
            unreachable = false
            dest.takeTree n
          elif unreachable and not hasAnyDef(n):
            skip n                          # dead: drop
          else:
            if unreachable: unreachable = false
            emitPruned(px, dest, n)
            if sk in {JmpS, RetS}: unreachable = true
      dest.addParRi()
    elif n.stmtKind == NoStmt and n.substructureKind == NoSub:
      # An expression subtree cannot contain statements, hence no `if` arms.
      dest.takeTree n
    else:
      dest.addParLe(n.cursorTagId, n.info)
      n.into:
        while n.hasMore:
          emitPruned(px, dest, n)
      dest.addParRi()
  else:
    dest.takeTree n

proc prunedInto(dest: var TokenBuf; expanded: var TokenBuf) =
  ## Emit every top-level subtree of `expanded` into `dest` with the decided
  ## `if` arms deleted (`emitPruned`). The label-pinning use counts are built
  ## only when the splice contains a `(lab …)` at all — the common splice has
  ## none and skips that walk.
  var px = PruneCtx(symUses: initTable[SymId, int]())
  var scan = beginRead(expanded)
  var labs = false
  while scan.hasMore:
    if hasLabelDef(scan): labs = true
    skip scan
  endRead(scan)
  if labs:
    var uc = beginRead(expanded)
    while uc.hasMore:
      collectSymUses(uc, px.symUses)
    endRead(uc)
  var pruner = beginRead(expanded)
  while pruner.hasMore:
    emitPruned(px, dest, pruner)
  endRead(pruner)

# ---- Same-module inliner pass (called from hexer.nim) ----

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
              if calleeSym != SymId(0):
                c.inProgress.incl calleeSym
              # Nested splices first, into a scratch buffer; THEN prune the
              # branches the substituted arguments decided — only after the
              # nested walk are inlined guards (`!=` forwarders) reduced to
              # the literal comparisons `condVal` can judge.
              var expanded = createTokenBuf(spliced.len)
              var inner = beginRead(spliced)
              for _ in 0 ..< nEmitted:
                trIntra(c, expanded, inner)
              endRead(inner)
              if calleeSym != SymId(0):
                c.inProgress.excl calleeSym
              prunedInto(dest, expanded)
              continue
          if n.isTagLit and n.stmtKind == VarS:
            # `(var :tmp T (call …))` immediately guarding an `if` — fold the
            # inlined condition straight into the guard so no boolean temp is
            # materialised (see `trySpliceCond`). Peeks the following sibling.
            var condCallee = SymId(0)
            var spliced = createTokenBuf(32)
            let nEmitted = trySpliceCond(c, spliced, n, condCallee)
            if nEmitted > 0:
              c.inProgress.incl condCallee
              var expanded = createTokenBuf(spliced.len)
              var inner = beginRead(spliced)
              for _ in 0 ..< nEmitted:
                trIntra(c, expanded, inner)
              endRead(inner)
              c.inProgress.excl condCallee
              prunedInto(dest, expanded)
              continue
          trIntra(c, dest, n)
      dest.addParRi()
    of VarS, GvarS, TvarS, ConstS, ProcS:
      # `(var :tmp <pragmas> <type> (call …))` is the bound form `xelim`
      # and the new nifcgen complex-init path emit; route it through the
      # var-init splice. Other locals copy verbatim.
      if sk == ProcS:
        # Entering a proc decl: give it its own growth budget, sized from its
        # body, and restore the enclosing one afterwards (procs are top-level
        # in NIFC, but the restore keeps this correct either way).
        var probe = n
        let pd = takeProcDecl(probe)
        let bodySize = (if pd.body.isTagLit: tokenCount(pd.body) else: 0)
        let savedGrowth = c.growthLeft
        c.growthLeft = growthBudget(bodySize)
        dest.addParLe(n.cursorTagId, n.info)
        into n:
          while n.hasMore:
            trIntra(c, dest, n)
        dest.addParRi()
        c.growthLeft = savedGrowth
        return
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
            c.inProgress.incl calleeSym
            var expanded = createTokenBuf(spliced.len)
            var inner = beginRead(spliced)
            for _ in 0 ..< nEmitted:
              trIntra(c, expanded, inner)
            endRead(inner)
            c.inProgress.excl calleeSym
            prunedInto(dest, expanded)
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

proc intraModuleInline*(moduleSuffix: string; buf: var TokenBuf) =
  ## Same-module inliner pass run as the last step of hexer's `expand`, so
  ## the `.x.nif` we publish has each tiny proc body already cascaded
  ## against its same-module callees. An importer then pulls a flat body, and
  ## the cascade is walked once per module here instead of once per importer.
  ## A body that grows past `InlineTinyBound` by this flattening is simply
  ## re-measured — and demoted to the scored tier — by whoever parses the
  ## published file (`indexProcBodies`), so the flattening cannot compound:
  ## what importers splice is what they measured.
  ##
  ## Measured on nimsem (126 modules), the flattening redundancy is worth
  ## little: the chains that matter run *across* modules (`nifcore` →
  ## `nifpools`), which no same-module flattening can pre-expand, so the
  ## inter-module pass still needs its depth and its cost is unchanged. Keep
  ## the numbers in mind before spending anything more here — full rebuild
  ## 18.66s → 18.87s, nimsem-on-system.nim 80ms → 81ms, binary −4KB.
  ##
  ## `xnifDir` stays empty here on purpose. This module's own `.x.nif` is
  ## pre-DCE, so its generic instances and hexer-minted types still carry the
  ## own-module shorthand (`seq.0.Ixdx2fh1.`) that dce2 later resolves to one
  ## canonical owner; a body copied *within* the module keeps meaning the same
  ## thing, while one copied *across* modules would not. Cross-module splicing
  ## therefore waits for the `.c.nif` (`shoggoth`'s inter-module pass).
  let ma = analyzeModule(buf)
  if ma.inlineInfo.len == 0: return

  # Only the `.inline` bodies are flattened, not every call site in the module:
  # a call site here is one the importer's own pass would splice anyway, and
  # doing it twice only inflates the `.x.nif` everything downstream reads.
  var ctx = initInlinerCtx(moduleSuffix, addr buf, maxDepth = 4,
                           counterPrefix = "h")
  collectProcBodies(ctx)
  var dest = createTokenBuf(buf.len + buf.len div 16)
  var n = beginRead(buf)
  if n.stmtKind != StmtsS: return
  dest.addParLe(n.cursorTagId, n.info)
  n.into:
    while n.hasMore:
      if n.isTagLit and n.stmtKind == ProcS and
         n.childCursor.isSymbolDef and ma.inlineInfo.hasKey(n.childCursor.symId):
        let tag = n.cursorTagId
        let info = n.info
        let d = takeProcDecl(n)
        dest.addParLe(tag, info)
        dest.addSubtree d.name
        dest.addSubtree d.params
        dest.addSubtree d.returnType
        dest.addSubtree d.pragmas
        var body = d.body
        ctx.growthLeft = growthBudget(tokenCount(body))
        trIntra(ctx, dest, body)
        dest.addParRi()
      else:
        dest.takeTree n
  dest.addParRi()
  buf = ensureMove(dest)
