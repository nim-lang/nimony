#
#
#           Hexer Compiler
#        (c) Copyright 2025 Andreas Rumpf
#
#    See the file "license.txt", included in this
#    distribution, for details about the copyright.
#

## Prepare for dead code elimination, generic instance merging,
## and cross-module inlining decisions.
##
## The inliner reuses this pass because (a) we already walk every proc
## body here, so computing per-parameter inlining weights is a free
## piggyback, and (b) we already ship a per-module sidecar that other
## modules read, so cross-module callers can consult the weights cheaply.
## Bodies themselves are fetched on demand via the NIF index, not via
## this sidecar.

import std / [assertions, tables, hashes, sets, syncio]
include ".." / lib / nifprelude
include ".." / lib / compat2
import ".." / nifc / [nifc_model]

import ".." / lib / symparser

type
  InlineWeights* = seq[int]
    ## Weight vector for inlining heuristics — one entry per parameter
    ## position. At a callsite the inliner computes
    ## `sum(weights[i] * argScore(arg[i]))` and inlines if it crosses
    ## the threshold.

  InlineInfo* = object
    ## Per-procedure inlining decision data.
    threshold*: int
      ## Score required for full inlining.
      ##   0     → always inline (e.g. `.inline` pragma)
      ##   100   → normal heuristic budget
      ##   10000 → effectively never inline (`.noinline`)
    weights*: InlineWeights
      ## Per-parameter weights consulted for the full-inline decision.
    guardThreshold*: int
      ## Score required for partial / guard inlining (inline only the
      ## early-exit branches, leave the slow path as a call).
    guards*: InlineWeights
      ## Per-parameter weights for the guard-inline decision. Empty when
      ## the procedure has no guard pattern.

  ModuleAnalysis* = object
    uses*: Table[SymId, HashSet[SymId]]
    roots*: HashSet[SymId]
    offers*: HashSet[SymId] # generic instances that are offered by this module
    inlineInfo*: Table[SymId, InlineInfo] # per-procedure inlining info

const
  DefaultInlineInfo* = InlineInfo(threshold: 100, weights: @[],
                                  guardThreshold: 100, guards: @[])

type
  WeightCtx = enum
    ## Context the inliner gives a parameter use, chosen by the nearest
    ## enclosing operation. The weight assigned to a use is `WeightOf[ctx]`.
    wcOther        # plain statement / unrecognized position — no signal
    wcCall         # call argument
    wcArith        # arithmetic / bitwise / boolean operand
    wcCompare      # comparison operand (eq/lt/le/…)
    wcArrayIndex   # array indexing (at/pat)
    wcBranchCond   # condition of if/while/case

const
  WeightOf: array[WeightCtx, int] = [
    wcOther:       0,
    wcCall:       10,
    wcArith:      20,
    wcCompare:    30,
    wcArrayIndex: 40,
    wcBranchCond: 50,
  ]

proc childContext(c: Cursor; outer: WeightCtx): WeightCtx =
  ## Context the operator at `c` imposes on its immediate children.
  ## Returns `outer` when the node is structurally transparent so the
  ## context propagates through unrecognized wrappers (conversions etc.).
  case c.exprKind
  of EqC, NeqC, LeC, LtC: return wcCompare
  of AddC, SubC, MulC, DivC, ModC, ShrC, ShlC,
     BitandC, BitorC, BitxorC, BitnotC, NegC,
     AndC, OrC, NotC:     return wcArith
  of AtC, PatC:           return wcArrayIndex
  of CallC:               return wcCall
  else: discard
  case c.stmtKind
  of IfS, WhileS, CaseS, IteS, ItecS, LoopS: return wcBranchCond
  of CallS:               return wcCall
  of StmtsS, ScopeS, RetS, AsgnS, StoreS,
     DiscardS, BreakS, JmpS, RaiseS:         return wcOther
  else: discard
  case c.substructureKind
  of ElifU:               return wcBranchCond
  of ElseU, OfU, KvU:     return wcOther
  else: discard
  return outer

proc walkBody(n: var Cursor; paramIdx: Table[SymId, int];
              weights: var seq[int]; ctx: WeightCtx; cost: var int) =
  ## Walks one subtree of a proc body, doing two jobs at once:
  ##   - accumulates weight for each parameter reference encountered;
  ##   - sums a body-cost proxy (token count, minus declarative
  ##     subtrees that don't generate per-call-site code).
  ##
  ## Subtrees skipped from the cost (and the recursion entirely):
  ##   - type expressions — `(i 64)`, `(ptr T)`, etc. Encoded as the
  ##     param/var/cast type slot; no code emission of their own.
  ##   - `(pragmas …)` sections — annotations, no code.
  case n.kind
  of Symbol:
    inc cost
    if paramIdx.hasKey(n.symId):
      weights[paramIdx.getOrQuit(n.symId)] += WeightOf[ctx]
    inc n
  of ParLe:
    if n.typeKind != NoType:
      skip n
      return
    if n.substructureKind == PragmasU:
      skip n
      return
    inc cost                       # open paren
    let nextCtx = childContext(n, ctx)
    inc n
    while n.kind != ParRi:
      walkBody(n, paramIdx, weights, nextCtx, cost)
    inc cost                       # close paren
    inc n
  of ParRi:
    discard
  else:
    inc cost
    inc n

proc collectParams(params: Cursor): seq[SymId] =
  ## Parameter symbols in declared order. Returns an empty seq for `(.)`
  ## (no params) so callers don't need to special-case forward decls.
  result = @[]
  if params.kind != ParLe: return
  var p = params
  inc p
  while p.kind != ParRi:
    if p.substructureKind == ParamU:
      var inner = p
      inc inner
      if inner.kind == SymbolDef:
        result.add inner.symId
    skip p

proc pragmaInlineHint(pragmas: Cursor): int =
  ## Looks for `.inline` / `.noinline` and returns the resulting threshold
  ## (0, 10000, or -1 if neither is present).
  if pragmas.kind != ParLe: return -1
  var p = pragmas
  inc p
  while p.kind != ParRi:
    if p.kind == ParLe:
      case p.pragmaKind
      of InlineP:   return 0
      of NoinlineP: return 10000
      else: discard
    skip p
  return -1

proc isNonTrivial(info: InlineInfo): bool =
  if info.threshold != DefaultInlineInfo.threshold: return true
  if info.guardThreshold != DefaultInlineInfo.guardThreshold: return true
  for w in info.weights:
    if w != 0: return true
  for g in info.guards:
    if g != 0: return true
  return false

proc computeInlineInfo*(procDecl: Cursor): InlineInfo =
  ## Compute inlining info for a procedure by analysing its body.
  ##
  ## Each parameter use accumulates a weight chosen by the nearest
  ## enclosing operation (see `WeightCtx` / `WeightOf`). At a callsite
  ## the inliner computes `sum(weights[i] * argScore[i] / 100)` and
  ## inlines if it crosses `threshold`.
  ##
  ## Argument scoring is the inliner's responsibility (planned scale,
  ## 0-100):
  ##   - literal constant         → 100
  ##   - immutable binding        →  50
  ##   - simple field/index read  →  30
  ##   - mutable variable         →  20
  ##   - complex expression       →   0
  ##
  ## Guard / partial-inline weights are not produced yet — that pattern
  ## (early-exit `if cond: return X`) needs a separate detector and
  ## will land in a follow-up.
  result = DefaultInlineInfo
  var p = procDecl
  let pd = takeProcDecl(p)

  let params = collectParams(pd.params)
  result.weights = newSeq[int](params.len)

  # Pragmas can short-circuit the body walk.
  let hint = pragmaInlineHint(pd.pragmas)
  if hint >= 0:
    result.threshold = hint
    if hint == 0 or hint == 10000:
      # `.inline` / `.noinline` — no point computing weights.
      return

  # Forward declarations and importc'd procs have no body to analyse.
  if pd.body.kind == DotToken: return
  if params.len == 0: return

  var paramIdx = initTable[SymId, int]()
  for i, s in params: paramIdx[s] = i

  # Single walk: accumulates per-parameter weights AND a body-cost
  # proxy used to scale the threshold. Without the cost-scaling,
  # `semExpr`-class procs (huge bodies, many call sites with constant
  # first args) collect enough weighted score to look inlinable; with
  # it, the per-arg score has to grow with the body to clear the bar.
  var body = pd.body
  var cost = 0
  walkBody(body, paramIdx, result.weights, wcOther, cost)
  if hint < 0:
    result.threshold = max(DefaultInlineInfo.threshold, cost)

proc tr(n: var Cursor; a: var ModuleAnalysis; owner: SymId) =
  case n.kind
  of ParLe:
    case n.stmtKind
    of ProcS:
      let procStart = n
      inc n
      var newOwner = owner
      let symName = pool.syms[n.symId]
      var procSym = SymId(0)
      if n.kind == SymbolDef:
        procSym = n.symId
        if isInstantiation(symName):
          a.offers.incl(n.symId)
        if not isLocalName(symName):
          newOwner = n.symId

      while n.hasMore:
        tr n, a, newOwner
      inc n

      # Compute inlining info for procedures visible to other modules.
      # Local-only procs can be inlined directly from their own module's
      # body without needing the sidecar, so skip them for now.
      if procSym != SymId(0) and not isLocalName(symName):
        let info = computeInlineInfo(procStart)
        if isNonTrivial(info):
          a.inlineInfo[procSym] = info
    of TypeS, VarS, ConstS, GvarS, TvarS:
      inc n
      var newOwner = owner
      let symName = pool.syms[n.symId]
      if n.kind == SymbolDef:
        if isInstantiation(symName):
          a.offers.incl(n.symId)
        if not isLocalName(symName):
          newOwner = n.symId

      while n.hasMore:
        tr n, a, newOwner
      inc n
    else:
      if n.substructureKind == FldU:
        inc n
        let symName = pool.syms[n.symId]
        if n.kind == SymbolDef:
          if isInstantiation(symName):
            a.offers.incl(n.symId)
      elif n.substructureKind == PragmasU:
        # Check if this pragma section contains exportc
        # If so, mark the owner as a root (exportc symbols are entry points)
        inc n
        var hasExportc = false
        while n.hasMore:
          if n.kind == ParLe and n.pragmaKind == ExportcP:
            hasExportc = true
          tr n, a, owner
        inc n
        if hasExportc and owner != SymId(0):
          a.roots.incl(owner)
        return
      else:
        inc n
      while n.hasMore:
        tr n, a, owner
      inc n
  of Symbol:
    if not isLocalName(pool.syms[n.symId]):
      if owner == SymId(0):
        a.roots.incl(n.symId)
      else:
        if not a.uses.hasKey(owner): a.uses[owner] = initHashSet[SymId]()
        a.uses.getOrQuit(owner).incl(n.symId)
    inc n
  of SymbolDef, UnknownToken, EofToken, DotToken, Ident, StringLit, CharLit, IntLit, UIntLit, FloatLit: inc n
  of ParRi: raiseAssert "ParRi should not be encountered here"

const
  depName = "uses"
  offerName = "offers"
  rootName = "roots"
  inlineName = "inline"
  weightsName = "w"
  guardsName = "g"

proc prepDce(outputFilename: string; n: Cursor; dottedSuffix: string) =
  var n = n
  var a = ModuleAnalysis()
  tr n, a, SymId(0)

  var b = nifbuilder.open(outputFilename)
  b.withTree "stmts":
    b.withTree rootName:
      for root in a.roots:
        b.addSymbol pool.syms[root], dottedSuffix
    for owner, uses in mpairs(a.uses):
      b.withTree depName:
        b.addSymbol pool.syms[owner], dottedSuffix
        for dep in uses:
          b.addSymbol pool.syms[dep], dottedSuffix
    b.withTree offerName:
      for offer in a.offers:
        b.addSymbol pool.syms[offer], dottedSuffix
    # Format: (inline (w sym threshold w0 w1 ...) (g sym threshold g0 g1 ...) ...)
    # A proc may contribute a `w` entry, a `g` entry, both, or neither.
    if a.inlineInfo.len > 0:
      b.withTree inlineName:
        for sym, info in a.inlineInfo.pairs:
          if info.weights.len > 0 or info.threshold != DefaultInlineInfo.threshold:
            b.withTree weightsName:
              b.addSymbol pool.syms[sym], dottedSuffix
              b.addIntLit info.threshold
              for w in info.weights:
                b.addIntLit w
          if info.guards.len > 0 or info.guardThreshold != DefaultInlineInfo.guardThreshold:
            b.withTree guardsName:
              b.addSymbol pool.syms[sym], dottedSuffix
              b.addIntLit info.guardThreshold
              for g in info.guards:
                b.addIntLit g
  b.close()

proc readInlineEntry(infile: string; n: var Cursor; result: var ModuleAnalysis;
                     weightsTag, guardsTag: TagId) =
  ## Read one `(w ...)` or `(g ...)` entry inside `(inline ...)`.
  let entryTag = n.tag
  n.into:
    if n.kind != Symbol:
      raiseAssert infile & ": expected Symbol in inline entry"
    let sym = n.symId
    skip n
    # First int = threshold; subsequent ints = per-parameter weights.
    var thresh = DefaultInlineInfo.threshold
    var ws: seq[int] = @[]
    var first = true
    while n.hasMore:
      if n.kind != IntLit:
        raiseAssert infile & ": expected IntLit in inline entry"
      let v = int(pool.integers[n.intId])
      if first:
        thresh = v
        first = false
      else:
        ws.add v
      skip n
    var info = result.inlineInfo.getOrDefault(sym, DefaultInlineInfo)
    if entryTag == weightsTag:
      info.threshold = thresh
      info.weights = ws
    elif entryTag == guardsTag:
      info.guardThreshold = thresh
      info.guards = ws
    result.inlineInfo[sym] = info

proc readModuleAnalysis*(infile: string): ModuleAnalysis =
  var buf = parseFromFile(infile)
  var n = beginRead(buf)
  result = ModuleAnalysis()
  if n.stmtKind == StmtsS:
    let depTag = pool.tags.getOrIncl(depName)
    let offerTag = pool.tags.getOrIncl(offerName)
    let rootTag = pool.tags.getOrIncl(rootName)
    let inlineTag = pool.tags.getOrIncl(inlineName)
    let weightsTag = pool.tags.getOrIncl(weightsName)
    let guardsTag = pool.tags.getOrIncl(guardsName)
    n.into:                                     # (stmts ...)
      while n.hasMore:
        if n.kind != ParLe:
          raiseAssert infile & ": expected ParLe"
        if n.tag == rootTag:
          n.into:                               # (roots ...)
            while n.hasMore:
              if n.kind == Symbol:
                result.roots.incl(n.symId)
                skip n
              else:
                raiseAssert infile & ": expected Symbol"
        elif n.tag == depTag:
          n.into:                               # (uses ...)
            let key = n.symId
            result.uses[key] = initHashSet[SymId]()
            skip n
            while n.hasMore:
              if n.kind == Symbol:
                result.uses.getOrQuit(key).incl(n.symId)
                skip n
              else:
                raiseAssert infile & ": expected Symbol"
        elif n.tag == offerTag:
          n.into:                               # (offers ...)
            while n.hasMore:
              if n.kind == Symbol:
                result.offers.incl(n.symId)
                skip n
              else:
                raiseAssert infile & ": expected Symbol"
        elif n.tag == inlineTag:
          n.into:                               # (inline (w|g ...) ...)
            while n.hasMore:
              if n.kind != ParLe or (n.tag != weightsTag and n.tag != guardsTag):
                raiseAssert infile & ": expected (w|g ...) inside inline"
              readInlineEntry(infile, n, result, weightsTag, guardsTag)
        else:
          # Skip unknown sections for forward compatibility with future
          # readers that produce sidecars with extra metadata.
          skip n

proc writeDceOutput*(buf: var TokenBuf; outfile, dottedSuffix: string) =
  ## Direct overload that works on an already-parsed token buffer,
  ## avoiding the file read + parse step.
  let n = beginRead(buf)
  prepDce(outfile, n, dottedSuffix)
  endRead(buf)

proc getInlineInfo*(modules: Table[string, ModuleAnalysis]; sym: SymId): InlineInfo =
  ## Look up inlining info for `sym` across all loaded modules.
  ## Returns `DefaultInlineInfo` when the symbol isn't tracked.
  let moduleName = extractModule(pool.syms[sym])
  if moduleName in modules:
    result = modules[moduleName].inlineInfo.getOrDefault(sym, DefaultInlineInfo)
  else:
    result = DefaultInlineInfo

proc hasInlineInfo*(modules: Table[string, ModuleAnalysis]; sym: SymId): bool =
  let moduleName = extractModule(pool.syms[sym])
  modules.hasKey(moduleName) and modules[moduleName].inlineInfo.hasKey(sym)

proc shouldInline*(info: InlineInfo; argScores: openArray[int]): bool =
  ## Full-inlining decision: sum weighted argument scores against the
  ## procedure's threshold.
  var sum = 0
  for i, score in argScores:
    if i < info.weights.len:
      sum += (info.weights[i] * score) div 100
  result = sum >= info.threshold

proc shouldPartialInline*(info: InlineInfo; argScores: openArray[int]): bool =
  ## Partial / guard-only inlining decision. Always false when the
  ## procedure has no guard pattern.
  if info.guards.len == 0: return false
  var sum = 0
  for i, score in argScores:
    if i < info.guards.len:
      sum += (info.guards[i] * score) div 100
  result = sum >= info.guardThreshold
