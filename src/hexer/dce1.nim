#
#
#           Hexer Compiler
#        (c) Copyright 2025 Andreas Rumpf
#
#    See the file "license.txt", included in this
#    distribution, for details about the copyright.
#

## Prepare for dead code elimination, generic instance merging,
## and inlining weight computation.

import std / [assertions, tables, sets]
include nifprelude
import ".." / nifc / [nifc_model]

import symparser

type
  InlineWeights* = seq[int]
    ## Weight vector for inlining heuristics.
    ## Each element corresponds to a parameter position.
    ## At a callsite, weight[i] * argumentScore(arg[i]) is added to a sum.

  InlineInfo* = object
    ## Complete inlining information for a function.
    threshold*: int
      ## Threshold for full inlining. If sum(weights[i] * argScore[i]) >= threshold: inline.
      ## 0 = always inline (.inline pragma)
      ## 100 = normal heuristics
      ## 10000 = effectively never inline (.noinline pragma)
    weights*: InlineWeights
      ## Weights for full inlining decision.
    guardThreshold*: int
      ## Threshold for partial/guard inlining.
    guards*: InlineWeights
      ## Weights for partial inlining decision (guard patterns only).
      ## Empty if function has no guard patterns.

  ModuleAnalysis* = object
    uses*: Table[SymId, HashSet[SymId]]
    roots*: HashSet[SymId]
    offers*: HashSet[SymId] # generic instances that are offered by this module
    inlineInfo*: Table[SymId, InlineInfo] # inlining info per function

proc computeInlineInfo*(procDecl: Cursor): InlineInfo =
  ## Compute inlining information for a procedure by analyzing its body.
  ##
  ## Returns:
  ## - `threshold`: Score needed for full inlining (0 = always, 100 = normal, 10000 = never)
  ## - `weights`: Per-parameter weights for full inlining decision.
  ## - `guardThreshold`: Score needed for partial inlining.
  ## - `guards`: Per-parameter weights for partial inlining (empty if no guards).
  ##
  ## Weight computation considers how parameters are used:
  ## - Parameters used in conditions (if/while/case): high weight (~50)
  ## - Parameters used in comparisons: high weight (~30)
  ## - Parameters used in array indexing: high weight (~40)
  ## - Parameters used in arithmetic: medium weight (~20)
  ## - Parameters passed to functions: low weight (~10)
  ##
  ## Argument scores at call sites (0-100):
  ## - Literal constant: 100
  ## - Immutable binding (let/const): 50
  ## - Simple field/array access: 30
  ## - Variable: 20
  ## - Complex expression: 0
  ##
  ## TODO: Implement the actual weight computation logic.
  ## For now, returns empty info (no inlining heuristics).
  result = InlineInfo(threshold: 100, weights: @[], guardThreshold: 100, guards: @[])

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

      while n.kind != ParRi:
        tr n, a, newOwner
      inc n

      # Compute inlining info for this procedure
      if procSym != SymId(0) and not isLocalName(symName):
        let info = computeInlineInfo(procStart)
        if info.weights.len > 0 or info.guards.len > 0 or info.threshold != 100:
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

      while n.kind != ParRi:
        tr n, a, newOwner
      inc n
    else:
      if n.substructureKind == FldU:
        inc n
        let symName = pool.syms[n.symId]
        if n.kind == SymbolDef:
          if isInstantiation(symName):
            a.offers.incl(n.symId)
      else:
        inc n
      while n.kind != ParRi:
        tr n, a, owner
      inc n
  of Symbol:
    if not isLocalName(pool.syms[n.symId]):
      if owner == SymId(0):
        a.roots.incl(n.symId)
      else:
        if not a.uses.hasKey(owner): a.uses[owner] = initHashSet[SymId]()
        a.uses[owner].incl(n.symId)
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

proc prepDce(outputFilename: string; n: Cursor) =
  var n = n
  var a = ModuleAnalysis()
  tr n, a, SymId(0)

  var b = nifbuilder.open(outputFilename)
  b.withTree "stmts":
    b.withTree rootName:
      for root in a.roots:
        b.addSymbol pool.syms[root]
    for owner, uses in mpairs(a.uses):
      b.withTree depName:
        b.addSymbol pool.syms[owner]
        for dep in uses:
          b.addSymbol pool.syms[dep]
    b.withTree offerName:
      for offer in a.offers:
        b.addSymbol pool.syms[offer]
    # Write inlining info
    # Format: (inline (w sym threshold w0 w1 ...) (g sym guardThreshold g0 g1 ...))
    if a.inlineInfo.len > 0:
      b.withTree inlineName:
        for sym, info in a.inlineInfo.pairs:
          if info.weights.len > 0 or info.threshold != 100:
            b.withTree weightsName:
              b.addSymbol pool.syms[sym]
              b.addIntLit info.threshold
              for w in info.weights:
                b.addIntLit w
          if info.guards.len > 0 or info.guardThreshold != 100:
            b.withTree guardsName:
              b.addSymbol pool.syms[sym]
              b.addIntLit info.guardThreshold
              for g in info.guards:
                b.addIntLit g
  b.close()

proc parseIntLit(n: Cursor): int =
  ## Parse an int literal from the token stream.
  assert n.kind == IntLit
  result = int(pool.integers[n.intId])

proc readModuleAnalysis*(infile: string): ModuleAnalysis =
  var buf = parseFromFile(infile)
  var n = beginRead(buf)
  result = ModuleAnalysis()
  if n.stmtKind == StmtsS:
    inc n
    let depTag = pool.tags.getOrIncl(depName)
    let offerTag = pool.tags.getOrIncl(offerName)
    let rootTag = pool.tags.getOrIncl(rootName)
    let inlineTag = pool.tags.getOrIncl(inlineName)
    let weightsTag = pool.tags.getOrIncl(weightsName)
    let guardsTag = pool.tags.getOrIncl(guardsName)
    while n.kind != ParRi:
      if n.kind == ParLe:
        if n.tag == rootTag:
          inc n
          while n.kind != ParRi:
            if n.kind == Symbol:
              result.roots.incl(n.symId)
              inc n
            else:
              raiseAssert infile & ": expected Symbol"
        elif n.tag == depTag:
          inc n
          let key = n.symId
          result.uses[key] = initHashSet[SymId]()
          inc n
          while n.kind != ParRi:
            if n.kind == Symbol:
              result.uses[key].incl(n.symId)
              inc n
            else:
              raiseAssert infile & ": expected Symbol"
        elif n.tag == offerTag:
          inc n
          while n.kind != ParRi:
            if n.kind == Symbol:
              result.offers.incl(n.symId)
              inc n
            else:
              raiseAssert infile & ": expected Symbol"
        elif n.tag == inlineTag:
          inc n
          while n.kind != ParRi:
            if n.kind == ParLe:
              let entryTag = n.tag
              inc n
              if n.kind == Symbol:
                let sym = n.symId
                inc n
                # First int is threshold, rest are per-parameter weights
                var thresh = 100
                var weights: seq[int] = @[]
                var first = true
                while n.kind != ParRi:
                  if n.kind == IntLit:
                    if first:
                      thresh = parseIntLit(n)
                      first = false
                    else:
                      weights.add parseIntLit(n)
                    inc n
                  else:
                    raiseAssert infile & ": expected IntLit in inline entry"
                # Ensure entry exists for this symbol
                if sym notin result.inlineInfo:
                  result.inlineInfo[sym] = InlineInfo(threshold: 100, weights: @[], guardThreshold: 100, guards: @[])
                # Store in appropriate field
                if entryTag == weightsTag:
                  result.inlineInfo[sym].threshold = thresh
                  result.inlineInfo[sym].weights = weights
                elif entryTag == guardsTag:
                  result.inlineInfo[sym].guardThreshold = thresh
                  result.inlineInfo[sym].guards = weights
              else:
                raiseAssert infile & ": expected Symbol in inline entry"
            else:
              raiseAssert infile & ": expected (w|g ...) in inline"
            inc n
        else:
          # Skip unknown tags for forward compatibility
          skip n
          continue
        inc n
      else:
        raiseAssert infile & ": expected ParLe"

proc writeDceOutput*(buf: var TokenBuf; outfile: string) =
  ## Direct overload that works on an already-parsed token buffer,
  ## avoiding the file read + parse step.
  let n = beginRead(buf)
  prepDce(outfile, n)
  endRead(buf)

const
  DefaultInlineInfo* = InlineInfo(threshold: 100, weights: @[], guardThreshold: 100, guards: @[])

proc getInlineInfo*(modules: Table[string, ModuleAnalysis]; sym: SymId): InlineInfo =
  ## Look up inlining info for a symbol across all loaded modules.
  ## Returns default info if not found.
  let moduleName = extractModule(pool.syms[sym])
  if moduleName in modules:
    result = modules[moduleName].inlineInfo.getOrDefault(sym, DefaultInlineInfo)
  else:
    result = DefaultInlineInfo

proc hasInlineInfo*(modules: Table[string, ModuleAnalysis]; sym: SymId): bool =
  ## Check if a symbol has inlining info defined.
  let moduleName = extractModule(pool.syms[sym])
  if moduleName in modules:
    result = sym in modules[moduleName].inlineInfo
  else:
    result = false

proc shouldInline*(info: InlineInfo; argScores: openArray[int]): bool =
  ## Determine if a function should be fully inlined given argument scores.
  ## argScores: 0-100 per argument (100 = constant, 0 = complex expression)
  var sum = 0
  for i, score in argScores:
    if i < info.weights.len:
      sum += (info.weights[i] * score) div 100
  result = sum >= info.threshold

proc shouldPartialInline*(info: InlineInfo; argScores: openArray[int]): bool =
  ## Determine if a function's guards should be partially inlined.
  ## Returns false if function has no guards.
  if info.guards.len == 0:
    return false
  var sum = 0
  for i, score in argScores:
    if i < info.guards.len:
      sum += (info.guards[i] * score) div 100
  result = sum >= info.guardThreshold
