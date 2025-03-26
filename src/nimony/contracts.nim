#       Nimony
# (c) Copyright 2025 Andreas Rumpf
#
# See the file "license.txt", included in this
# distribution, for details about the copyright.

##[
Contract analysis. Tries to prove or disprove `.requires` and `.ensures`
annotations.

The analysis is performed on the control flow graph of a routine or toplevel
code snippet.

In order to not to be too annoying in the case of a contract violation, the
compiler emits a warning (that can be suppressed or turned into an error).
The warning also triggers a runtime check so this entire pass becomes yet
another code transformation.

]##

import std / [assertions, tables, intsets]

include nifprelude

import ".." / models / tags
import nimony_model, programs, decls, typenav, sembasics, reporters, renderer, typeprops,
 controlflow, inferle, xints

type
  BasicBlockIdx = distinct int
  BasicBlock = object
    indegree, touched: int
    indegreeFacts: Facts
  Context = object
    cf, dest, toplevelStmts: TokenBuf
    routines: seq[Cursor]
    typeCache: TypeCache
    facts: Facts
    writesTo: seq[SymId]
    #toPropId: Table[SymId, VarId]
    startInstr: Cursor

proc contractViolation(c: var Context; orig: Cursor; fact: LeXplusC; report: bool) =
  if report:
    echo "known facts in this context: "
    for i in 0 ..< c.facts.len:
      echo c.facts[i]
    echo "canonical fact: ", fact
  error "contract violation: ", orig

proc takeToken(c: var Context; n: var Cursor) {.inline.} =
  c.dest.add n
  inc n

proc takeParRi(c: var Context; n: var Cursor) =
  if n.kind == ParRi:
    c.dest.add n
    inc n
  else:
    error "expected ')', but got: ", n

#[

proc foo(x: int) {.requires: x > 0.}
foo(2)

--> We must translate the `requires: x > 0.` to `2 > 0`.
But inferle already requires us to do a translation step here.

]#

proc argAt(call: Cursor; pos: int): Cursor =
  result = call
  inc result
  for i in 0 ..< pos: skip result

type
  ProofRes = enum
    Unprovable, Disproven, Proven

proc `and`(a, b: ProofRes): ProofRes =
  if a == Proven and b == Proven:
    Proven
  elif a == Disproven or b == Disproven:
    Disproven
  else:
    Unprovable

proc `or`(a, b: ProofRes): ProofRes =
  if a == Proven or b == Proven:
    Proven
  elif a == Disproven and b == Disproven:
    Disproven
  else:
    Unprovable

proc `not`(a: ProofRes): ProofRes =
  if a == Unprovable:
    Unprovable
  elif a == Proven:
    Disproven
  else:
    Proven

template getVarId(c: var Context; symId: SymId): VarId = VarId(symId) #c.toPropId.getOrDefault(symId)

proc mapSymbol(c: var Context; paramMap: Table[SymId, int]; call: Cursor; symId: SymId): VarId =
  result = VarId(0)
  let pos = paramMap.getOrDefault(symId)
  if pos > 0:
    let arg = call.argAt(pos)
    if arg.kind == Symbol:
      result = getVarId(c, arg.symId)

proc compileCmp(c: var Context; paramMap: Table[SymId, int]; req, call: Cursor): LeXplusC =
  var r = req
  var a = InvalidVarId
  var b = InvalidVarId
  var cnst = createXint(0'i32)
  if r.kind == Symbol:
    a = mapSymbol(c, paramMap, call, r.symId)
    inc r
  if r.kind == Symbol:
    b = mapSymbol(c, paramMap, call, r.symId)
    inc r
  elif r.kind == IntLit:
    b = VarId(0)
    cnst = createXint(pool.integers[r.intId])
    inc r
  elif r.kind == UIntLit:
    b = VarId(0)
    cnst = createXint(pool.uintegers[r.uintId])
    inc r
  elif (let op = r.exprKind; op in {AddX, SubX}):
    inc r
    skip r # type
    if r.kind == Symbol:
      b = mapSymbol(c, paramMap, call, r.symId)
      inc r
      if r.kind == IntLit:
        cnst = createXint(pool.integers[r.intId])
      elif r.kind == UIntLit:
        cnst = createXint(pool.uintegers[r.uintId])
      else:
        error "expected integer literal but got: ", r
    else:
      error "expected symbol but got: ", r
  result = query(a, b, cnst)
  skipParRi r

proc checkReq(c: var Context; paramMap: Table[SymId, int]; req, call: Cursor): ProofRes =
  case req.exprKind
  of AndX:
    var r = req
    inc r
    let a = checkReq(c, paramMap, r, call)
    skip r
    let b = checkReq(c, paramMap, r, call)
    skipParRi r
    result = a and b
  of OrX:
    var r = req
    inc r
    let a = checkReq(c, paramMap, r, call)
    skip r
    let b = checkReq(c, paramMap, r, call)
    skipParRi r
    result = a or b
  of NotX:
    var r = req
    inc r
    result = not checkReq(c, paramMap, r, call)
    skipParRi r
  of EqX:
    # x == 3?
    var r = req
    inc r
    skip r # skip type
    let cm = compileCmp(c, paramMap, r, call)
    # a <= b + c
    # But we require a == b + c
    # so we also need  a >= b + c  --> b + c <= a  --> b <= a - c
    let cm2 = cm.geXplusC
    if not cm.isValid:
      result = Unprovable
    elif implies(c.facts, cm) and implies(c.facts, cm2):
      result = Proven
    else:
      result = Disproven
  of LeX:
    var r = req
    inc r
    skip r # skip type
    let cm = compileCmp(c, paramMap, r, call)
    if not cm.isValid:
      result = Unprovable
    elif implies(c.facts, cm):
      result = Proven
    else:
      result = Disproven
  of LtX:
    var r = req
    inc r
    skip r # skip type
    let cm = compileCmp(c, paramMap, r, call)
    if not cm.isValid:
      result = Unprovable
    elif implies(c.facts, cm.ltXplusC):
      result = Proven
    else:
      result = Disproven
  else:
    result = Unprovable

proc analyseCallArgs(c: var Context; n: var Cursor; fnType: Cursor) =
  var fnType = skipProcTypeToParams(fnType)
  assert fnType.isParamsTag
  inc fnType # skip `params`
  var paramMap = initTable[SymId, int]() # param to position
  while fnType.kind != ParRi:
    let param = takeLocal(fnType, SkipFinalParRi)
    paramMap[param.name.symId] = paramMap.len+1
  skipParRi fnType
  skip fnType # skip return type
  # now we have the pragmas:
  let req = extractPragma(fnType, RequiresP)
  if not cursorIsNil(req):
    # ... analyse that the input parameters match the requirements
    let res = checkReq(c, paramMap, req, n)
    if res != Proven:
      error "contract violation: ", req
  else:
    while n.kind != ParRi:
      skip n

proc analyseCall(c: var Context; n: var Cursor) =
  inc n # skip call instruction
  let fnType = skipProcTypeToParams(getType(c.typeCache, n))
  assert fnType.isParamsTag
  analyseCallArgs(c, n, fnType)

#[
We use the control flow graph for a structured traversal over the basic blocks (bb):
We start with the proc entry. We follow the bb until we arrive at a `goto` instruction
or at a `ite` instruction which know has two goto instructions.
We know a `goto` enters a different bb. We add this bb to a worklist. We can process
this bb once all its predecessors have been processed. We keep track of that by
lookup table that counts the indegree of each bb.
]#
type
  Continuation = object
    thenPart, elsePart: BasicBlockIdx
    conditionalFacts: int

const
  NoBasicBlock = BasicBlockIdx(-1)
  BasicBlockReturn = BasicBlockIdx(-2)

proc `==`*(a, b: BasicBlockIdx): bool {.borrow.}
proc `<=`*(a, b: BasicBlockIdx): bool {.borrow.}
proc `<`*(a, b: BasicBlockIdx): bool {.borrow.}

proc toBasicBlock*(c: Context; pc: Cursor): BasicBlockIdx {.inline.} =
  result = BasicBlockIdx(cursorToPosition(c.startInstr, pc))

proc eliminateDeadInstructions*(c: TokenBuf; start = 0; last = -1): seq[bool] =
  # Create a sequence to track which instructions are reachable
  result = newSeq[bool](if last < 0: c.len else: last + 1)
  let last = if last < 0: c.len-1 else: min(last, c.len-1)

  # Initialize with the start position
  var worklist = @[start]
  var processed = initIntSet()

  # Process the worklist
  while worklist.len > 0:
    let pos = worklist.pop()
    if pos > last or pos in processed:
      continue

    processed.incl(pos)
    result[pos] = true  # Mark as reachable

    # Handle different instruction types
    if c[pos].kind == GotoInstr:
      let diff = c[pos].getInt28
      if diff != 0:
        worklist.add(pos + diff)  # Add the target of the jump
        # For forward jumps, everything between the goto and its target is potentially unreachable
        if diff > 0:
          # Don't automatically continue to the next instruction after a goto
          continue
    elif cast[TagEnum](c[pos].tag) == IteTagId:
      # For if-then-else, process the condition and both branches
      var p = pos + 1
      # Skip the condition, marking it as reachable
      while p <= last and c[p].kind != GotoInstr:
        result[p] = true
        inc p

      if p <= last and c[p].kind == GotoInstr:
        # Process the then branch target
        let thenDiff = c[p].getInt28
        result[p] = true  # Mark the goto as reachable
        worklist.add(p + thenDiff)

        # Move to the else branch
        inc p
        if p <= last and c[p].kind == GotoInstr:
          # Process the else branch target
          let elseDiff = c[p].getInt28
          result[p] = true  # Mark the goto as reachable
          worklist.add(p + elseDiff)

          # Don't automatically continue to the next instruction after ITE
          continue

    # For regular instructions or after processing special instructions,
    # continue to the next instruction
    worklist.add(pos + 1)

proc computeBasicBlocks*(c: TokenBuf; start = 0; last = -1): Table[BasicBlockIdx, BasicBlock] =
  result = initTable[BasicBlockIdx, BasicBlock]()
  let last = if last < 0: c.len-1 else: min(last, c.len-1)
  result[BasicBlockIdx(start)] = BasicBlock(indegree: 0, indegreeFacts: createFacts())

  # First, eliminate dead code
  let reachable = eliminateDeadInstructions(c, start, last)

  # Now compute basic blocks considering only reachable instructions
  for i in start..last:
    if not reachable[i]:
      continue  # Skip unreachable instructions

    if c[i].kind == GotoInstr:
      let diff = c[i].getInt28
      # Consider only forward jumps that lead to reachable instructions
      if diff > 0 and i+diff <= last and reachable[i+diff]:
        let idx = BasicBlockIdx(i+diff)
        result.mgetOrPut(idx, BasicBlock(indegree: 0, indegreeFacts: createFacts())).indegree += 1

proc rightHandSide(c: var Context; pc: var Cursor; fact: var LeXplusC): bool =
  result = false
  if pc.exprKind in {AddX, SubX}:
    inc pc
    skip pc # type
    if pc.kind == Symbol:
      let symId2 = pc.symId
      fact.b = getVarId(c, symId2)
      inc pc
      if pc.kind == IntLit:
        fact.c = fact.c + createXint(pool.integers[pc.intId])
        result = true
        inc pc
      elif pc.kind == UIntLit:
        fact.c = fact.c + createXint(pool.uintegers[pc.uintId])
        result = true
        inc pc
      else:
        skip pc
    else:
      skip pc
    skipParRi pc
  elif pc.kind == Symbol:
    let symId2 = pc.symId
    fact.b = getVarId(c, symId2)
    result = true
    inc pc
  elif pc.kind == IntLit:
    fact.b = VarId(0)
    fact.c = fact.c + createXint(pool.integers[pc.intId])
    result = true
    inc pc
  elif pc.kind == UIntLit:
    fact.b = VarId(0)
    fact.c = fact.c + createXint(pool.uintegers[pc.uintId])
    result = true
    inc pc
  else:
    skip pc

proc translateCond(c: var Context; pc: var Cursor; wasEquality: var bool): LeXplusC =
  var r = pc
  result = LeXplusC(a: InvalidVarId, b: VarId(0), c: createXint(0'i32))

  var negations = 0
  while r.exprKind == NotX:
    inc negations
    inc r

  let xk = r.exprKind
  if xk in {LeX, LtX}:
    inc r
    skip r # skip type
  elif xk == EqX:
    wasEquality = true
    inc r
    skip r # skip type
  else:
    skip pc
    return result

  if r.kind == IntLit:
    result.a = VarId(0)
    result.c = -createXint(pool.integers[r.intId])
    inc r
  elif r.kind == UIntLit:
    result.a = VarId(0)
    result.c = -createXint(pool.uintegers[r.uintId])
    inc r
  elif r.kind == Symbol:
    result.a = getVarId(c, r.symId)
    inc r
  else:
    skip pc
    return result
  if not rightHandSide(c, r, result):
    result.a = InvalidVarId
  # a < b  --> a <= b - 1:
  if xk == LtX:
    result.c = result.c - createXint(1'i32)
  skipParRi r

  while negations > 0:
    negateFact(result)
    dec negations
    skipParRi r

  pc = r

proc analyseCondition(c: var Context; pc: var Cursor): int =
  var wasEquality = false
  let fact = translateCond(c, pc, wasEquality)
  if fact.isValid:
    c.facts.add fact
    if wasEquality:
      c.facts.add fact.geXplusC
      result = 2
    else:
      result = 1
  else:
    result = 0

proc addAsgnFact(c: var Context; fact: LeXplusC) =
  # we know that `a <= b + c` and `a >= b + c`:
  if fact.isValid:
    c.facts.add fact
    c.facts.add fact.geXplusC

proc analyseAsgn(c: var Context; pc: var Cursor) =
  inc pc # skip asgn instruction
  if pc.kind == Symbol:
    let symId = pc.symId
    var fact = query(getVarId(c, symId), InvalidVarId, createXint(0'i32))
    c.writesTo.add symId
    # after `x = 4` we know two facts: `x >= 4` and `x <= 4`
    inc pc
    if rightHandSide(c, pc, fact):
      if fact.a == fact.b:
        variableChangedByDiff(c.facts, fact.a, fact.c)
      else:
        invalidateFactsAbout(c.facts, fact.a)
        addAsgnFact c, fact
    else:
      invalidateFactsAbout(c.facts, fact.a)
  else:
    skip pc # skip left-hand-side
    skip pc # skip right-hand-side
  skipParRi pc

proc analyseAssume(c: var Context; pc: var Cursor) =
  inc pc
  var wasEquality = false
  let fact = translateCond(c, pc, wasEquality)
  if not fact.isValid:
    error "invalid assume: ", pc
  else:
    c.facts.add fact
    if wasEquality:
      c.facts.add fact.geXplusC
  skipParRi pc

proc analyseAssert(c: var Context; pc: var Cursor) =
  # We also support `(assert (report) (error) <condition>)` for testing purposes.
  let orig = pc
  inc pc
  var report = false
  var shouldError = false
  if pc.pragmaKind == ReportP:
    report = true
    inc pc
    skipParRi pc
  if pc.pragmaKind == ErrorP:
    shouldError = true
    inc pc
    skipParRi pc

  var wasEquality = false
  let fact = translateCond(c, pc, wasEquality)
  if not fact.isValid:
    error "invalid assert: ", orig
  elif implies(c.facts, fact):
    if shouldError:
      contractViolation(c, orig, fact, report)
    elif wasEquality:
      if implies(c.facts, fact.geXplusC):
        if report: echo "OK ", fact
      else:
        if shouldError:
          if report: echo "OK (could indeed not prove) ", fact
        else:
          contractViolation(c, orig, fact, report)
    else:
      if report: echo "OK ", fact
  else:
    if shouldError:
      if report: echo "OK (could indeed not prove) ", fact
    else:
      contractViolation(c, orig, fact, report)
  skipParRi pc

proc traverseBasicBlock(c: var Context; pc: Cursor): Continuation =
  #echo "TRAVERSING BASIC BLOCK: L", toBasicBlock(c, pc).int
  var nested = 0
  var pc = pc
  while true:
    #echo "PC IS: ", pc.kind
    case pc.kind
    of GotoInstr:
      # Every goto intruction leaves the basic block.
      let diff = pc.getInt28
      if diff < 0:
        # it is a backwards jump: In Nimony we know this came from a loop in
        # the control flow graph. So we skip over it and proceed with the BB after the loop:
        inc pc
        return Continuation(thenPart: toBasicBlock(c, pc), elsePart: NoBasicBlock)
      else:
        # ordinary goto, simply follow it:
        return Continuation(thenPart: toBasicBlock(c, pc +! diff), elsePart: NoBasicBlock)
    of ParRi:
      if nested == 0:
        raiseAssert "BUG: unpaired ')'"
      dec nested
      inc pc
    of Symbol:
      inc pc
    of SymbolDef:
      raiseAssert "BUG: symbol definition in single path"
    of EofToken, DotToken, Ident, StringLit, CharLit, IntLit, UIntLit, FloatLit:
      inc pc
    of ParLe:
      #echo "PC IS: ", pool.tags[pc.tag]
      if pc.cfKind == IteF:
        inc pc
        let conditionalFacts = analyseCondition(c, pc)
        # now 2 goto instructions follow:
        let a = pc +! pc.getInt28
        inc pc
        let b = pc +! pc.getInt28
        return Continuation(thenPart: toBasicBlock(c, a), elsePart: toBasicBlock(c, b), conditionalFacts: conditionalFacts)
      else:
        let kind = pc.stmtKind
        case kind
        of AsgnS:
          analyseAsgn(c, pc)
        of AssumeS:
          analyseAssume(c, pc)
        of AssertS:
          analyseAssert(c, pc)
        of RetS:
          # check if `result` fullfills the `.ensures` contract.
          return Continuation(thenPart: BasicBlockReturn, elsePart: NoBasicBlock)
        of StmtsS, ScopeS, BlockS, ContinueS, BreakS:
          inc pc
          inc nested
        of LocalDecls:
          inc pc
          let name = pc.symId
          skip pc # name
          skip pc # export marker
          skip pc # pragmas
          c.typeCache.registerLocal(name, cast[SymKind](kind), pc)
          skip pc # type
          inc nested
          # proceed with its value here
        of NoStmt:
          raiseAssert "BUG: unknown statement: " & toString(pc, false)
        of DiscardS:
          inc pc
          inc nested
        of CallS, CmdS:
          analyseCall(c, pc)
        of EmitS, InclS, ExclS:
          # not of interest for contract analysis:
          skip pc
        of IfS, WhenS, WhileS, ForS, CaseS, TryS, YldS, RaiseS, ExportS,
           IncludeS, ImportS, FromimportS, ImportExceptS, CommentS, PragmasS,
           ImportasS, ExportexceptS, BindS, MixinS, UsingS,
           UnpackDeclS, StaticstmtS, AsmS, DeferS:
          raiseAssert "BUG: statement not eliminated: " & $pc.stmtKind
        of ProcS, FuncS, IteratorS, ConverterS, MethodS, MacroS, TemplateS, TypeS:
          # declarative junk we don't care about:
          skip pc
  return Continuation(thenPart: BasicBlockReturn, elsePart: NoBasicBlock)

proc decAndTest(x: var int): bool {.inline.} =
  dec x
  result = x == 0

proc takeFacts(c: var Context; bb: var BasicBlock; conditionalFacts: int; negate: bool) =
  #let start = bb.indegreeFacts.len
  if bb.touched == 0:
    for i in 1 ..< c.facts.len - conditionalFacts:
      bb.indegreeFacts.add c.facts[i]
    if negate and conditionalFacts > 1:
      # negation of (a and b) would be (not a or not b) so we cannot model that:
      discard "must lose information here"
    else:
      for i in c.facts.len - conditionalFacts ..< c.facts.len:
        var f = c.facts[i]
        if negate: negateFact(f)
        bb.indegreeFacts.add f
  else:
    # merge the facts:
    bb.indegreeFacts = merge(c.facts, c.facts.len - conditionalFacts, bb.indegreeFacts, negate)
  inc bb.touched
  #c.facts.shrink c.facts.len - conditionalFacts

proc pushFacts(c: var Context; bb: var BasicBlock) =
  #echo "PUSHING FACTS ", bb.indegreeFacts.len
  c.facts.shrink 0
  for i in 0 ..< bb.indegreeFacts.len:
    c.facts.add bb.indegreeFacts[i]

proc checkContracts(c: var Context; n: Cursor) =
  c.cf = toControlflow(n)
  c.facts = createFacts()
  freeze c.cf
  #echo "CF IS ", codeListing(c.cf)

  c.startInstr = readonlyCursorAt(c.cf, 0)
  var body = c.startInstr
  if body.stmtKind in {ProcS, FuncS, IteratorS, ConverterS, MethodS, MacroS}:
    inc body
    for i in 0 ..< BodyPos: skip body

  var current = BasicBlockIdx(cursorToPosition(c.cf, body))
  var bbs = computeBasicBlocks(c.cf, current.int)
  var nextIter = true
  var candidates = newSeq[BasicBlockIdx]()
  while nextIter or candidates.len > 0:
    if not nextIter:
      current = candidates.pop()
    nextIter = false

    var pc = readonlyCursorAt(c.cf, current.int)
    pushFacts(c, bbs[current])
    let cont = traverseBasicBlock(c, pc)

    if cont.thenPart > NoBasicBlock:
      let bb = addr(bbs[cont.thenPart])
      takeFacts(c, bb[], cont.conditionalFacts, false)
      if decAndTest(bb.indegree):
        current = cont.thenPart
        nextIter = true
    if cont.elsePart > NoBasicBlock:
      let bb = addr(bbs[cont.elsePart])
      takeFacts(c, bb[], cont.conditionalFacts, true)
      if decAndTest(bb.indegree):
        if not nextIter:
          current = cont.elsePart
          nextIter = true
        else:
          candidates.add cont.elsePart

proc traverseProc(c: var Context; n: var Cursor) =
  let orig = n
  let r = takeRoutine(n, SkipExclBody)
  skip n # effects
  if not isGeneric(r):
    c.routines.add orig
    var nested = 0
    while true:
      # don't forget about inner procs:
      let sk = n.stmtKind
      if sk in {ProcS, FuncS, IteratorS, ConverterS, MethodS}:
        traverseProc(c, n)
      elif sk in {MacroS, TemplateS, TypeS, CommentS, PragmasS}:
        skip n
      elif n.kind == ParLe:
        inc nested
        inc n
      elif n.kind == ParRi:
        dec nested
        inc n
        if nested == 0: break
      else:
        inc n
    skipParRi n
  else:
    skip n # body
    skipParRi n

proc traverseToplevel(c: var Context; n: var Cursor) =
  case n.stmtKind
  of StmtsS:
    c.toplevelStmts.add n
    inc n
    while n.kind != ParRi:
      traverseToplevel(c, n)
    c.toplevelStmts.add n
    skipParRi n
  of ProcS, FuncS, IteratorS, ConverterS, MethodS:
    traverseProc(c, n)
  of MacroS, TemplateS, TypeS, CommentS, PragmasS,
     ImportasS, ExportexceptS, BindS, MixinS, UsingS,
     ExportS,
     IncludeS, ImportS, FromimportS, ImportExceptS:
    skip n
  of IfS, WhenS, WhileS, ForS, CaseS, TryS, YldS, RaiseS,
     UnpackDeclS, StaticstmtS, AsmS, DeferS,
     CallS, CmdS, GvarS, TvarS, VarS, ConstS, ResultS,
     GletS, TletS, LetS, CursorS, BlockS, EmitS, AsgnS, ScopeS,
     BreakS, ContinueS, RetS, InclS, ExclS, DiscardS, AssumeS, AssertS, NoStmt:
    c.toplevelStmts.takeTree n

proc analyzeContracts*(input: var TokenBuf): TokenBuf =
  let oldInfos = prepare(input)
  var c = Context(typeCache: createTypeCache(),
    dest: createTokenBuf(500))
  c.typeCache.openScope()
  var n = beginRead(input)
  traverseToplevel c, n
  for r in c.routines:
    checkContracts(c, r)
  var nt = beginRead c.toplevelStmts
  checkContracts(c, nt)

  endRead input
  restore(input, oldInfos)
  c.typeCache.closeScope()
  result = ensureMove(c.dest)

when isMainModule:
  import std / [syncio, os]
  proc main(infile: string) =
    var input = parse(readFile(infile))
    discard analyzeContracts(input)
    #echo toString(outp, false)

  main(paramStr(1))
