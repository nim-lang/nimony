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

import std / [assertions, tables]

include nifprelude

import ".." / models / tags
import nimony_model, programs, decls, typenav, sembasics, reporters, renderer, typeprops,
 controlflow, inferle, xints

type
  Context = object
    cf, dest: TokenBuf
    typeCache: TypeCache
    facts: Facts
    writesTo: seq[SymId]
    toPropId: Table[SymId, VarId]
    startInstr: Cursor

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

proc mapSymbol(c: var Context; paramMap: Table[SymId, int]; call: Cursor; symId: SymId): VarId =
  result = VarId(0)
  let pos = paramMap.getOrDefault(symId)
  if pos > 0:
    let arg = call.argAt(pos)
    if arg.kind == Symbol:
      result = c.toPropId.getOrDefault(arg.symId)

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
    cnst = createXint(pool.integers[r.intId])
    inc r
  elif r.kind == UIntLit:
    cnst = createXint(pool.uintegers[r.uintId])
    inc r
  elif (let op = r.exprKind; op in {AddX, SubX}):
    inc r
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
  assert fnType == "params"
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
  assert fnType == "params"
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
  BasicBlockIdx = distinct int
  Continuation = object
    thenPart, elsePart: BasicBlockIdx
    newFacts: int
  BasicBlock = object
    indegree: int
    indegreeFacts: Facts

const
  NoBasicBlock = BasicBlockIdx(-1)
  BasicBlockReturn = BasicBlockIdx(-2)

proc `==`*(a, b: BasicBlockIdx): bool {.borrow.}
proc `<=`*(a, b: BasicBlockIdx): bool {.borrow.}
proc `<`*(a, b: BasicBlockIdx): bool {.borrow.}

proc toBasicBlock*(c: Context; pc: Cursor): BasicBlockIdx {.inline.} =
  result = BasicBlockIdx(cursorToPosition(c.startInstr, pc))

proc computeBasicBlocks*(c: TokenBuf; start = 0; last = -1): Table[BasicBlockIdx, BasicBlock] =
  result = initTable[BasicBlockIdx, BasicBlock]()
  let last = if last < 0: c.len-1 else: min(last, c.len-1)
  for i in start..last:
    if c[i].kind == GotoInstr:
      let diff = c[i].getInt28
      # we ignore backward jumps for now:
      if diff > 0:
        let idx = BasicBlockIdx(i+diff)
        result.mgetOrPut(idx, BasicBlock(indegree: 0)).indegree += 1

proc analyseCondition(c: var Context; pc: var Cursor): int =
  var r = pc
  var a = InvalidVarId
  var b = InvalidVarId
  var cnst = createXint(0'i32)
  if r.kind == Symbol:
    a = c.toPropId.getOrDefault(r.symId)
    inc r
  else:
    skip pc
    return 0
  if r.kind == Symbol:
    b = c.toPropId.getOrDefault(r.symId)
    inc r
  elif r.kind == IntLit:
    cnst = createXint(pool.integers[r.intId])
    inc r
  elif r.kind == UIntLit:
    cnst = createXint(pool.uintegers[r.uintId])
    inc r
  elif (let op = r.exprKind; op in {AddX, SubX}):
    inc r
    if r.kind == Symbol:
      b = c.toPropId.getOrDefault(r.symId)
      inc r
      if r.kind == IntLit:
        cnst = createXint(pool.integers[r.intId])
      elif r.kind == UIntLit:
        cnst = createXint(pool.uintegers[r.uintId])
      else:
        error "expected integer literal but got: ", r
    else:
      error "expected symbol but got: ", r
  let fact = query(a, b, cnst)
  if fact.isValid:
    c.facts.add fact
    result = 1
  else:
    result = 0
  skipParRi r
  pc = r

proc addAsgnFact(c: var Context; fact: LeXplusC) =
  # we know that `a <= b + c` and `a >= b + c`:
  c.facts.add fact
  c.facts.add fact.geXplusC

proc analyseAsgn(c: var Context; pc: var Cursor) =
  inc pc # skip asgn instruction
  if pc.kind == Symbol:
    var fact = query(InvalidVarId, InvalidVarId, createXint(0'i32))
    let symId = pc.symId
    c.writesTo.add symId
    # after `x = 4` we know two facts: `x >= 4` and `x <= 4`
    fact.a = c.toPropId.getOrDefault(symId, InvalidVarId)
    inc pc
    if pc.exprKind in {AddX, SubX}:
      inc pc
      if pc.kind == Symbol:
        let symId2 = pc.symId
        fact.b = c.toPropId.getOrDefault(symId2, InvalidVarId)
        inc pc
        if pc.kind == IntLit:
          fact.c = createXint(pool.integers[pc.intId])
          addAsgnFact c, fact
          inc pc
        elif pc.kind == UIntLit:
          fact.c = createXint(pool.uintegers[pc.uintId])
          addAsgnFact c, fact
          inc pc
        else:
          skip pc
      else:
        skip pc
      skipParRi pc
    elif pc.kind == Symbol:
      let symId2 = pc.symId
      fact.b = c.toPropId.getOrDefault(symId2, InvalidVarId)
      addAsgnFact c, fact
      inc pc
    elif pc.kind == IntLit:
      fact.c = createXint(pool.integers[pc.intId])
      addAsgnFact c, fact
      inc pc
    elif pc.kind == UIntLit:
      fact.c = createXint(pool.uintegers[pc.uintId])
      addAsgnFact c, fact
      inc pc
    else:
      skip pc
  else:
    skip pc # skip left-hand-side
    skip pc # skip right-hand-side
  skipParRi pc

proc traverseBasicBlock(c: var Context; pc: Cursor): Continuation =
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
        let newFacts = analyseCondition(c, pc)
        # now 2 goto instructions follow:
        let a = pc +! pc.getInt28
        inc pc
        let b = pc +! pc.getInt28
        return Continuation(thenPart: toBasicBlock(c, a), elsePart: toBasicBlock(c, b), newFacts: newFacts)
      else:
        let kind = pc.stmtKind
        case kind
        of AsgnS:
          analyseAsgn(c, pc)
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

proc takeFacts(c: var Context; bb: var BasicBlock; newFacts: int; negate: bool) =
  let start = bb.indegreeFacts.len
  for i in c.facts.len - newFacts ..< c.facts.len:
    bb.indegreeFacts.add c.facts[i]
  c.facts.shrink c.facts.len - newFacts
  if negate:
    negateFacts(bb.indegreeFacts, start)

proc pushFacts(c: var Context; bb: var BasicBlock) =
  for i in 0 ..< bb.indegreeFacts.len:
    c.facts.add bb.indegreeFacts[i]

proc checkContracts(c: var Context) =
  var bbs = computeBasicBlocks(c.cf)
  var n = readonlyCursorAt(c.cf, 0)
  #echo "LOOKING AT: ", codeListing(c)
  var pc = n
  var nextIter = true
  while nextIter:
    nextIter = false
    #echo "Looking at: ", toString(pc, false)
    let cont = traverseBasicBlock(c, pc)
    if cont.thenPart > NoBasicBlock:
      takeFacts(c, bbs[cont.thenPart], cont.newFacts, false)
      if decAndTest(bbs[cont.thenPart].indegree):
        pc = readonlyCursorAt(c.cf, cont.thenPart.int)
        pushFacts(c, bbs[cont.thenPart])
        nextIter = true
    elif cont.elsePart > NoBasicBlock:
      takeFacts(c, bbs[cont.elsePart], cont.newFacts, true)
      if decAndTest(bbs[cont.elsePart].indegree):
        pc = readonlyCursorAt(c.cf, cont.elsePart.int)
        pushFacts(c, bbs[cont.elsePart])
        nextIter = true

proc analyzeContracts*(input: var TokenBuf): TokenBuf =
  let oldInfos = prepare(input)
  var c = Context(typeCache: createTypeCache(),
    dest: createTokenBuf(500),
    cf: toControlflow(beginRead input))
  freeze c.cf
  #echo "CF IS ", codeListing(c.cf)
  c.typeCache.openScope()

  checkContracts(c)

  endRead input
  restore(input, oldInfos)
  c.typeCache.closeScope()
  result = ensureMove(c.dest)
