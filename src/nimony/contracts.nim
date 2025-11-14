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

]##

import std / [assertions, tables, sets, intsets]

include nifprelude

import ".." / models / tags
import nimony_model, programs, decls, typenav, sembasics, reporters, renderer, typeprops,
 controlflow, inferle, xints

type
  BasicBlockIdx = distinct int
  BasicBlock = object
    indegree, touched: int
    indegreeFacts: Facts
    writesTo: seq[SymId]
  Context = object
    cf, toplevelStmts: TokenBuf
    routines: seq[Cursor]
    typeCache: TypeCache
    facts: Facts
    writesTo: seq[SymId]
    #toPropId: Table[SymId, VarId]
    directlyInitialized: HashSet[SymId]
    startInstr: Cursor
    errors: TokenBuf
    procCanRaise: bool

proc buildErr(c: var Context; info: PackedLineInfo; msg: string) =
  when defined(debug):
    writeStackTrace()
    echo infoToStr(info) & " Error: " & msg
    quit msg
  c.errors.buildTree ErrT, info:
    c.errors.addDotToken()
    c.errors.add strToken(pool.strings.getOrIncl(msg), info)

proc contractViolation(c: var Context; orig: Cursor; fact: LeXplusC; report: bool) =
  if report:
    echo "known facts in this context: "
    for i in 0 ..< c.facts.len:
      echo c.facts[i]
    echo "canonical fact: ", fact
  error "contract violation: ", orig

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
    skipParRi r
  result = query(a, b, cnst)

proc checkReq(c: var Context; paramMap: Table[SymId, int]; req, call: Cursor): ProofRes =
  case req.exprKind
  of AndX:
    var r = req
    inc r
    let a = checkReq(c, paramMap, r, call)
    skip r
    let b = checkReq(c, paramMap, r, call)
    result = a and b
  of OrX:
    var r = req
    inc r
    let a = checkReq(c, paramMap, r, call)
    skip r
    let b = checkReq(c, paramMap, r, call)
    result = a or b
  of NotX:
    var r = req
    inc r
    result = not checkReq(c, paramMap, r, call)
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
  of ExprX:
    var r = req
    while r.exprKind == ExprX:
      inc r
      while r.kind != ParRi and not isLastSon(r): skip r
    result = checkReq(c, paramMap, r, call)
  else:
    result = Unprovable

proc analyseCall(c: var Context; n: var Cursor)

proc markedAs(t: Cursor; mark: NimonyOther): bool =
  result = false
  case t.typeKind
  of PtrT, RefT:
    var e = t.firstSon
    skip e # base type
    if e.kind != ParRi and e.substructureKind == mark:
      result = true
  of CstringT, PointerT:
    let e = t.firstSon
    # no base type
    if e.kind != ParRi and e.substructureKind == mark:
      result = true
  else:
    discard

proc analysableRoot(c: var Context; n: Cursor): SymId =
  var n = n
  while true:
    case n.exprKind
    of DotX, TupatX, ArrAtX, HderefX:
      # Cannot analyse expressions yet that involve derefs
      # DerefX, AddrX, HderefX, HaddrX, PatX:
      inc n
    of ConvKinds:
      inc n
      skip n # type part
    of BaseobjX:
      inc n
      skip n # type part
      skip n # skip intlit
    else:
      break
  if n.kind == Symbol:
    result = n.symId
    let x = getLocalInfo(c.typeCache, result)
    if x.kind == GvarY:
      # assume sharing of global variables between threads
      # so `if glob != nil: use glob[]` cannot be proven correct.
      # Maybe this needs a better solution.
      result = NoSymId
  else:
    result = NoSymId

proc wantNotNil(c: var Context; n: Cursor) =
  case n.exprKind
  of NilX:
    buildErr(c, n.info, "expected non-nil value")
  of AddrX:
    discard "fine, addresses are not nil"
  else:
    let t = getType(c.typeCache, n)
    if markedAs(t, NotnilU):
      discard "fine, per type we know it is not nil"
    else:
      let r = analysableRoot(c, n)
      if r == NoSymId:
        if n.exprKind == NewobjX and c.procCanRaise:
          discard "fine, nil value is mapped to OOM by the compiler"
        else:
          buildErr c, n.info, "cannot analyze expression is not nil: " & asNimCode(n)
      else:
        let fact = inferle.isNotNil(VarId r)
        if implies(c.facts, fact):
          discard "fine, did prove access correct"
        else:
          buildErr c, n.info, "cannot prove expression is not nil: " & asNimCode(n)

proc checkNilMatch(c: var Context; n: Cursor; expected: Cursor) =
  if markedAs(expected, NotnilU):
    wantNotNil c, n

proc wantNotNilDeref(c: var Context; n: Cursor) =
  let e = getType(c.typeCache, n)
  # reason: derefs are only interesting when the type was marked as nilable:
  # var x: nil ref T: deref(x) # interesting
  # var x: ref T not nil: deref(x) # safe by construction
  # var x: ref T: deref(x) # should be left unchecked
  if markedAs(e, NilU):
    wantNotNil c, n

proc analyseOconstr(c: var Context; n: var Cursor) =
  inc n
  skip n # type
  while n.kind != ParRi:
    assert n.substructureKind == KvU
    inc n
    let expected = getType(c.typeCache, n)
    skip n # field name
    checkNilMatch c, n, expected
    skip n # value
    if n.kind != ParRi:
      # optional inheritance
      skip n
    skipParRi n
  skipParRi n

proc analyseArrayConstr(c: var Context; n: var Cursor) =
  inc n
  let expected = n.firstSon # element type of the array
  skip n # type
  while n.kind != ParRi:
    checkNilMatch c, n, expected
    skip n
  skipParRi n

proc analyseTupConstr(c: var Context; n: var Cursor) =
  inc n
  var expected = n.firstSon # type of the first field
  skip n # type
  while n.kind != ParRi:
    assert expected.kind != ParRi
    checkNilMatch c, n, getTupleFieldType(expected)
    skip n
    skip expected # type of the next field
  skipParRi n

proc analyseExpr(c: var Context; pc: var Cursor) =
  #echo "analyseExpr ", toString(pc, false)
  var nested = 0
  while true:
    case pc.kind
    of Symbol:
      let symId = pc.symId
      let x = getLocalInfo(c.typeCache, symId)
      if x.kind in {VarY, LetY, CursorY}:
        if symId notin c.directlyInitialized and symId notin c.writesTo:
          buildErr(c, pc.info, "cannot prove that " & pool.syms[symId] & " has been initialized")
          # do not name the same variable twice:
          c.writesTo.add symId
      inc pc
    of SymbolDef:
      bug "symbol definition in single path"
    of EofToken, DotToken, Ident, StringLit, CharLit, IntLit, UIntLit, FloatLit, UnknownToken:
      inc pc
    of ParRi:
      assert nested > 0
      dec nested
      inc pc
    of ParLe:
      case pc.exprKind
      of CallKinds:
        analyseCall c, pc
      of DdotX:
        inc pc
        wantNotNilDeref c, pc
        analyseExpr c, pc # object
        skip pc # field name
        if pc.kind != ParRi: skip pc # inheritence depth
        skipParRi pc
      of DerefX:
        inc pc
        wantNotNilDeref c, pc
        analyseExpr c, pc
        skipParRi pc
      of OconstrX, NewobjX:
        analyseOconstr c, pc
      of AconstrX:
        analyseArrayConstr c, pc
      of TupconstrX:
        analyseTupConstr c, pc
      of CastX, ConvX, HconvX:
        inc pc
        skip pc # skips type
        analyseExpr c, pc
        skipParRi pc
      else:
        inc nested
        inc pc
    if nested == 0: break

proc analyseCallArgs(c: var Context; n: var Cursor) =
  let callCursor = n
  var fnType = skipProcTypeToParams(getType(c.typeCache, n))
  analyseExpr c, n # the `fn` itself could be a proc pointer we must ensure was initialized
  assert fnType.isParamsTag
  inc fnType
  var paramMap = initTable[SymId, int]() # param to position
  while n.kind != ParRi:
    let previousFormalParam = fnType
    assert fnType.kind != ParRi
    let param = takeLocal(fnType, SkipFinalParRi)
    paramMap[param.name.symId] = paramMap.len+1
    let pk = param.typ.typeKind
    if pk == OutT:
      if n.kind == Symbol:
        # is now initialized:
        c.writesTo.add n.symId
    elif pk == VarargsT:
      # do not advance formal parameter:
      fnType = previousFormalParam
    checkNilMatch c, n, param.typ
    analyseExpr c, n
  while fnType.kind != ParRi: skip fnType
  inc fnType # skip ParRi
  # skip return type:
  skip fnType
  # now we have the pragmas:
  let req = extractPragma(fnType, RequiresP)
  if not cursorIsNil(req):
    # ... analyse that the input parameters match the requirements
    let res = checkReq(c, paramMap, req, callCursor)
    when isMainModule:
      # XXX Enable when it works
      if res != Proven:
        error "contract violation: ", req

proc analyseCall(c: var Context; n: var Cursor) =
  inc n # skip call instruction
  analyseCallArgs(c, n)
  skipParRi n

#[
We use the control flow graph for a structured traversal over the basic blocks (bb):
We start with the proc entry. We follow the bb until we arrive at a `goto` instruction
or at a `ite` instruction which know has two goto instructions.
We know a `goto` enters a different bb. We add this bb to a worklist. We can process
this bb once all its predecessors have been processed. We keep track of that by a
lookup table that counts the indegree of each bb.
]#
type
  Continuation = object
    thenPart, elsePart: BasicBlockIdx
    conditionalFacts: int

const
  NoBasicBlock = BasicBlockIdx(-1)
  BasicBlockReturn = BasicBlockIdx(-2)
  LoopBack = BasicBlockIdx(-3)

proc `==`*(a, b: BasicBlockIdx): bool {.borrow.}
proc `<=`*(a, b: BasicBlockIdx): bool {.borrow.}
proc `<`*(a, b: BasicBlockIdx): bool {.borrow.}

proc toBasicBlock*(c: Context; pc: Cursor): BasicBlockIdx {.inline.} =
  result = BasicBlockIdx(cursorToPosition(c.startInstr, pc))

proc computeBasicBlocks*(c: TokenBuf; start = 0; last = -1): Table[BasicBlockIdx, BasicBlock] =
  result = initTable[BasicBlockIdx, BasicBlock]()
  let last = if last < 0: c.len-1 else: min(last, c.len-1)
  result[BasicBlockIdx(start)] = BasicBlock(indegree: 0, indegreeFacts: createFacts(), writesTo: @[])

  # First, eliminate dead code
  let reachable = eliminateDeadInstructions(c, start, last)

  # Now compute basic blocks considering only reachable instructions
  for i in start..last:
    if reachable[i - start] and c[i].kind == GotoInstr:
      let diff = c[i].getInt28
      if diff > 0 and i+diff <= last and reachable[i+diff - start]:
        let idx = BasicBlockIdx(i+diff)
        result.mgetOrPut(idx, BasicBlock(indegree: 0, indegreeFacts: createFacts(), writesTo: @[])).indegree += 1

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
        analyseExpr c, pc
    else:
      analyseExpr c, pc
      analyseExpr c, pc
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
  elif pc.exprKind == NilX:
    fact.b = VarId(0)
    fact.c = fact.c + createXint(0'i32)
    result = true
    skip pc
  else:
    analyseExpr c, pc

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
    analyseExpr c, pc
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
  elif r.exprKind == NilX:
    result.a = VarId(0)
    skip r
  else:
    analyseExpr c, pc
    return result
  if r.exprKind == NilX:
    wasEquality = false
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

proc cannotBeNil(c: var Context; n: Cursor): bool {.inline.} =
  let t = getType(c.typeCache, n)
  result = markedAs(t, NotnilU)

proc analyseAsgn(c: var Context; pc: var Cursor) =
  inc pc # skip asgn instruction
  let expected = getType(c.typeCache, pc)
  if pc.kind == Symbol:
    let symId = pc.symId
    let x = getLocalInfo(c.typeCache, symId)
    if x.kind in {LetY, GletY, TletY}:
      if symId in c.directlyInitialized or symId in c.writesTo:
        c.buildErr pc.info, "invalid reassignment to `let` variable"

    var fact = query(getVarId(c, symId), InvalidVarId, createXint(0'i32))
    c.writesTo.add symId
    # after `x = 4` we know two facts: `x >= 4` and `x <= 4`
    inc pc
    checkNilMatch c, pc, expected
    let rhs = pc
    if rightHandSide(c, pc, fact):
      if fact.a == fact.b:
        variableChangedByDiff(c.facts, fact.a, fact.c)
      else:
        invalidateFactsAbout(c.facts, fact.a)
        addAsgnFact c, fact
    else:
      invalidateFactsAbout(c.facts, fact.a)

    if (rhs.exprKind == NewobjX and c.procCanRaise) or cannotBeNil(c, rhs):
      # we know: x != nil after the assignment:
      c.facts.add isNotNil(fact.a)
  else:
    analyseExpr c, pc
    checkNilMatch c, pc, expected
    analyseExpr c, pc
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
    #echo "Instruction is ", toString(pc, false)
    case pc.kind
    of GotoInstr:
      # Every goto intruction leaves the basic block.
      let diff = pc.getInt28
      if diff < 0:
        return Continuation(thenPart: LoopBack, elsePart: NoBasicBlock)
      else:
        # ordinary goto, simply follow it:
        return Continuation(thenPart: toBasicBlock(c, pc +! diff), elsePart: NoBasicBlock)
    of ParRi:
      if nested == 0:
        bug "unpaired ')'"
      dec nested
      inc pc
    of Symbol:
      inc pc
    of SymbolDef:
      bug "symbol definition in single path"
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
          let skipInitCheck = hasPragma(pc, NoinitP)
          skip pc # pragmas
          c.typeCache.registerLocal(name, cast[SymKind](kind), pc)
          skip pc # type
          if pc.kind != DotToken or skipInitCheck:
            c.directlyInitialized.incl name
          analyseExpr c, pc
          skipParRi pc
        of NoStmt:
          if pc.cfKind == ForbindF:
            inc pc
            analyseCall c, pc
            skip pc
            skipParRi pc
          else:
            case pc.exprKind
            of PragmaxX:
              inc pc
              skip pc # pragmas
              inc nested
            of DestroyX, CopyX, WasMovedX, SinkhX, TraceX:
              inc pc
              analyseExpr c, pc
              # don't assume arity here
              while pc.kind != ParRi:
                analyseExpr c, pc
              skipParRi pc
            else:
              bug "unknown statement: " & toString(pc, false)
        of DiscardS, YldS:
          inc pc
          analyseExpr c, pc
          skipParRi pc
        of CallKindsS:
          analyseCall(c, pc)
        of EmitS, InclS, ExclS:
          # not of interest for contract analysis:
          skip pc
        of IfS, WhenS, WhileS, ForS, CaseS, TryS, RaiseS, ExportS,
           IncludeS, ImportS, FromimportS, ImportExceptS, CommentS, PragmasS,
           ImportasS, ExportexceptS, BindS, MixinS, UsingS,
           UnpackDeclS, StaticstmtS, AsmS, DeferS:
          bug "statement not eliminated: " & $pc.stmtKind
        of ProcS, FuncS, IteratorS, ConverterS, MethodS, MacroS, TemplateS, TypeS:
          # declarative junk we don't care about:
          skip pc
  return Continuation(thenPart: BasicBlockReturn, elsePart: NoBasicBlock)

proc decAndTest(x: var int): bool {.inline.} =
  dec x
  result = x == 0

proc filter(dest: var seq[SymId]; src: seq[SymId]) =
  var i = 0
  while i < dest.len:
    if dest[i] notin src:
      dest.del(i)
    else:
      inc i

proc takeFacts(c: var Context; bb: var BasicBlock; conditionalFacts: int; negate: bool) =
  #let start = bb.indegreeFacts.len
  if bb.touched == 0:
    bb.writesTo = c.writesTo
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
    bb.writesTo.filter(c.writesTo)
  inc bb.touched
  #c.facts.shrink c.facts.len - conditionalFacts

proc pushFacts(c: var Context; bb: var BasicBlock) =
  #echo "PUSHING FACTS ", bb.indegreeFacts.len
  c.facts.shrink 0
  c.writesTo = bb.writesTo
  for i in 0 ..< bb.indegreeFacts.len:
    c.facts.add bb.indegreeFacts[i]

proc checkContracts(c: var Context; n: Cursor) =
  c.cf = toControlflow(n)
  c.facts = createFacts()
  freeze c.cf
  #echo "CF IS ", codeListing(c.cf)

  c.startInstr = readonlyCursorAt(c.cf, 0)
  c.procCanRaise = false
  c.typeCache.openScope()
  var body = c.startInstr
  if body.stmtKind in {ProcS, FuncS, IteratorS, ConverterS, MethodS, MacroS}:
    inc body
    for i in 0 ..< BodyPos:
      if i == ProcPragmasPos:
        c.procCanRaise = hasPragma(body, RaisesP)
      skip body

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
  c.typeCache.closeScope()

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
        if nested == 0: break
  else:
    skip n # body
  skipParRi n # proc decl end

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
     CallKindsS, GvarS, TvarS, VarS, ConstS, ResultS,
     GletS, TletS, LetS, CursorS, BlockS, EmitS, AsgnS, ScopeS,
     BreakS, ContinueS, RetS, InclS, ExclS, DiscardS, AssumeS, AssertS, NoStmt:
    c.toplevelStmts.takeTree n

proc analyzeContracts*(input: var TokenBuf): TokenBuf =
  #let oldInfos = prepare(input)
  var c = Context(typeCache: createTypeCache())
  c.typeCache.openScope()
  var n = beginRead(input)
  traverseToplevel c, n
  for r in c.routines:
    c.directlyInitialized.clear()
    checkContracts(c, r)
  var nt = beginRead c.toplevelStmts
  checkContracts(c, nt)

  endRead input
  #restore(input, oldInfos)
  c.typeCache.closeScope()
  result = ensureMove c.errors

when isMainModule:
  import std / [syncio, os]
  proc main(infile: string) =
    var input = parseFromFile(infile)
    discard analyzeContracts(input)
    #echo toString(outp, false)

  main(paramStr(1))
