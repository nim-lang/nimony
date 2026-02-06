#       Nimony
# (c) Copyright 2025 Andreas Rumpf
#
# See the file "license.txt", included in this
# distribution, for details about the copyright.

##[
Contract analysis using NJVL (No-Jump Versioned Locations) IR.

Tries to prove or disprove `.requires` and `.ensures` annotations.
Uses the structured ite/loop constructs from NJVL instead of goto-based
control flow graphs.

The analysis is performed on NJVL IR which has:
- `(ite cond then else)` for branching
- `(loop pre cond body)` for loops
- `(store value dest)` for assignments
- `(v symId version)` for versioned variables
- `(join symId newV old1 old2)` for merge points

In order to not be too annoying in the case of a contract violation, the
compiler emits a warning (that can be suppressed or turned into an error).
]##

import std / [assertions, tables, sets, strutils]

include nifprelude

import ".." / models / tags
import ".." / lib / symparser
import ".." / njvl / [njvl_model, vl]
import nimony_model, programs, decls, typenav, sembasics, reporters,
       renderer, typeprops, inferle, xints

type
  NjvlContext = object
    facts: Facts           # From inferle.nim - tracks le/notnil facts
    typeCache: TypeCache
    directlyInitialized: HashSet[SymId]
    writesTo: seq[SymId]
    paramBaseNames: HashSet[string]
    errors: TokenBuf
    procCanRaise: bool
    moduleSuffix: string

proc buildErr(c: var NjvlContext; info: PackedLineInfo; msg: string) =
  when defined(debug):
    writeStackTrace()
    echo infoToStr(info) & " Error: " & msg
    quit msg
  c.errors.buildTree ErrT, info:
    c.errors.addDotToken()
    c.errors.add strToken(pool.strings.getOrIncl(msg), info)

# Forward declarations
proc traverseStmt(c: var NjvlContext; n: var Cursor)
proc traverseExpr(c: var NjvlContext; pc: var Cursor)
proc analyseCall(c: var NjvlContext; n: var Cursor)

template getVarId(c: var NjvlContext; symId: SymId): VarId = VarId(symId)

proc isParamSym(symId: SymId): bool =
  let res = tryLoadSym(symId)
  result = res.status == LacksNothing and
    (res.decl.symKind == ParamY or res.decl.substructureKind == ParamU)

proc baseParamName(symName: string): string =
  var name = symName
  let moduleName = extractModule(symName)
  if moduleName.len > 0:
    let cut = name.len - moduleName.len - 1
    if cut >= 0:
      name = name[0 ..< cut]
  let lastDot = name.rfind('.')
  if lastDot >= 0:
    var onlyDigits = true
    for i in lastDot + 1 ..< name.len:
      if name[i] notin {'0'..'9'}:
        onlyDigits = false
        break
    if onlyDigits:
      name = name[0 ..< lastDot]
  result = name

proc isXelimTemp(symName: string; moduleSuffix: string): bool =
  symName.startsWith("`x.") and symName.endsWith("." & moduleSuffix)

# --- Fact extraction from conditions ---

proc rightHandSide(c: var NjvlContext; pc: var Cursor; fact: var LeXplusC): bool =
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
        traverseExpr c, pc
    else:
      traverseExpr c, pc
      traverseExpr c, pc
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
    traverseExpr c, pc

proc translateCond(c: var NjvlContext; pc: var Cursor; wasEquality: var bool): LeXplusC =
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
    traverseExpr c, pc
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
    traverseExpr c, pc
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

proc analyseCondition(c: var NjvlContext; pc: var Cursor): int =
  ## Returns number of facts added
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

# --- Not-nil checking ---

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

proc analysableRoot(c: var NjvlContext; n: Cursor): SymId =
  var n = n
  while true:
    case n.exprKind
    of DotX, TupatX, ArrAtX, HderefX:
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
      result = NoSymId
  else:
    result = NoSymId

proc wantNotNil(c: var NjvlContext; n: Cursor) =
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

proc checkNilMatch(c: var NjvlContext; n: Cursor; expected: Cursor) =
  if markedAs(expected, NotnilU):
    wantNotNil c, n

proc wantNotNilDeref(c: var NjvlContext; n: Cursor) =
  let e = getType(c.typeCache, n)
  if markedAs(e, NilU):
    wantNotNil c, n

# --- .requires checking ---

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

proc argAt(call: Cursor; pos: int): Cursor =
  result = call
  inc result
  for i in 0 ..< pos: skip result

proc mapSymbol(c: var NjvlContext; paramMap: Table[SymId, int]; call: Cursor; symId: SymId): VarId =
  result = VarId(0)
  let pos = paramMap.getOrDefault(symId)
  if pos > 0:
    let arg = call.argAt(pos)
    if arg.kind == Symbol:
      result = getVarId(c, arg.symId)

proc compileCmp(c: var NjvlContext; paramMap: Table[SymId, int]; req, call: Cursor): LeXplusC =
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

proc checkReq(c: var NjvlContext; paramMap: Table[SymId, int]; req, call: Cursor): ProofRes =
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
    var r = req
    inc r
    skip r # skip type
    let cm = compileCmp(c, paramMap, r, call)
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

# --- Expression analysis ---

proc analyseOconstr(c: var NjvlContext; n: var Cursor) =
  inc n
  let objType = n
  skip n # type
  while n.kind != ParRi:
    assert n.substructureKind == KvU
    inc n
    assert n.kind == Symbol
    let expected = lookupField(c.typeCache, objType, n.symId)
    assert not cursorIsNil(expected), "could not lookup type for " & pool.syms[n.symId]
    skip n # field name
    checkNilMatch c, n, expected
    skip n # value
    if n.kind != ParRi:
      # optional inheritance
      skip n
    skipParRi n
  skipParRi n

proc analyseArrayConstr(c: var NjvlContext; n: var Cursor) =
  inc n
  let expected = n.firstSon # element type of the array
  skip n # type
  while n.kind != ParRi:
    checkNilMatch c, n, expected
    skip n
  skipParRi n

proc analyseTupConstr(c: var NjvlContext; n: var Cursor) =
  inc n
  var expected = n.firstSon # type of the first field
  skip n # type
  while n.kind != ParRi:
    assert expected.kind != ParRi
    checkNilMatch c, n, getTupleFieldType(expected)
    skip n
    skip expected # type of the next field
  skipParRi n

proc traverseExpr(c: var NjvlContext; pc: var Cursor) =
  var nested = 0
  while true:
    case pc.kind
    of Symbol:
      let symId = pc.symId
      let x = getLocalInfo(c.typeCache, symId)
      if x.kind in {VarY, LetY, CursorY}:
        if symId notin c.directlyInitialized and symId notin c.writesTo:
          let symName = pool.syms[symId]
          if isXelimTemp(symName, c.moduleSuffix):
            c.directlyInitialized.incl symId
          elif isParamSym(symId):
            c.directlyInitialized.incl symId
          elif baseParamName(symName) in c.paramBaseNames:
            c.directlyInitialized.incl symId
          else:
            buildErr(c, pc.info, "cannot prove that " & pool.syms[symId] & " has been initialized")
            c.writesTo.add symId
      inc pc
    of SymbolDef:
      bug "symbol definition in expression"
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
        traverseExpr c, pc # object
        skip pc # field name
        if pc.kind != ParRi: skip pc # inheritance depth
        skipParRi pc
      of DerefX:
        inc pc
        wantNotNilDeref c, pc
        traverseExpr c, pc
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
        traverseExpr c, pc
        skipParRi pc
      else:
        inc nested
        inc pc
    if nested == 0: break

proc analyseCallArgs(c: var NjvlContext; n: var Cursor) =
  let callCursor = n
  var fnType = skipProcTypeToParams(getType(c.typeCache, n))
  traverseExpr c, n # the `fn` itself
  assert fnType.isParamsTag
  inc fnType
  var paramMap = initTable[SymId, int]()
  while n.kind != ParRi:
    let previousFormalParam = fnType
    assert fnType.kind != ParRi
    let param = takeLocal(fnType, SkipFinalParRi)
    paramMap[param.name.symId] = paramMap.len+1
    let pk = param.typ.typeKind
    if pk == OutT:
      if n.kind == Symbol:
        c.writesTo.add n.symId
    elif pk == VarargsT:
      fnType = previousFormalParam
    checkNilMatch c, n, param.typ
    traverseExpr c, n
  while fnType.kind != ParRi: skip fnType
  inc fnType # skip ParRi
  skip fnType # skip return type
  # now we have the pragmas:
  let req = extractPragma(fnType, RequiresP)
  if not cursorIsNil(req):
    let res = checkReq(c, paramMap, req, callCursor)
    when isMainModule:
      if res != Proven:
        error "contract violation: ", req

proc analyseCall(c: var NjvlContext; n: var Cursor) =
  inc n # skip call instruction
  analyseCallArgs(c, n)
  skipParRi n

# --- Assignment fact tracking ---

proc addAsgnFact(c: var NjvlContext; fact: LeXplusC) =
  if fact.isValid:
    c.facts.add fact
    c.facts.add fact.geXplusC

proc cannotBeNil(c: var NjvlContext; n: Cursor): bool {.inline.} =
  let t = getType(c.typeCache, n)
  result = markedAs(t, NotnilU)

# --- NJVL-specific traversal ---

proc traverseStore(c: var NjvlContext; n: var Cursor) =
  ## Handle (store value dest) - note reversed order from asgn
  inc n # skip store tag

  # First analyze the value (source)
  let valueStart = n
  traverseExpr c, n

  # Now handle the destination
  if n.kind == Symbol:
    let symId = n.symId
    let x = getLocalInfo(c.typeCache, symId)
    if x.kind in {LetY, GletY, TletY}:
      if symId in c.directlyInitialized or symId in c.writesTo:
        c.buildErr n.info, "invalid reassignment to `let` variable"

    var fact = query(getVarId(c, symId), InvalidVarId, createXint(0'i32))
    c.writesTo.add symId

    # Check for not-nil type match
    let expected = getType(c.typeCache, n)
    checkNilMatch c, valueStart, expected

    # Try to extract facts from the value
    var valueForFact = valueStart
    if rightHandSide(c, valueForFact, fact):
      if fact.a == fact.b:
        variableChangedByDiff(c.facts, fact.a, fact.c)
      else:
        invalidateFactsAbout(c.facts, fact.a)
        addAsgnFact c, fact
    else:
      invalidateFactsAbout(c.facts, fact.a)

    # Check if the rhs is known to be not nil
    if (valueStart.exprKind == NewobjX and c.procCanRaise) or cannotBeNil(c, valueStart):
      c.facts.add isNotNil(fact.a)

    inc n # skip the symbol
  else:
    traverseExpr c, n

  skipParRi n

proc traverseIte(c: var NjvlContext; n: var Cursor) =
  ## Handle (ite cond then else [join])
  inc n # skip ite/itec tag

  # Analyze condition and extract facts
  let savedFacts = save(c.facts)
  let condFacts = analyseCondition(c, n)

  # Then branch - has positive condition facts
  c.typeCache.openScope()
  traverseStmt c, n
  c.typeCache.closeScope()
  let thenFacts = c.facts
  let thenWritesTo = c.writesTo

  # Restore facts and negate for else branch
  restore(c.facts, savedFacts)
  if condFacts > 0:
    # Re-add condition facts but negated
    var tempCursor = n
    # We already moved past the condition, so we need to work with what we have
    # For now, just analyze else without negated facts (conservative)
    discard

  # Else branch
  c.typeCache.openScope()
  if n.kind == DotToken:
    inc n # empty else
  else:
    traverseStmt c, n
  c.typeCache.closeScope()

  # Merge facts from both branches
  # Use conservative approach: only keep facts that hold in both branches
  c.facts = merge(thenFacts, 0, c.facts, false)

  # Merge writesTo: keep only what's written in both branches
  var mergedWritesTo: seq[SymId] = @[]
  for s in thenWritesTo:
    if s in c.writesTo:
      mergedWritesTo.add s
  c.writesTo = mergedWritesTo

  # Skip optional join information
  if n.kind == ParLe and n.stmtKind == StmtsS:
    skip n

  skipParRi n

proc traverseLoop(c: var NjvlContext; n: var Cursor) =
  ## Handle (loop pre cond body)
  inc n # skip loop tag

  # Pre-condition statements
  c.typeCache.openScope()
  traverseStmt c, n

  # Analyze loop condition
  let savedFacts = save(c.facts)
  var condCursor = n
  var wasEquality = false
  let condFact = translateCond(c, condCursor, wasEquality)
  skip n # skip condition expression

  # Conservative approach for loops:
  # We don't know how many times the loop runs, so we can't trust facts
  # about variables that might be modified in the loop body.
  # For simplicity, we analyze the body but reset facts afterwards.

  # Loop body
  traverseStmt c, n

  c.typeCache.closeScope()

  # After loop, we know the condition is false (if we exited normally)
  restore(c.facts, savedFacts)
  if condFact.isValid:
    var negated = condFact
    negateFact(negated)
    c.facts.add negated

  skipParRi n

proc traverseLocal(c: var NjvlContext; n: var Cursor) =
  let kind = n.symKind
  let isParamDecl = kind == ParamY or n.substructureKind == ParamU
  inc n
  let name = n.symId
  skip n # name
  skip n # export marker
  let skipInitCheck = hasPragma(n, NoinitP)
  skip n # pragmas
  c.typeCache.registerLocal(name, cast[SymKind](kind), n)
  skip n # type
  if isParamDecl:
    c.paramBaseNames.incl baseParamName(pool.syms[name])
  elif isXelimTemp(pool.syms[name], c.moduleSuffix):
    c.directlyInitialized.incl name
  if isParamDecl or n.kind != DotToken or skipInitCheck:
    c.directlyInitialized.incl name
  traverseExpr c, n
  skipParRi n

proc traverseAssume(c: var NjvlContext; n: var Cursor) =
  inc n
  var wasEquality = false
  let fact = translateCond(c, n, wasEquality)
  if not fact.isValid:
    error "invalid assume: ", n
  else:
    c.facts.add fact
    if wasEquality:
      c.facts.add fact.geXplusC
  skipParRi n

proc traverseAssert(c: var NjvlContext; n: var Cursor) =
  let orig = n
  inc n
  var report = false
  var shouldError = false
  if n.pragmaKind == ReportP:
    report = true
    inc n
    skipParRi n
  if n.pragmaKind == ErrorP:
    shouldError = true
    inc n
    skipParRi n

  var wasEquality = false
  let fact = translateCond(c, n, wasEquality)
  if not fact.isValid:
    error "invalid assert: ", orig
  elif implies(c.facts, fact):
    if shouldError:
      error "contract violation: ", orig
    elif wasEquality:
      if implies(c.facts, fact.geXplusC):
        if report: echo "OK ", fact
      else:
        if shouldError:
          if report: echo "OK (could indeed not prove) ", fact
        else:
          error "contract violation: ", orig
    else:
      if report: echo "OK ", fact
  else:
    if shouldError:
      if report: echo "OK (could indeed not prove) ", fact
    else:
      error "contract violation: ", orig
  skipParRi n

proc registerProcParams(c: var NjvlContext; decl: Cursor) =
  let r = asRoutine(decl, SkipExclBody)
  c.typeCache.registerParams(r.name.symId, decl, r.params)
  var p = r.params
  if p.kind == ParLe:
    inc p
    while p.kind != ParRi:
      let param = takeLocal(p, SkipFinalParRi)
      c.paramBaseNames.incl baseParamName(pool.syms[param.name.symId])
      if param.typ.typeKind != OutT:
        c.directlyInitialized.incl param.name.symId

proc traverseStmt(c: var NjvlContext; n: var Cursor) =
  case n.njvlKind
  of IteV, ItecV:
    traverseIte c, n
  of LoopV:
    traverseLoop c, n
  of StoreV:
    traverseStore c, n
  of AssumeV:
    traverseAssume c, n
  of AssertV:
    traverseAssert c, n
  of CfvarV:
    # Control flow variable declaration - just skip
    inc n
    skip n # symdef
    skipParRi n
  of JtrueV:
    # Jump hint - skip
    skip n
  of KillV:
    # Variable going out of scope - skip
    skip n
  of UnknownV:
    # Unknown instruction - skip value, analyze rest
    inc n
    skip n # the unknown location
    skipParRi n
  of ContinueV:
    # Continue in loop - skip
    skip n
  of VV:
    # Versioned variable reference - should not appear as statement
    skip n
  of NoVTag:
    case n.stmtKind
    of StmtsS, ScopeS, BlockS:
      inc n
      while n.kind != ParRi:
        traverseStmt c, n
      skipParRi n
    of LocalDecls:
      traverseLocal c, n
    of ProcS, FuncS, IteratorS, ConverterS, MethodS, MacroS:
      # Nested routine - analyze separately
      c.typeCache.openScope()
      let orig = n
      let r = takeRoutine(n, SkipExclBody)
      skip n # effects
      if not isGeneric(r):
        var nested = 0
        while true:
          let sk = n.stmtKind
          if sk in {ProcS, FuncS, IteratorS, ConverterS, MethodS}:
            # Don't recurse into nested procs here
            skip n
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
      c.typeCache.closeScope()
    of TemplateS, TypeS, CommentS, PragmasS:
      skip n
    of RetS:
      inc n
      if n.kind != ParRi and n.kind != DotToken:
        traverseExpr c, n
      while n.kind != ParRi:
        inc n
      skipParRi n
    of CallKindsS:
      analyseCall c, n
    of DiscardS, YldS:
      inc n
      traverseExpr c, n
      skipParRi n
    of EmitS, InclS, ExclS:
      skip n
    of NoStmt:
      # Could be a call or expression
      if n.exprKind in CallKinds:
        analyseCall c, n
      else:
        traverseExpr c, n
    else:
      # Unknown statement - try to traverse children
      inc n
      var nested = 1
      while nested > 0:
        case n.kind
        of ParLe:
          inc nested
          inc n
        of ParRi:
          dec nested
          inc n
        else:
          inc n

proc traverseProc(c: var NjvlContext; n: Cursor) =
  var n = n
  c.facts = createFacts()
  c.procCanRaise = false
  c.directlyInitialized.clear()
  c.writesTo.setLen(0)
  c.paramBaseNames.clear()
  c.typeCache.openScope()

  registerProcParams(c, n)

  var body = n
  if body.stmtKind in {ProcS, FuncS, IteratorS, ConverterS, MethodS, MacroS}:
    inc body
    for i in 0 ..< BodyPos:
      if i == ProcPragmasPos:
        c.procCanRaise = hasPragma(body, RaisesP)
      skip body

  # Analyze body
  traverseStmt c, body

  c.typeCache.closeScope()

proc traverseToplevel(c: var NjvlContext; n: var Cursor) =
  case n.stmtKind
  of StmtsS:
    inc n
    while n.kind != ParRi:
      traverseToplevel c, n
    skipParRi n
  of ProcS, FuncS, IteratorS, ConverterS, MethodS:
    let orig = n
    let r = takeRoutine(n, SkipExclBody)
    skip n # effects
    if not isGeneric(r):
      traverseProc c, orig
      # Skip the body we already processed
      var nested = 0
      while true:
        let sk = n.stmtKind
        if sk in {ProcS, FuncS, IteratorS, ConverterS, MethodS}:
          traverseToplevel c, n
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
    skipParRi n
  of MacroS, TemplateS, TypeS, CommentS, PragmasS,
     ImportasS, ExportexceptS, BindS, MixinS, UsingS,
     ExportS,
     IncludeS, ImportS, FromimportS, ImportExceptS:
    skip n
  else:
    # Toplevel statements - analyze them
    traverseStmt c, n

proc analyzeContractsNjvl*(input: var TokenBuf; moduleSuffix: string): TokenBuf =
  ## Main entry point: converts input to NJVL and analyzes contracts
  var n = beginRead(input)

  # Convert to NJVL first
  var njvlBuf = toNjvl(n, moduleSuffix)
  endRead input

  # Now analyze the NJVL IR
  var c = NjvlContext(
    typeCache: createTypeCache(),
    moduleSuffix: moduleSuffix
  )
  c.typeCache.openScope()

  var njvl = beginRead(njvlBuf)
  traverseToplevel c, njvl
  endRead njvlBuf

  c.typeCache.closeScope()
  result = ensureMove c.errors

when isMainModule:
  import std / [syncio, os]
  proc main(infile: string) =
    var input = parseFromFile(infile)
    discard analyzeContractsNjvl(input, "main")

  main(paramStr(1))
