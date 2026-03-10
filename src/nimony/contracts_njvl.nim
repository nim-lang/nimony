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
  renderer, typeprops, inferle, xints, builtintypes
import writesets

type
  NjvlContext = object
    facts: Facts           # From inferle.nim - tracks le/notnil facts
    typeCache: TypeCache
    directlyInitialized: seq[HashSet[SymId]]
    writesTo: IteTracker[SymId]
    errors: TokenBuf
    procCanRaise: bool
    moduleSuffix: string
    nestedProcs: int
    knownCfVars: HashSet[SymId]
    inlineVars: Table[SymId, Cursor] # var -> to its init expression
    knownTrueCfVars: IteTracker[SymId]  # cfvars set to true by (jtrue ...)
    writeSets: WriteSets               # per-ite write-set implications (TokenBuf-based)
    resultSym: SymId                   # symId of the `result` local for the current proc, or NoSymId
    activeBorrows: seq[BorrowInfo]

  BorrowPath = seq[SymId]  ## root :: field1 :: field2 :: ...
  BorrowInfo = object
    path: BorrowPath
    borrower: SymId        ## variable holding the borrow; upon `(kill borrower)` the borrow ends
    info: PackedLineInfo

proc buildErr(c: var NjvlContext; info: PackedLineInfo; msg: string) =
  when defined(debug):
    writeStackTrace()
    echo infoToStr(info) & " Error: " & msg
    quit msg
  c.errors.buildTree ErrT, info:
    c.errors.addDotToken()
    c.errors.add strToken(pool.strings.getOrIncl(msg), info)

proc contractViolation(c: var NjvlContext; orig: Cursor; fact: LeXplusC; report: bool) =
  if report:
    echo "known facts in this context: "
    for i in 0 ..< c.facts.len:
      echo c.facts[i]
    echo "canonical fact: ", fact
  error "contract violation: ", orig

# Forward declarations
proc traverseStmt(c: var NjvlContext; n: var Cursor)
proc traverseExpr(c: var NjvlContext; pc: var Cursor)
proc analyseCall(c: var NjvlContext; n: var Cursor)

proc extractSymId(n: Cursor): SymId {.inline.} =
  var n = n
  if n.exprKind in {HaddrX, HderefX}: inc n

  if n.kind == Symbol:
    result = n.symId
  elif n.kind == ParLe and n.tagEnum == VTagId:
    result = n.firstSon.symId
  else:
    result = NoSymId

proc extractSymIdForStore(n: Cursor): SymId =
  # idea both (etupat result.0 +0) and (etupat result.0 +1) create
  # a full store to `result.0`.
  var n = n
  if n.njvlKind == EtupatV:
    inc n
  result = extractSymId(n)

proc skipSymbol(r: var Cursor): SymId {.inline.} =
  ## Consume a bare Symbol or (v sym version) node and return its SymId.
  ## Returns NoSymId (without advancing) if r is neither.
  result = extractSymId(r)
  if result != NoSymId:
    skip r

# --- Borrow checking ---

proc extractBorrowPath(c: NjvlContext; n: Cursor; result: var BorrowPath) =
  ## Extract a path (root :: field1 :: field2 :: ...) from an expression,
  ## expanding inline variables.
  if n.kind == ParLe:
    let ek = n.exprKind
    if ek in {DotX, DdotX}:
      var r = n
      inc r
      extractBorrowPath(c, r, result)
      skip r # skip object subtree
      if r.kind == Symbol:
        result.add r.symId
    elif ek in {HaddrX, HderefX, AddrX, DerefX}:
      var r = n
      inc r
      extractBorrowPath(c, r, result)
    elif ek in {TupatX, ArrAtX, AtX, PatX}:
      # Array/tuple access: recurse into container, don't distinguish indices
      var r = n
      inc r
      extractBorrowPath(c, r, result)
    elif ek in ConvKinds:
      var r = n
      inc r
      skip r # type
      extractBorrowPath(c, r, result)
    elif ek == BaseobjX:
      var r = n
      inc r
      skip r # type
      skip r # intlit
      extractBorrowPath(c, r, result)
    elif n.njvlKind == EtupatV:
      var r = n
      inc r
      extractBorrowPath(c, r, result)
    elif n.njvlKind == VV:
      let s = n.firstSon.symId
      if s in c.inlineVars:
        extractBorrowPath(c, c.inlineVars[s], result)
      else:
        result.add s
  elif n.kind == Symbol:
    let s = n.symId
    if s in c.inlineVars:
      extractBorrowPath(c, c.inlineVars[s], result)
    else:
      result.add s

proc extractPath(c: NjvlContext; n: Cursor): BorrowPath =
  result = @[]
  extractBorrowPath(c, n, result)

type
  BorrowableCheck = enum
    IsBorrowable       ## simple path: symbols, dots, array access
    NotBorrowable      ## deref in middle of path or function call
    HasAddr            ## path contains explicit `addr` — unsafe escape hatch

proc checkBorrowable(n: Cursor; seenFieldAccess: bool): BorrowableCheck =
  ## Walk the path expression inward. `seenFieldAccess` tracks whether we've
  ## already seen a dot/array access outside the current position.
  ## A deref after a field access means "deref in the middle" → not borrowable.
  if n.kind == ParLe:
    let ek = n.exprKind
    if ek == AddrX:
      return HasAddr
    elif ek in {DotX, DdotX, TupatX, ArrAtX, AtX, PatX}:
      var r = n
      inc r
      return checkBorrowable(r, true)
    elif ek in {HderefX, DerefX}:
      if seenFieldAccess:
        return NotBorrowable # deref in middle of path
      var r = n
      inc r
      return checkBorrowable(r, false)
    elif ek == HaddrX:
      var r = n
      inc r
      return checkBorrowable(r, seenFieldAccess)
    elif ek in ConvKinds:
      var r = n
      inc r
      skip r # type
      return checkBorrowable(r, seenFieldAccess)
    elif ek == BaseobjX:
      var r = n
      inc r
      skip r # type
      skip r # intlit
      return checkBorrowable(r, seenFieldAccess)
    elif ek in CallKinds:
      return NotBorrowable # function call in path
    elif n.njvlKind in {VV, EtupatV}:
      return IsBorrowable
    else:
      return NotBorrowable
  elif n.kind == Symbol:
    return IsBorrowable
  else:
    return NotBorrowable

proc pathsOverlap(a, b: BorrowPath): bool =
  ## Two paths overlap if one is a prefix of the other (or they are equal).
  ## Disjoint siblings (e.g. a.b vs a.c) do not overlap.
  if a.len == 0 or b.len == 0: return false
  let minLen = min(a.len, b.len)
  for i in 0 ..< minLen:
    if a[i] != b[i]:
      return false
  result = true

proc checkBorrowConflict(c: var NjvlContext; mutPath: BorrowPath; info: PackedLineInfo) =
  for b in c.activeBorrows:
    if pathsOverlap(mutPath, b.path):
      buildErr c, info, "'" & pool.syms[mutPath[0]] & "' is borrowed and cannot be mutated"
      return

proc endBorrow(c: var NjvlContext; sym: SymId) =
  var i = 0
  while i < c.activeBorrows.len:
    if c.activeBorrows[i].borrower == sym:
      c.activeBorrows.delete(i)
    else:
      inc i

proc conditionCfvarForNot(c: NjvlContext; n: Cursor): SymId =
  ## If condition is `(not cfvar)`, return the cfvar's SymId. Else NoSymId.
  var r = n
  if r.exprKind == NotX:
    inc r
    result = extractSymId(r)
    if result != NoSymId and result notin c.knownCfVars:
      result = NoSymId
  else:
    result = NoSymId

proc conditionCfvarDirect(c: NjvlContext; n: Cursor): SymId =
  ## If condition is directly a cfvar (not negated), return its SymId. Else NoSymId.
  result = extractSymId(n)
  if result != NoSymId and result notin c.knownCfVars:
    result = NoSymId

proc cfCondKnownValue(c: NjvlContext; n: Cursor): int =
  ## Returns +1 if the condition is a cfvar known to be true,
  ## -1 if it is `(not cf)` where cf is known true, 0 otherwise.
  ## After vl.nim, cfvars appear as `(v sym N)` so we use extractSymId.
  let s = extractSymId(n)
  if s != NoSymId and s in c.knownTrueCfVars:
    result = 1
  elif n.exprKind == NotX:
    var inner = n
    inc inner  # skip NotX tag
    let s2 = extractSymId(inner)
    if s2 != NoSymId and s2 in c.knownTrueCfVars:
      result = -1
    else:
      result = 0
  else:
    result = 0

template getVarId(c: var NjvlContext; symId: SymId): VarId = VarId(symId)

# --- Fact extraction from conditions ---

proc rightHandSide(c: var NjvlContext; pc: var Cursor; fact: var LeXplusC): bool =
  result = false
  if pc.exprKind in {AddX, SubX}:
    inc pc
    skip pc # type
    let symId2 = skipSymbol(pc)
    if symId2 != NoSymId:
      fact.b = getVarId(c, symId2)
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
  elif (let symId2 = skipSymbol(pc); symId2 != NoSymId):
    fact.b = getVarId(c, symId2)
    result = true
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
    wasEquality = negations == 0  # negated equality is inequality, not equality
    inc r
    skip r # skip type
  else:
    # Check for a bare symbol/hderef/haddr (truthy ref check: `if x:` means `x != nil`)
    let sa = extractSymId(r)
    if sa != NoSymId:
      result = isNotNil(getVarId(c, sa))
      skip r
      while negations > 0:
        negateFact(result)
        dec negations
        skipParRi r
      pc = r
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
  elif (let sa = skipSymbol(r); sa != NoSymId):
    result.a = getVarId(c, sa)
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
  let s = extractSymId(n)
  if s != NoSymId:
    result = s
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
        # account for the fact that NJ already introduced tuples for the error handling:
        var n = n
        if n.exprKind == TupconstrX:
          inc n
          skip n # skip type
          if n.kind == Symbol and pool.syms[n.symId] == ("Success.0." & SystemModuleSuffix):
            inc n
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
    let sid = extractSymId(arg)
    if sid != NoSymId:
      result = getVarId(c, sid)

proc compileCmp(c: var NjvlContext; paramMap: Table[SymId, int]; req, call: Cursor): LeXplusC =
  var r = req
  var a = InvalidVarId
  var b = InvalidVarId
  var cnst = createXint(0'i32)
  let sid = extractSymId(r)
  if sid != NoSymId:
    a = mapSymbol(c, paramMap, call, sid)
    inc r
  let rid = extractSymId(r)
  if rid != NoSymId:
    b = mapSymbol(c, paramMap, call, rid)
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
    let cid = extractSymId(r)
    if cid != NoSymId:
      b = mapSymbol(c, paramMap, call, cid)
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

proc isDirectlyInitialized(c: var NjvlContext; symId: SymId): bool =
  for s in mitems c.directlyInitialized:
    if symId in s:
      return true
  return false

proc isEffectivelyInitialized(c: var NjvlContext; symId: SymId): bool =
  if isDirectlyInitialized(c, symId) or symId in c.writesTo:
    return true
  return false

proc traverseExpr(c: var NjvlContext; pc: var Cursor) =
  var nested = 0
  while true:
    case pc.kind
    of Symbol:
      let symId = pc.symId
      let x = getLocalInfo(c.typeCache, symId)
      if x.kind in {VarY, LetY, CursorY, ResultY}:
        if not isEffectivelyInitialized(c, symId):
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
      of DotX:
        inc pc
        traverseExpr c, pc # object
        skip pc # field name
        if pc.kind != ParRi: skip pc # inheritance depth
        skipParRi pc
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
  var fnPragmas = fnType
  skip fnPragmas # params
  skip fnPragmas # return type
  traverseExpr c, n # the `fn` itself
  assert fnType.isParamsTag
  inc fnType
  var paramMap = initTable[SymId, int]()
  # Collect argument paths for aliasing check
  var argPaths: seq[tuple[path: BorrowPath, isMut: bool, info: PackedLineInfo]] = @[]
  while n.kind != ParRi:
    let previousFormalParam = fnType
    assert fnType.kind != ParRi
    let param = takeLocal(fnType, SkipFinalParRi)
    paramMap[param.name.symId] = paramMap.len+1
    let pk = param.typ.typeKind
    # Save arg info before traverseExpr advances n
    let argInfo = n.info
    let isMut = n.exprKind == HaddrX
    # Validate borrowable path for haddr arguments (call-scoped borrows)
    if isMut:
      var inner = n
      inc inner # skip haddr tag
      let bc = checkBorrowable(inner, false)
      if bc == NotBorrowable:
        buildErr c, n.info, "cannot borrow from '" & asNimCode(inner) &
          "': path is not borrowable; use 'addr' to override or a temporary move"
    let argPath = extractPath(c, n)
    argPaths.add (argPath, isMut, argInfo)
    if pk == OutT:
      let s = extractSymId(n)
      if s != NoSymId:
        c.writesTo.add s
    elif pk == VarargsT:
      fnType = previousFormalParam
    checkNilMatch c, n, param.typ
    traverseExpr c, n
  # Check aliasing: a mutable argument must not overlap with any other argument
  for i in 0 ..< argPaths.len:
    if argPaths[i].isMut and argPaths[i].path.len > 0:
      for j in 0 ..< argPaths.len:
        if i != j and argPaths[j].path.len > 0:
          if pathsOverlap(argPaths[i].path, argPaths[j].path):
            buildErr c, argPaths[j].info, "argument aliases with mutable parameter"
            break
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

  # Check borrow conflicts for the destination
  let destMutPath = extractPath(c, n)
  if destMutPath.len > 0:
    checkBorrowConflict(c, destMutPath, n.info)

  # Now handle the destination (Symbol or NJVL versioned variable (v symId version))
  let destSymId = extractSymIdForStore(n)
  if destSymId != NoSymId:
    let symId = destSymId
    let x = getLocalInfo(c.typeCache, symId)
    if x.kind in {LetY, GletY, TletY}:
      if isDirectlyInitialized(c, symId) or symId in c.writesTo:
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

    skip n
  else:
    traverseExpr c, n

  skipParRi n

proc traverseIte(c: var NjvlContext; n: var Cursor) =
  ## Handle (ite cond then else [join])
  inc n # skip ite/itec tag

  # Fast path: if the condition's truth value is known from cfvar state,
  # only traverse the live branch and skip the dead one.
  let knownVal = cfCondKnownValue(c, n)
  if knownVal == 1:
    # condition is a cfvar known to be true: only then-branch runs
    skip n  # skip condition
    traverseStmt c, n  # then branch
    skip n  # skip else
    if n.kind == ParLe and n.stmtKind == StmtsS: skip n  # skip join
    skipParRi n
    return
  elif knownVal == -1:
    # condition is (not cf) with cf known true: only else-branch runs
    skip n  # skip condition
    skip n  # skip then branch
    if n.kind != DotToken: traverseStmt c, n else: inc n  # else
    if n.kind == ParLe and n.stmtKind == StmtsS: skip n  # skip join
    skipParRi n
    return

  # Condition may be (not cfvar) or directly a cfvar - capture before analyseCondition consumes it
  let condCf = conditionCfvarForNot(c, n)
  let condDirectCf = conditionCfvarDirect(c, n)

  # Analyze condition and extract facts
  let savedFacts = save(c.facts)
  let savedBorrowsLen = c.activeBorrows.len
  var writesSp = c.writesTo.split()
  var cfSp = c.knownTrueCfVars.split()
  let condFacts = analyseCondition(c, n)

  # Copy condition facts for else-branch negation (only single fact can be negated)
  var condFactsList: seq[LeXplusC] = @[]
  if condFacts == 1:
    condFactsList.add c.facts[c.facts.len - 1]

  # Then branch
  traverseStmt c, n
  let thenFacts = c.facts
  c.writesTo.thenDone(writesSp)
  c.knownTrueCfVars.thenDone(cfSp)
  c.activeBorrows.setLen(savedBorrowsLen)

  # Restore facts for else branch
  restore(c.facts, savedFacts)
  for f in condFactsList:
    var negated = f
    negateFact(negated)
    c.facts.add negated

  # Else branch
  if n.kind == DotToken:
    inc n # empty else
  else:
    traverseStmt c, n
  c.activeBorrows.setLen(savedBorrowsLen)

  # Record implication for this ite: captures writes and jtrue'd cfvars from each branch.
  # `guard` is the cfvar from a `(not guard)` condition (condCf), if any.
  c.writeSets.openRecord(condCf)
  for item in writesSp.thenData: c.writeSets.emitThen(item)
  for cf in cfSp.thenData: c.writeSets.emitThen(cf)
  c.writeSets.separateRecord()
  for item in c.writesTo.since(writesSp.cp): c.writeSets.emitElse(item)
  for cf in c.knownTrueCfVars.since(cfSp.cp): c.writeSets.emitElse(cf)
  c.writeSets.closeRecord()

  # Conservative merge: only keep facts/writes/cfvars that hold in both branches.
  # Variables written in only one branch are tracked via writeSets and resolved
  # through the cfvar implication mechanism (impliedWhenFalse, impliedByIte).
  c.facts = merge(thenFacts, 0, c.facts, false)
  c.writesTo.join(writesSp)
  c.knownTrueCfVars.join(cfSp)

  # If the condition was a direct cfvar and the then-branch activated cfvars via jtrue
  # (a leaving path), sequential code past this ite can only be reached when the
  # cfvar was false. Query implications to find vars now known initialized.
  # Example: `(ite ´g.0 (stmts quit jtrue ´r.0) .)` — after this, ´g.0 must be false,
  # so variables initialized only when ´g.0=false are now known initialized.
  if condDirectCf != NoSymId and cfSp.thenData.len > 0:
    for sym in c.writeSets.impliedWhenFalse(condDirectCf, c.knownCfVars):
      c.writesTo.add sym

  # Skip optional join information
  if n.kind == ParLe and n.stmtKind == StmtsS:
    skip n

  skipParRi n

proc traverseLoop(c: var NjvlContext; n: var Cursor) =
  ## Handle (loop pre cond body)
  inc n # skip loop tag

  # Pre-condition statements
  traverseStmt c, n

  # Analyze loop condition
  let savedBorrowsLen = c.activeBorrows.len
  let savedFacts = save(c.facts)
  var condCursor = n
  var wasEquality = false
  let condFact = translateCond(c, condCursor, wasEquality)
  skip n # skip condition expression

  # Add condition fact so body is analyzed knowing condition is true
  if condFact.isValid:
    c.facts.add condFact
    if wasEquality:
      c.facts.add condFact.geXplusC

  # Loop body
  traverseStmt c, n

  # After loop, we know the condition is false (if we exited normally)
  c.activeBorrows.setLen(savedBorrowsLen)
  restore(c.facts, savedFacts)
  if condFact.isValid:
    var negated = condFact
    negateFact(negated)
    c.facts.add negated

  skipParRi n

proc traverseLocal(c: var NjvlContext; n: var Cursor) =
  let kind = n.symKind
  inc n
  let name = n.symId
  skip n # name
  skip n # export marker
  let skipInitCheck = hasPragma(n, NoinitP)
  let isInline = hasPragma(n, InlineP)
  skip n # pragmas
  c.typeCache.registerLocal(name, kind, n)
  skip n # type
  if n.kind != DotToken or skipInitCheck:
    c.directlyInitialized[^1].incl name
  if kind == ResultY:
    c.resultSym = name
  if isInline:
    c.inlineVars[name] = n
  # Detect borrow: (haddr X) as init expression starts a borrow.
  # Validate that the path is borrowable (no deref in the middle, no calls).
  # Explicit `addr` in the path is an escape hatch ("unchecked").
  if n.kind == ParLe and n.exprKind == HaddrX:
    var inner = n
    inc inner # skip haddr tag
    let bc = checkBorrowable(inner, false)
    if bc == NotBorrowable:
      buildErr c, n.info, "cannot borrow from '" & asNimCode(inner) &
        "': path is not borrowable; use 'addr' to override or a temporary move"
    else:
      let path = extractPath(c, inner)
      if path.len > 0:
        c.activeBorrows.add BorrowInfo(path: path, borrower: name, info: n.info)
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
  skipParRi n

proc isInitializedAtProcEnd(c: var NjvlContext; symId: SymId): bool =
  ## Checks if `symId` is provably initialized at the end of a proc.
  ## Uses `impliedByIte`: if a cfvar cf appears anywhere in a writeSets record
  ## (as guard, in then-set, or in else-set), then all non-cfvar vars from both
  ## branches of that record are provably initialized.
  let eff = isEffectivelyInitialized(c, symId)
  if eff: return true
  for cf in c.knownCfVars:
    if symId in c.writeSets.impliedByIte(cf, c.knownCfVars):
      return true
  return false

proc traverseProc(c: var NjvlContext; n: var Cursor) =
  let decl = n
  c.facts = createFacts()
  c.directlyInitialized.add initHashSet[SymId]()
  c.procCanRaise = false
  let oldWritesTo = move c.writesTo
  let oldKnownTrueCfVars = move c.knownTrueCfVars
  let savedWriteSetsScopeIdx = c.writeSets.pushScope()
  let oldKnownCfVars = move c.knownCfVars
  let oldResultSym = c.resultSym
  let oldInlineVars = move c.inlineVars
  let oldBorrows = move c.activeBorrows
  c.resultSym = NoSymId
  inc n
  let symId = n.symId
  var isGeneric = false
  var isExternProc = false
  var outParams: seq[SymId] = @[]
  for i in 0 ..< BodyPos:
    if i == ProcPragmasPos:
      c.procCanRaise = hasPragma(n, RaisesP)
      isExternProc = hasPragma(n, ImportcP) or hasPragma(n, ImportcppP)
    elif i == TypevarsPos:
      isGeneric = n.substructureKind == TypevarsU
    elif i == ParamsPos:
      if n.kind == ParLe:
        var p = n
        inc p
        while p.kind != ParRi:
          let r = takeLocal(p, SkipFinalParRi)
          c.typeCache.registerLocal(r.name.symId, ParamY, r.typ)
          if r.typ.typeKind == OutT and not hasPragma(r.pragmas, NoinitP):
            outParams.add r.name.symId
      c.typeCache.registerLocal(symId, ProcY, decl)
    skip n

  # Analyze body
  if not isGeneric:
    traverseStmt c, n
    let info = decl.info
    # Emulate a "use result" / "use outParam" check at proc end.
    # In-body writes are wrapped in (ite (not ´r.0) ...) guards, so they appear in
    # writeSets but not in writesTo after the conservative join.
    # We check all declared cfvars: result is provably set if cf appears
    # anywhere in a writeSets record that also mentions result.
    # Skip importc/importcpp procs: they have no Nim body and satisfy their
    # out-params / result contract at the C level.
    if not isExternProc:
      if c.resultSym != NoSymId and not isInitializedAtProcEnd(c, c.resultSym):
        buildErr c, info, "cannot prove that " & pool.syms[c.resultSym] & " has been initialized"
      for sym in outParams:
        if not isInitializedAtProcEnd(c, sym):
          buildErr c, info, "cannot prove that " & pool.syms[sym] & " has been initialized"
  else:
    skip n
  skipParRi n
  c.writesTo = oldWritesTo
  c.knownTrueCfVars = oldKnownTrueCfVars
  c.writeSets.popScope(savedWriteSetsScopeIdx)
  c.knownCfVars = oldKnownCfVars
  c.resultSym = oldResultSym
  c.inlineVars = ensureMove oldInlineVars
  c.activeBorrows = ensureMove oldBorrows
  discard c.directlyInitialized.pop()

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
    # Control flow variable declaration
    inc n
    c.knownCfVars.incl n.symId
    skip n # symdef
    skipParRi n
  of JtrueV:
    # (jtrue cf1 cf2 ...) - cfvars listed here are now known true on this path.
    # NJ emits jtrue after noreturn calls and leaving paths (return/break/raise).
    # The cfvar information is used at join points to determine which branches are
    # leaving paths, enabling the writeSets implication mechanism.
    inc n
    while n.kind != ParRi:
      assert n.kind == Symbol
      c.knownTrueCfVars.add n.symId
      inc n
    inc n  # ParRi
  of KillV:
    # Variable going out of scope - end any active borrows
    inc n
    while n.kind != ParRi:
      let s = extractSymId(n)
      if s != NoSymId:
        endBorrow(c, s)
      skip n
    inc n # ParRi
  of UnknownV:
    # Unknown instruction - variable's contents become unknown after a call.
    # Check borrow conflicts: passing a borrowed path to a var param is a mutation.
    inc n
    let unknownPath = extractPath(c, n)
    if unknownPath.len > 0:
      checkBorrowConflict(c, unknownPath, n.info)
    skip n # the unknown location
    skipParRi n
  of ContinueV:
    # Continue in loop - skip
    skip n
  of VV:
    # Versioned variable reference - should not appear as statement
    skip n
  of EtupatV:
    traverseExpr c, n
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
      # Nested routine - analyze and advance past it
      c.typeCache.openScope()
      inc c.nestedProcs
      traverseProc c, n
      dec c.nestedProcs
      c.typeCache.closeScope()
    of TemplateS, TypeS, CommentS, PragmasS, RetS:
      skip n
    of CallKindsS:
      analyseCall c, n
    of DiscardS, YldS:
      inc n
      traverseExpr c, n
      skipParRi n
    of EmitS, InclS, ExclS:
      skip n
    of PragmaxS:
      inc n
      skip n # pragmas
      while n.kind != ParRi:
        traverseStmt c, n
      skipParRi n
    of NoStmt:
      if n.exprKind in CallKinds:
        analyseCall c, n
      elif n.exprKind == PragmaxX:
        inc n
        skip n
        traverseStmt c, n
        skipParRi n
      elif n.exprKind in {DestroyX, CopyX, WasmovedX, SinkhX, TraceX}:
        inc n
        traverseExpr c, n
        while n.kind != ParRi:
          traverseExpr c, n
        skipParRi n
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

proc traverseToplevel(c: var NjvlContext; n: var Cursor) =
  case n.stmtKind
  of StmtsS:
    inc n
    while n.kind != ParRi:
      traverseToplevel c, n
    skipParRi n
  of PragmaxS:
    inc n
    skip n
    traverseToplevel c, n
    skipParRi n
  of ProcS, FuncS, IteratorS, ConverterS, MethodS:
    inc c.nestedProcs
    traverseProc c, n
    dec c.nestedProcs
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
    moduleSuffix: moduleSuffix,
    directlyInitialized: @[initHashSet[SymId]()],
    writeSets: createWriteSets()
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
