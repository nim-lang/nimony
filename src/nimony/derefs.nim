#       Nimony
# (c) Copyright 2024 Andreas Rumpf
#
# See the file "license.txt", included in this
# distribution, for details about the copyright.

##[

Inserts `haddr` and `hderef` nodes for the code generators or analysis passes that
need to care (move analyser, etc.).

Expressions
===========

There are 4 cases:

1. `var T` to `var T` transfer: Nothing to do.
2. `var T` to `T` transfer: Insert HiddenDeref instruction.
3. `T` to `var T` transfer: Insert HiddenAddr instruction.
4. `T` to `T` transfer: Nothing to do.

]##

import std / [assertions]

include nifprelude

import nimony_model, programs, decls, typenav, sembasics, reporters

type
  Expects = enum
    WantT
    WantTButSkipDeref
    WantVarT
    WantVarTResult
    WantMutableT  # `var openArray` does need derefs but mutable locations regardless

  CurrentRoutine = object
    returnType: Expects
    firstParam: SymId
    firstParamKind: TypeKind
    dangerousLocations: seq[(SymId, Cursor)] # Cursor is only used for line information

  Context = object
    dest: TokenBuf
    r: CurrentRoutine
    typeCache: TypeCache

proc takeToken(c: var Context; n: var Cursor) {.inline.} =
  c.dest.add n
  inc n

proc wantParRi(c: var Context; n: var Cursor) =
  if n.kind == ParRi:
    c.dest.add n
    inc n
  else:
    error "expected ')', but got: ", n

proc rootOf(n: Cursor): SymId =
  var n = n
  while true:
    case n.exprKind
    of DotX, AtX, ArrAtX, ParX:
      # `PatX`, `DerefDotX` deliberately missing here as they are not protected from mutation
      inc n
    of DconvX, OconvX:
      inc n
      skip n # skip the type
    else: break
  case n.kind
  of Symbol, SymbolDef:
    result = n.symId
  else:
    result = NoSymId

proc isAddressable*(n: Cursor): bool =
  ## Addressable means that we can take the address of the expression.
  let s = rootOf(n)
  if s != NoSymId:
    let res = tryLoadSym(s)
    assert res.status == LacksNothing
    let local = asLocal(res.decl)
    result = local.kind in {ParamY, LetY, ResultY, VarY, CursorY, LetY, ConstY}
    # Assignments to `ConstY` are prevented later.
  else:
    result = false

proc tr(c: var Context; n: var Cursor; e: Expects)

proc trSons(c: var Context; n: var Cursor; e: Expects) =
  if n.kind != ParLe:
    takeToken c, n
  else:
    takeToken c, n
    while n.kind != ParRi:
      tr c, n, e
    wantParRi c, n

proc validBorrowsFrom(c: var Context; n: Cursor): bool =
  # --------------------------------------------------------------------------
  # We need to borrow from a location that is derived from the proc's
  # first parameter.
  # --------------------------------------------------------------------------
  var n = n
  var someIndirection = false
  while true:
    case n.exprKind
    of DotX, AtX, ArrAtX, ParX:
      inc n
    of HderefX, HaddrX, DerefX, AddrX, DerefDotX:
      inc n
      someIndirection = true
    of DconvX, OconvX, ConvX, CastX:
      inc n
      skip n # skip the type
    of ExprX:
      while true:
        if isLastSon(n): break
        skip n
    of CallKinds:
      inc n
      let fn = n
      skip n # skip the `fn`
      if n.kind != ParRi:
        var fnType = getType(c.typeCache, fn)
        assert fnType == "params"
        inc fnType
        let firstParam = asLocal(fnType)
        if firstParam.kind == ParamY:
          let mightForward = firstParam.typ.typeKind in {MutT, OutT}
          if not mightForward:
            someIndirection = true
        # borrowing only work from first parameters:
        discard "n already at the correct position"
      else:
        break
    else:
      break
  if n.kind == Symbol:
    let res = tryLoadSym(n.symId)
    assert res.status == LacksNothing
    # XXX Is this check really reliable for local symbols
    # that are not guaranteed to be unique?
    if res.decl == "param" and n.symId == c.r.firstParam:
      # There is a difference between
      # proc forward(x: var int): var int = x
      # and
      # proc access(x: Table): var int = x[].field[0]
      # and we do not try to hide it!
      result = c.r.firstParamKind in {MutT, OutT} or someIndirection
    else:
      result = false
  else:
    result = false

proc borrowsFromReadonly(c: var Context; n: Cursor): bool =
  var n = n
  while true:
    case n.exprKind
    of DotX, AtX, ArrAtX, ParX:
      inc n
    of DconvX, HconvX, ConvX, CastX:
      inc n
      skip n
    of CallKinds:
      inc n
      skip n # skip the `fn`
      if n.kind != ParRi:
        # borrowing only work from first parameters:
        discard "n already at the correct position"
      else:
        break
    else:
      break
  if n.kind == Symbol:
    let res = tryLoadSym(n.symId)
    assert res.status == LacksNothing
    let local = asLocal(res.decl)
    case local.kind
    of ConstY:
      result = true
    of LetY:
      let tk = local.typ.typeKind
      result = tk notin {MutT, OutT}
      if result and tk == OpenArrayT:
        # Special rule to make `toOpenArray` work:
        result = borrowsFromReadonly(c, local.val)
    of ParamY:
      result = local.typ.typeKind notin {MutT, OutT}
    else:
      result = false
  elif n.kind in {StringLit, IntLit, UIntLit, FloatLit, CharLit} or
       n.exprKind in {SufX, OconstrX, NewOconstrX, AconstrX}:
    result = true
  else:
    result = false

proc isResultUsage(n: Cursor): bool {.inline.} =
  if n.kind == Symbol:
    let res = tryLoadSym(n.symId)
    assert res.status == LacksNothing
    let local = asLocal(res.decl)
    result = local.kind == ResultY
  else:
    result = false

proc trReturn(c: var Context; n: var Cursor) =
  takeToken c, n
  if isResultUsage(n):
    takeTree c.dest, n
  else:
    let err = c.r.returnType == WantVarTResult and not validBorrowsFrom(c, n)
    if err:
      buildLocalErr(c.dest, n.info, "cannot borrow from " & toString(n, false))
    else:
      tr c, n, c.r.returnType
  wantParRi c, n

proc mightBeDangerous(c: var Context; n: Cursor) =
  let root = rootOf(n)
  if root != NoSymId:
    for d in items(c.r.dangerousLocations):
      if d[0] == root:
        buildLocalErr c.dest, n.info, "cannot mutate " & toString(n, false) &
          "; binding created here: " & infoToStr(d[1].info)

proc checkForDangerousLocations(c: var Context; n: var Cursor) =
  template recurse =
    while n.kind != ParRi:
      checkForDangerousLocations c, n

  if isDeclarative(n):
    skip n
  elif n.stmtKind == AsgnS:
    inc n
    mightBeDangerous(c, n)
    recurse()
  elif n.exprKind in CallKinds:
    inc n # skip `(call)`
    let orig = n
    var fnType = getType(c.typeCache, n)
    skip n # skip `fn`
    assert fnType == "params"
    inc fnType
    while n.kind != ParRi:
      let previousFormalParam = fnType
      let param = takeLocal(fnType, SkipFinalParRi)
      let pk = param.typ.typeKind
      if pk in {MutT, OutT}:
        mightBeDangerous(c, n)
      elif pk == VarargsT:
        # do not advance formal parameter:
        fnType = previousFormalParam
      skip n
    n = orig
    recurse()
  elif n.kind == ParLe:
    recurse()

proc trProcDecl(c: var Context; n: var Cursor) =
  c.typeCache.openScope()
  takeToken c, n
  let symId = n.symId
  var isGeneric = false
  var r = CurrentRoutine(returnType: WantT)
  for i in 0..<BodyPos:
    if i == TypevarsPos:
      isGeneric = n.substructureKind == TypevarsS
    if i == ParamsPos:
      c.typeCache.registerParams(symId, n)
      var params = n
      inc params
      let firstParam = asLocal(params)
      if firstParam.kind == ParamY:
        r.firstParam = firstParam.name.symId
        r.firstParamKind = firstParam.typ.typeKind
        if r.firstParamKind in {MutT, OutT}: r.returnType = WantVarTResult
    takeTree c.dest, n
  if isGeneric:
    takeTree c.dest, n
    wantParRi c, n
  else:
    swap c.r, r
    var body = n
    trSons c, n, WantT
    if c.r.dangerousLocations.len > 0:
      checkForDangerousLocations c, body
    swap c.r, r
    wantParRi c, n
  c.typeCache.closeScope()

proc trCallArgs(c: var Context; n: var Cursor; fnType: Cursor) =
  var fnType = fnType
  assert fnType == "params"
  inc fnType
  while n.kind != ParRi:
    var e = WantT
    let previousFormalParam = fnType
    let param = takeLocal(fnType, SkipFinalParRi)
    var pk = param.typ.typeKind
    if pk == MutT:
      var elemType = param.typ
      inc elemType
      if elemType.typeKind == OpenArrayT:
        e = WantMutableT
      else:
        e = WantVarT
    elif pk == OutT:
      e = WantVarT
    elif pk == VarargsT:
      # do not advance formal parameter:
      fnType = previousFormalParam
    tr c, n, e

proc firstArgIsMutable(c: var Context; n: Cursor): bool =
  assert n.exprKind in CallKinds
  var n = n
  inc n
  if n.kind != ParRi:
    result = not borrowsFromReadonly(c, n)
  else:
    result = false

proc trCall(c: var Context; n: var Cursor; e: Expects; dangerous: var bool) =
  let info = n.info
  let callExpr = n

  var callBuf = createTokenBuf()

  swap c.dest, callBuf
  takeToken c, n
  let fnType = getType(c.typeCache, n)
  assert fnType == "params"
  takeToken c, n
  var retType = fnType
  skip retType

  var needHderef = false
  if retType.typeKind == MutT:
    if e == WantT:
      needHderef = true
      trCallArgs(c, n, fnType)
    elif e in {WantVarTResult, WantTButSkipDeref} or firstArgIsMutable(c, callExpr):
      trCallArgs(c, n, fnType)
    elif not dangerous:
      # DANGER-ZONE HERE! Consider: `for d in items(a)` where `a` is immutable.
      # Which is turned to `var d: var T = a[i]`.
      # Wether we can allow this or not depends on whether `d` is used later on for mutations!
      # Thus we mark the binding as "dangerous". In a second pass we then look for the pattern
      # `passedToVar(d)` or `d[] = value` and flag them as errornous.
      dangerous = true
      trCallArgs(c, n, fnType)
    else:
      buildLocalErr c.dest, info, "cannot pass $1 to var/out T parameter"
  elif e notin {WantT, WantTButSkipDeref}:
    if retType.typeKind == OpenArrayT and firstArgIsMutable(c, callExpr):
      trCallArgs(c, n, fnType)
    else:
      buildLocalErr c.dest, info, "cannot pass $1 to var/out T parameter"
  else:
    trCallArgs(c, n, fnType)
  wantParRi c, n

  swap c.dest, callBuf
  if needHderef:
    c.dest.addParLe(HderefX, info)
    c.dest.add callBuf
    c.dest.addParRi()
  else:
    c.dest.add callBuf

proc trAsgnRhs(c: var Context; le: Cursor; ri: var Cursor; e: Expects) =
  if ri.exprKind in CallKinds:
    var dangerous = false
    trCall c, ri, e, dangerous
    if dangerous:
      let s = rootOf(le)
      if s != NoSymId:
        c.r.dangerousLocations.add (s, ri)
  else:
    tr c, ri, e

type
  LvalueStatus = enum
    Valid
    InvalidBorrow
    LocationIsConst

proc trAsgn(c: var Context; n: var Cursor) =
  takeToken c, n
  var e = WantT
  var err = Valid
  let le = n
  if isResultUsage(le):
    e = c.r.returnType
    if e == WantVarTResult:
      tr c, n, e
      if not validBorrowsFrom(c, n):
        err = InvalidBorrow
    else:
      tr c, n, e
  elif borrowsFromReadonly(c, n):
    err = LocationIsConst
  else:
    tr c, n, e
  case err
  of InvalidBorrow:
    buildLocalErr c.dest, n.info, "cannot borrow from " & toString(n, false)
  of LocationIsConst:
    buildLocalErr c.dest, n.info, "cannot mutate expression " & toString(n, false)
    tr c, n, e
    tr c, n, e
  else:
    trAsgnRhs c, le, n, e
  wantParRi c, n

proc trLocation(c: var Context; n: var Cursor; e: Expects) =
  # Idea: A variable like `x` does not own its value as it can be read multiple times.
  let typ = getType(c.typeCache, n)
  let k = typ.typeKind
  if k in {MutT, OutT}:
    if e notin {WantT, WantTButSkipDeref}:
      # Consider `fvar(returnsVar(someLet))`: We must not allow this.
      if borrowsFromReadonly(c, n):
        buildLocalErr c.dest, n.info, "cannot pass $1 to var/out T parameter"
      trSons c, n, WantT
    else:
      if (k == MutT and typ.firstSon.typeKind != OpenArrayT) or k == OutT:
        c.dest.addParLe(HderefX, n.info)
        trSons c, n, WantT
        c.dest.addParRi()
      else:
        trSons c, n, WantT
  elif e notin {WantT, WantTButSkipDeref}:
    if borrowsFromReadonly(c, n):
      buildLocalErr c.dest, n.info, "cannot pass $1 to var/out T parameter"
      trSons c, n, WantT
    else:
      c.dest.addParLe(HaddrX, n.info)
      trSons c, n, WantT
      c.dest.addParRi()
  else:
    trSons c, n, WantT

proc trLocal(c: var Context; n: var Cursor) =
  takeToken c, n
  let name = n
  for i in 0..<LocalTypePos:
    takeTree c.dest, n
  let typ = n
  takeTree c.dest, n
  c.typeCache.registerLocal(name.symId, typ)
  let e = if typ.typeKind in {OutT, MutT}: WantVarT else: WantT
  trAsgnRhs c, name, n, e
  wantParRi c, n

proc trStmtListExpr(c: var Context; n: var Cursor; outerE: Expects) =
  takeToken c, n
  while n.kind != ParRi:
    if isLastSon(n):
      tr c, n, outerE
    else:
      tr c, n, WantT
  wantParRi c, n

proc trObjConstr(c: var Context; n: var Cursor) =
  takeToken c, n
  takeTree c.dest, n # type
  while n.kind != ParRi:
    assert n.exprKind == KvX
    takeToken c, n
    takeTree c.dest, n # key
    let fieldType = getType(c.typeCache, n)
    let e = if fieldType.typeKind in {MutT, OutT}: WantVarT else: WantT
    tr c, n, e
    wantParRi c, n
  wantParRi c, n

proc tr(c: var Context; n: var Cursor; e: Expects) =
  case n.kind
  of Symbol:
    trLocation c, n, e
  of IntLit, UIntLit, FloatLit, CharLit, StringLit:
    if e notin {WantT, WantTButSkipDeref}:
      # Consider `fvar(returnsVar(someLet))`: We must not allow this.
      buildLocalErr c.dest, n.info, "cannot pass $1 to var/out T parameter"
      inc n
    else:
      takeToken c, n
  of UnknownToken, ParRi, EofToken, DotToken, Ident, SymbolDef:
    takeToken c, n
  of ParLe:
    case n.exprKind
    of CallKinds:
      var disallowDangerous = true
      trCall c, n, e, disallowDangerous
    of DotX, DerefDotX, AtX, ArrAtX, PatX:
      trLocation c, n, e
    of OconstrX, NewOconstrX:
      if e notin {WantT, WantTButSkipDeref}:
        buildLocalErr c.dest, n.info, "cannot pass $1 to var/out T parameter"
        skip n
      else:
        trObjConstr c, n
    of ExprX:
      trStmtListExpr c, n, e
    of DconvX, OconvX:
      trSons c, n, WantT
    of ParX:
      trSons c, n, e
    of HconvX, ConvX, CastX:
      if e notin {WantT, WantTButSkipDeref}:
        buildLocalErr c.dest, n.info, "cannot pass $1 to var/out T parameter"
        skip n
      else:
        trSons c, n, WantT

    else:
      case n.stmtKind
      of RetS:
        trReturn(c, n)
      of AsgnS:
        trAsgn c, n
      of VarS, LetS, ConstS, CursorS, ResultS:
        trLocal c, n
      of ProcS, FuncS, MacroS, MethodS, ConverterS:
        trProcDecl c, n
      of ScopeS:
        c.typeCache.openScope()
        trSons c, n, WantT
        c.typeCache.closeScope()
      else:
        if isDeclarative(n):
          takeTree c.dest, n
        else:
          trSons c, n, WantT

proc injectDerefs*(n: Cursor): TokenBuf =
  var c = Context(typeCache: createTypeCache(),
    r: CurrentRoutine(returnType: WantT, firstParam: NoSymId), dest: TokenBuf())
  c.typeCache.openScope()
  var n2 = n
  var n3 = n
  tr(c, n2, WantT)
  if c.r.dangerousLocations.len > 0:
    checkForDangerousLocations(c, n3)
  c.typeCache.closeScope()
  result = ensureMove(c.dest)
