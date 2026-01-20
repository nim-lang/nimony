# included in sem.nim

proc semObjectComponent(c: var SemContext; dest: var TokenBuf; n: var Cursor) =
  case n.substructureKind
  of FldU:
    semLocal(c, dest, n, FldY)
  of WhenU:
    var it = Item(n: n, typ: c.types.autoType)
    semWhenImpl(c, dest, it, ObjectWhen)
    n = it.n
  of CaseU:
    var it = Item(n: n, typ: c.types.autoType)
    semCaseImpl(c, dest, it, ObjectCase)
    n = it.n
  of StmtsU:
    inc n
    while n.kind != ParRi:
      semObjectComponent c, dest, n
    skipParRi n
  of NilU:
    takeTree dest, n
  else:
    buildErr c, dest, n.info, "illformed AST inside object: " & asNimCode(n)
    skip n

proc semObjectType(c: var SemContext; dest: var TokenBuf; n: var Cursor) =
  takeToken dest, n
  # inherits from?
  if n.kind == DotToken:
    takeToken dest, n
  else:
    let beforeType = dest.len
    semLocalTypeImpl c, dest, n, InLocalDecl
    let inheritsFrom = cursorAt(dest, beforeType)
    if c.routine.inGeneric == 0 and not isInheritable(inheritsFrom, true):
      endRead(dest)
      dest.shrink beforeType
      c.buildErr dest, n.info, "cannot inherit from type: " & asNimCode(inheritsFrom)
    else:
      endRead(dest)
  if n.kind == DotToken:
    takeToken dest, n
  else:
    # object fields:
    let oldScopeKind = c.currentScope.kind
    withNewScope c:
      # copy toplevel scope status for exported fields
      c.currentScope.kind = oldScopeKind
      while n.kind != ParRi:
        semObjectComponent c, dest, n
  takeParRi dest, n

proc semTupleType(c: var SemContext; dest: var TokenBuf; n: var Cursor) =
  dest.add parLeToken(TupleT, n.info)
  inc n
  # tuple fields:
  withNewScope c:
    while n.kind != ParRi:
      if n.substructureKind == KvU:
        takeToken dest, n
        let nameCursor = n
        let name = takeIdent(n)
        if name == StrId(0):
          c.buildErr dest, nameCursor.info, "invalid tuple field name", nameCursor
        else:
          dest.add identToken(name, nameCursor.info)
        semLocalTypeImpl c, dest, n, InLocalDecl
        takeParRi dest, n
      else:
        semLocalTypeImpl c, dest, n, InLocalDecl
  takeParRi dest, n

type
  EnumTypeState = object
    isBoolType: bool # `bool` is a magic enum and needs special handling
    isExported: bool
    enumType: SymId
    thisValue: xint
    hasHole: bool
    declaredNames: HashSet[StrId]

proc semEnumField(c: var SemContext; dest: var TokenBuf; n: var Cursor; state: var EnumTypeState)

proc semEnumType(c: var SemContext; dest: var TokenBuf; n: var Cursor; enumType: SymId; beforeExportMarker: int) =
  let start = dest.len
  takeToken dest, n
  let baseTypeStart = dest.len
  if n.kind == DotToken:
    wantDot c, dest, n
  else:
    takeTree dest, n
  let magicToken = dest[beforeExportMarker]
  var state = EnumTypeState(enumType: enumType, thisValue: createXint(0'i64), hasHole: false,
    isBoolType: magicToken.kind == ParLe and pool.tags[magicToken.tagId] == $BoolT,
    isExported: magicToken.kind != DotToken)
  var signed = false
  var lastValue = state.thisValue
  while n.substructureKind == EfldU:
    semEnumField(c, dest, n, state)
    if state.thisValue.isNegative:
      signed = true
    lastValue = state.thisValue
    inc state.thisValue
  if state.hasHole:
    dest[start] = parLeToken(HoleyEnumT, dest[start].info)
  var baseType: Cursor
  if signed:
    baseType = c.types.int32Type
  else:
    var err = false
    let max = asUnsigned(lastValue, err)
    # according to old size align computation:
    if max <= high(uint8).uint64:
      baseType = c.types.uint8Type
    elif max <= high(uint16).uint64:
      baseType = c.types.uint16Type
    elif max <= high(uint32).uint64:
      baseType = c.types.int32Type # according to old codegen
    else:
      baseType = c.types.int64Type # according to old codegen
  dest.replace baseType, baseTypeStart
  takeParRi dest, n

proc declareConceptSelf(c: var SemContext; dest: var TokenBuf; info: PackedLineInfo) =
  let name = pool.strings.getOrIncl("Self")
  let result = identToSym(c, name, TypevarY)
  let s = Sym(kind: TypevarY, name: result,
              pos: dest.len)
  discard c.currentScope.addNonOverloadable(name, s)
  let declStart = dest.len
  buildTree dest, TypevarY, info:
    dest.add symdefToken(result, info) # name
    dest.addDotToken() # export marker
    dest.addDotToken() # pragmas
    dest.addDotToken() # typ
    dest.addDotToken() # value
  publish c, dest, result, declStart

proc semConceptType(c: var SemContext; dest: var TokenBuf; n: var Cursor) =
  takeToken dest, n
  wantDot c, dest, n
  wantDot c, dest, n
  declareConceptSelf c, dest, n.info
  skip n # skip dot or previous `Self` declaration
  if n.stmtKind != StmtsS:
    bug "(stmts) expected, but got: ", n
  takeToken dest, n
  let oldScopeKind = c.currentScope.kind
  withNewScope c:
    # make syms of routines in toplevel concept also toplevel:
    c.currentScope.kind = oldScopeKind
    while true:
      let k = n.symKind
      if k in RoutineKinds:
        var it = Item(n: n, typ: c.types.voidType)
        semProc(c, dest, it, k, checkConceptProc)
        n = it.n
      else:
        break
  takeParRi dest, n
  takeParRi dest, n

proc subsGenericTypeFromArgs(c: var SemContext; dest: var TokenBuf;
                             info: PackedLineInfo; instSuffix: string;
                             origin, targetSym: SymId; decl: TypeDecl; args: Cursor) =
  #[
  What we need to do is rather simple: A generic instantiation is
  the typical (type :Name ex generic_params pragmas body) tuple but
  this time the generic_params list the used `Invoke` construct for the
  instantiation.
  ]#
  var inferred = initTable[SymId, Cursor]()
  var err = 0
  dest.buildTree TypeS, info:
    dest.add symdefToken(targetSym, info)
    dest.addDotToken() # export
    dest.buildTree InvokeT, info:
      dest.add symToken(origin, info)
      var a = args
      var typevars = decl.typevars
      inc typevars
      while a.kind != ParRi and typevars.kind != ParRi:
        var tv = typevars
        assert tv.substructureKind == TypevarU
        inc tv
        assert tv.kind == SymbolDef
        inferred[tv.symId] = a
        takeTree dest, a
        skip typevars
      if a.kind != ParRi:
        err = -1
      elif typevars.kind != ParRi:
        err = 1
    # take the pragmas from the origin:
    dest.copyTree decl.pragmas
    if err == 0:
      var sc = SubsContext(params: addr inferred, instSuffix: instSuffix)
      subs(c, dest, sc, decl.body)
      addFreshSyms(c, sc)
    elif err == 1:
      dest.buildLocalErr info, "too few generic arguments provided"
    else:
      dest.buildLocalErr info, "too many generic arguments provided"

proc isRangeExpr(n: Cursor): bool =
  var n = n
  if n.exprKind notin {CallX, InfixX}:
    return false
  inc n
  let name = takeIdent(n)
  result = name != StrId(0) and pool.strings[name] == ".."

proc addRangeValues(c: var SemContext; dest: var TokenBuf; n: var Cursor) =
  var err: bool = false
  let first = asSigned(evalOrdinal(c, n), err)
  if err:
    c.buildErr dest, n.info, "could not evaluate as ordinal", n
    err = false
  else:
    dest.addIntLit(first, n.info)
  skip n
  let last = asSigned(evalOrdinal(c, n), err)
  if err:
    c.buildErr dest, n.info, "could not evaluate as ordinal", n
    err = false
  else:
    dest.addIntLit(last, n.info)

proc semRangeTypeFromExpr(c: var SemContext; dest: var TokenBuf; n: var Cursor; info: PackedLineInfo) =
  inc n # call tag
  skip n # `..`
  dest.addParLe(RangetypeT, info)
  var it = Item(n: n, typ: c.types.autoType)
  var valuesBuf = createTokenBuf(4)

  # expression needs to be fully evaluated, switch to body phase
  var phase = SemcheckBodies
  swap c.phase, phase
  semExpr c, valuesBuf, it
  removeModifier(it.typ)
  semExpr c, valuesBuf, it

  swap c.phase, phase
  n = it.n
  # insert base type:
  dest.addSubtree it.typ
  var values = cursorAt(valuesBuf, 0)
  addRangeValues c, dest, values
  takeParRi dest, n

const InvocableTypeMagics = {ArrayT, RangetypeT, VarargsT,
  PtrT, RefT, UarrayT, SetT, StaticT, TypedescT,
  SinkT, LentT}

proc semMagicInvoke(c: var SemContext; dest: var TokenBuf; n: var Cursor; kind: TypeKind; info: PackedLineInfo) =
  # `n` is at first arg
  var typeBuf = createTokenBuf(16)
  typeBuf.addParLe(kind, info)
  # reorder invocation according to type specifications:
  case kind
  of ArrayT:
    # invoked as array[len, elem], but needs to become (array elem len)
    let indexPart = n
    skip n
    takeTree typeBuf, n # element type
    typeBuf.addSubtree indexPart
    takeParRi typeBuf, n
  of RangetypeT:
    # range types are invoked as `range[a..b]`
    if isRangeExpr(n):
      # don't bother calling semLocalTypeImpl, fully build type here
      semRangeTypeFromExpr c, dest, n, info
      skipParRi n
    else:
      c.buildErr dest, info, "expected `a..b` expression for range type"
      skipToEnd n
    return
  of PtrT, RefT, UarrayT, SetT, StaticT, TypedescT, SinkT, LentT:
    # unary invocations
    takeTree typeBuf, n
    takeParRi typeBuf, n
  of VarargsT:
    takeTree typeBuf, n
    if n.kind != ParRi:
      # optional varargs call
      takeTree typeBuf, n
    takeParRi typeBuf, n
  else:
    bug "unreachable" # see type kind check for magicKind
  var m = cursorAt(typeBuf, 0)
  semLocalTypeImpl c, dest, m, InLocalDecl

proc semInvoke(c: var SemContext; dest: var TokenBuf; n: var Cursor) =
  let typeStart = dest.len
  let info = n.info
  takeToken dest, n # copy `at`
  semLocalTypeImpl c, dest, n, InInvokeHead

  var headId: SymId = SymId(0)
  var decl = default TypeDecl
  var ok = false
  if dest[typeStart+1].kind == Symbol:
    headId = dest[typeStart+1].symId
    decl = getTypeSection(headId)
    if decl.kind != TypeY:
      c.buildErr dest, info, "cannot attempt to instantiate a non-type"
    elif decl.typevars.substructureKind != TypevarsU:
      c.buildErr dest, info, "cannot attempt to instantiate a concrete type"
    else:
      ok = true
  else:
    # symbol may have inlined into a magic
    let head = cursorAt(dest, typeStart+1)
    let kind = head.typeKind
    endRead(dest)
    if kind in InvocableTypeMagics:
      # magics that can be invoked
      dest.shrink typeStart
      semMagicInvoke(c, dest, n, kind, info)
      return
    else:
      c.buildErr dest, info, "cannot attempt to instantiate a non-type"

  var params = default(Cursor)
  if decl.kind == TypeY:
    params = decl.typevars
    inc params
  var paramCount = 0
  var argCount = 0
  var m = createMatch(addr c)
  let usedTypevarsInitial = c.usedTypevars
  let beforeArgs = dest.len
  while n.kind != ParRi:
    inc argCount
    let argInfo = n.info
    var argBuf = createTokenBuf(16)
    semLocalTypeImpl c, argBuf, n, AllowValues
    var addArg = true
    if cursorIsNil(params) or params.kind == ParRi:
      # will error later from param/arg count not matching
      discard
    else:
      inc paramCount
      let constraint = takeLocal(params, SkipFinalParRi).typ
      if constraint.kind != DotToken:
        var arg = beginRead(argBuf)
        var constraintMatch = constraint
        if not matchesConstraint(m, constraintMatch, arg):
          c.buildErr dest, argInfo, "type " & typeToString(arg) & " does not match constraint: " & typeToString(constraint)
          ok = false
          addArg = false
    if addArg:
      dest.add argBuf
  let usedTypevarsFinal = c.usedTypevars
  let isConcrete = usedTypevarsInitial == usedTypevarsFinal # no generic params were used
  takeParRi dest, n
  if ok and paramCount != argCount:
    dest.shrink typeStart
    c.buildErr dest, info, "wrong amount of generic parameters for type " & pool.syms[headId] &
      ", expected " & $paramCount & " but got " & $argCount
    return

  if ok and (isConcrete or
      # structural types are inlined even with generic arguments
      not isNominal(decl.body.typeKind)):
    # we have to be eager in generic type instantiations so that type-checking
    # can do its job properly:
    let key = typeToCanon(dest, typeStart)
    var sym = Sym(kind: TypeY, name: SymId(0), pos: InvalidPos) # pos unused by semTypeSym
    if c.instantiatedTypes.hasKey(key):
      let cachedSym = c.instantiatedTypes[key]
      dest.shrink typeStart
      dest.add symToken(cachedSym, info)
      sym.name = cachedSym
    else:
      var args = cursorAt(dest, beforeArgs)
      let instSuffix = instToSuffix(dest, typeStart)
      let targetSym = newInstSymId(c, headId, instSuffix)
      c.instantiatedTypes[key] = targetSym
      if isConcrete:
        c.typeInstDecls.add targetSym
      var sub = createTokenBuf(30)
      subsGenericTypeFromArgs c, sub, info, instSuffix, headId, targetSym, decl, args
      dest.endRead()
      let oldScope = c.currentScope
      # move to top level scope:
      while c.currentScope.up != nil:
        c.currentScope = c.currentScope.up
      var phase = SemcheckTopLevelSyms
      var topLevel = createTokenBuf(30)
      swap c.phase, phase
      var tn = beginRead(sub)
      semTypeSection c, topLevel, tn
      c.phase = SemcheckSignatures
      var instance = createTokenBuf(30)
      tn = beginRead(topLevel)
      semTypeSection c, instance, tn
      swap c.phase, phase
      c.currentScope = oldScope
      publish targetSym, ensureMove instance
      dest.shrink typeStart
      dest.add symToken(targetSym, info)
      sym.name = targetSym
    assert sym.name != SymId(0)
    semTypeSym c, dest, sym, info, typeStart, InLocalDecl

proc semArrayType(c: var SemContext; dest: var TokenBuf; n: var Cursor; context: TypeDeclContext) =
  let info = n.info
  takeToken dest, n
  semLocalTypeImpl c, dest, n, InLocalDecl
  # index type, possibilities are:
  # 1. length as integer
  # 2. range expression i.e. `a..b`
  # 3. full ordinal type i.e. `uint8`, `Enum`, `range[a..b]`
  # 4. standalone unresolved expression/type variable, could resolve to 1 or 3
  if isRangeExpr(n):
    semRangeTypeFromExpr c, dest, n, info
  else:
    var indexBuf = createTokenBuf(4)
    semLocalTypeImpl c, indexBuf, n, AllowValues
    var index = cursorAt(indexBuf, 0)
    if index.typeKind == RangetypeT:
      # direct range type
      dest.addSubtree index
    elif isOrdinalType(index):
      # ordinal type, turn it into a range type
      dest.addParLe(RangetypeT, index.info)
      dest.addSubtree index # base type
      var err = false
      let first = asSigned(firstOrd(c, index), err)
      if err:
        c.buildErr dest, index.info, "could not get first index of ordinal type: " & typeToString(index)
      else:
        dest.addIntLit(first, index.info)
      err = false
      let last = asSigned(lastOrd(c, index), err)
      if err:
        c.buildErr dest, index.info, "could not get last index of ordinal type: " & typeToString(index)
      else:
        dest.addIntLit(last, index.info)
      dest.addParRi()
    elif containsGenericParams(index):
      # unresolved types are left alone
      dest.addSubtree index
    elif index.typeKind != NoType:
      c.buildErr dest, index.info, "invalid array index type: " & typeToString(index)
    else:
      # length expression
      var err = false
      let length = asSigned(evalOrdinal(c, index), err)
      if err:
        c.buildErr dest, index.info, "invalid array index type: " & typeToString(index)
      else:
        dest.addParLe(RangetypeT, info)
        let ordinal = evalExpr(c, index)
        if ordinal[0].kind == UIntLit:
          dest.addSubtree c.types.uintType
        else:
          dest.addSubtree c.types.intType
        dest.addIntLit 0, info
        dest.addIntLit length - 1, info
        dest.addParRi()
  takeParRi dest, n

proc semRangeType(c: var SemContext; dest: var TokenBuf; n: var Cursor; context: TypeDeclContext) =
  takeToken dest, n
  semLocalTypeImpl c, dest, n, InLocalDecl
  var valuesBuf = createTokenBuf(4)
  semLocalTypeImpl c, valuesBuf, n, AllowValues
  semLocalTypeImpl c, valuesBuf, n, AllowValues
  var values = cursorAt(valuesBuf, 0)
  addRangeValues c, dest, values
  takeParRi dest, n

proc tryTypeClass(c: var SemContext; dest: var TokenBuf; n: var Cursor): bool =
  # if the type tree has no children, interpret it as a type kind typeclass
  var op = n
  inc op
  if op.kind == ParRi:
    dest.addParLe(TypeKindT, n.info)
    takeTree dest, n
    dest.addParRi()
    result = true
  else:
    result = false

proc isOrExpr(n: Cursor): bool =
  # old nim special cases `|` infixes in type contexts
  result = n.exprKind == InfixX
  if result:
    var n = n
    inc n
    let name = takeIdent(n)
    result = name != StrId(0) and (pool.strings[name] == "|" or pool.strings[name] == "or")

proc isAndExpr(n: Cursor): bool =
  result = n.exprKind == InfixX
  if result:
    var n = n
    inc n
    let name = takeIdent(n)
    result = name != StrId(0) and (pool.strings[name] == "and")

proc isNotExpr(n: Cursor): bool =
  result = n.exprKind == PrefixX
  if result:
    var n = n
    inc n
    let name = takeIdent(n)
    result = name != StrId(0) and (pool.strings[name] == "not")

proc handleNotnilType(c: var SemContext; dest: var TokenBuf; nn: var Cursor; context: TypeDeclContext): bool =
  result = false
  let info = nn.info
  var n = nn.firstSon # skip infix
  skip n # skip `not` identifier
  let before = dest.len
  semLocalTypeImpl c, dest, n, context
  if n.exprKind == NilX:
    skip n
    let nd = cursorAt(dest, before)
    if nd.typeKind in {RefT, PtrT, PointerT, CstringT}:
      dest.endRead()
      # remove ParRi of the pointer
      dest.shrink dest.len-1
      dest.addParPair NotNilU, info
      dest.addParRi()
    elif containsGenericParams(nd):
      # keep as is, will be checked later after generic instantiation:
      dest.endRead()
      dest.shrink before
      dest.addSubtree nn
    else:
      dest.endRead()
      dest.shrink before
      c.buildErr dest, info, "`not nil` only valid for a ptr/ref type"
    skipParRi n
    nn = n
    result = true
  else:
    dest.shrink before

proc handleNilableType(c: var SemContext; dest: var TokenBuf; nn: var Cursor; context: TypeDeclContext): bool =
  result = false
  if nn.exprKind == InfixX:
    # (nil ref <as ident> T)
    var n = nn
    inc n
    let info = n.info
    let ptrkind = takeIdent(n)
    var ptrk = NoType
    if ptrkind != StrId(0):
      if pool.strings[ptrkind] == "ref": ptrk = RefT
      elif pool.strings[ptrkind] == "ptr": ptrk = PtrT
      elif pool.strings[ptrkind] == "not":
        return handleNotnilType(c, dest, nn, context)
    if ptrk != NoType and n.exprKind == NilX:
      skip n # skip `nil`
      dest.addParLe ptrk, info
      semLocalTypeImpl c, dest, n, context
      dest.addParPair NilX, info
      takeParRi dest, n
      nn = n
      result = true
  elif nn.exprKind in {PrefixX, CmdX}:
    # `nil RootRef`
    var n = nn
    let info = n.info
    inc n
    if n.exprKind == NilX:
      skip n
      let before = dest.len
      semLocalTypeImpl c, dest, n, context
      let nd = cursorAt(dest, before)
      if nd.typeKind in {RefT, PtrT, PointerT, CstringT}:
        dest.endRead()
        # remove ParRi of the pointer
        dest.shrink dest.len-1
        dest.addParPair NilX, info
        dest.addParRi()
      elif containsGenericParams(nd):
        # keep as is, will be checked later after generic instantiation:
        dest.endRead()
        dest.shrink before
        dest.addSubtree nn
      else:
        dest.endRead()
        dest.shrink before
        c.buildErr dest, info, "`nil` only valid for a ptr/ref type"
      skipParRi n
      nn = n
      result = true

proc semLocalTypeImpl(c: var SemContext; dest: var TokenBuf; n: var Cursor; context: TypeDeclContext) =
  let info = n.info
  case n.kind
  of Ident:
    let start = dest.len
    let s = semIdent(c, dest, n, {})
    semTypeSym c, dest, s, info, start, context
  of Symbol:
    let start = dest.len
    let s = fetchSym(c, n.symId)
    dest.add n
    inc n
    semTypeSym c, dest, s, info, start, context
  of ParLe:
    case typeKind(n)
    of NoType:
      let xkind = exprKind(n)
      if xkind == QuotedX:
        let start = dest.len
        let s = semQuoted(c, dest, n, {})
        semTypeSym c, dest, s, info, start, context
      elif xkind == ParX:
        inc n
        semLocalTypeImpl c, dest, n, context
        skipParRi n
      elif xkind == TupX:
        semTupleType c, dest, n
      elif handleNilableType(c, dest, n, context):
        discard "handled"
      elif isOrExpr(n):
        # old nim special cases `|` infixes in type contexts
        # XXX `or` case temporarily handled here instead of magic overload in system
        dest.addParLe(OrT, info)
        inc n # tag
        skip n # `|`
        var nested = 1
        while nested != 0:
          if isOrExpr(n):
            inc n # tag
            skip n # `|`
            inc nested
          elif n.kind == ParRi:
            inc n
            dec nested
          else:
            semLocalTypeImpl c, dest, n, context
        dest.addParRi()
      elif isAndExpr(n):
        # XXX temporarily handled here instead of magic overload in system
        dest.addParLe(AndT, info)
        inc n # tag
        skip n # `and`
        var nested = 1
        while nested != 0:
          if isAndExpr(n):
            inc n # tag
            skip n # `and`
            inc nested
          elif n.kind == ParRi:
            inc n
            dec nested
          else:
            semLocalTypeImpl c, dest, n, context
        dest.addParRi()
      elif isNotExpr(n):
        # XXX temporarily handled here instead of magic overload in system
        dest.addParLe(NotT, info)
        inc n # tag
        skip n # `not`
        semLocalTypeImpl c, dest, n, context
        takeParRi dest, n
      elif false and isRangeExpr(n):
        # a..b, interpret as range type but only without AllowValues
        # to prevent conflict with HSlice
        # disabled for now, array types special case range expressions
        semRangeTypeFromExpr c, dest, n, info
      else:
        semTypeExpr c, dest, n, context, info
    of IntT, FloatT, CharT, BoolT, UIntT, NiltT, AutoT,
        SymKindT, UntypedT, TypedT, CstringT, PointerT, TypeKindT, OrdinalT:
      takeTree dest, n
    of VoidT:
      if context == InReturnTypeDecl:
        skip n
        dest.addDotToken()
      else:
        takeTree dest, n
    of PtrT, RefT:
      if tryTypeClass(c, dest, n):
        return
      takeToken dest, n
      if exprKind(n) == BracketX:
        # ptr[T] or ref[T], extract T
        inc n
        semLocalTypeImpl c, dest, n, InLocalDecl
        skipParRi n
      else:
        semLocalTypeImpl c, dest, n, InLocalDecl
      if n.kind != ParRi:
        takeTree dest, n # notnil, nil
      takeParRi dest, n
    of MutT, OutT, LentT, SinkT, NotT, UarrayT,
       StaticT, TypedescT:
      if tryTypeClass(c, dest, n):
        return
      takeToken dest, n
      semLocalTypeImpl c, dest, n, InLocalDecl
      takeParRi dest, n
    of SetT:
      if tryTypeClass(c, dest, n):
        return
      takeToken dest, n
      let elemTypeStart = dest.len
      semLocalTypeImpl c, dest, n, InLocalDecl
      takeParRi dest, n
      let elemType = cursorAt(dest, elemTypeStart)
      if containsGenericParams(elemType):
        # allow
        dest.endRead()
      elif not isOrdinalType(elemType, allowEnumWithHoles = true):
        dest.endRead()
        c.buildErr dest, info, "set element type must be ordinal"
      else:
        let length = lengthOrd(c, elemType)
        dest.endRead()
        if length.isNaN or length > MaxSetElements:
          c.buildErr dest, info, "type " & typeToString(elemType) & " is too large to be a set element type"
    of OrT, AndT:
      takeToken dest, n
      while n.kind != ParRi:
        semLocalTypeImpl c, dest, n, context
      takeParRi dest, n
    of TupleT:
      if tryTypeClass(c, dest, n):
        return
      semTupleType c, dest, n
    of ArrayT:
      if tryTypeClass(c, dest, n):
        return
      semArrayType c, dest, n, context
    of RangetypeT:
      if tryTypeClass(c, dest, n):
        return
      semRangeType c, dest, n, context
    of VarargsT:
      takeToken dest, n
      if n.kind != ParRi:
        semLocalTypeImpl c, dest, n, InLocalDecl
        if n.kind != ParRi:
          # optional converter
          var it = Item(n: n, typ: c.types.autoType)
          semExpr c, dest, it, {KeepMagics, AllowOverloads}
          # XXX Check the expression is a symchoice or a sym
          n = it.n
      takeParRi dest, n
    of ObjectT:
      if tryTypeClass(c, dest, n):
        discard
      elif context != InTypeSection:
        c.buildErr dest, info, "`object` type must be defined in a `type` section"
        skip n
      else:
        semObjectType c, dest, n
    of EnumT, HoleyEnumT:
      if tryTypeClass(c, dest, n):
        discard
      else:
        c.buildErr dest, info, "`enum` type must be defined in a `type` section"
        skip n
    of ConceptT:
      if context != InTypeSection:
        c.buildErr dest, info, "`concept` type must be defined in a `type` section"
        skip n
      else:
        semConceptType c, dest, n
    of DistinctT:
      if tryTypeClass(c, dest, n):
        discard
      elif context != InTypeSection:
        c.buildErr dest, info, "`distinct` type must be defined in a `type` section"
        skip n
      else:
        takeToken dest, n
        semLocalTypeImpl c, dest, n, InLocalDecl
        takeParRi dest, n
    of RoutineTypes:
      if tryTypeClass(c, dest, n):
        return
      takeToken dest, n
      wantDot c, dest, n # name
      wantDot c, dest, n # export marker
      wantDot c, dest, n # pattern
      wantDot c, dest, n # generics
      let beforeParams = dest.len
      c.openScope()
      semParams c, dest, n
      semLocalTypeImpl c, dest, n, InReturnTypeDecl
      var crucial = default CrucialPragma
      semPragmas c, dest, n, crucial, ProcY
      wantDot c, dest, n # exceptions
      # make it robust against Nifler's output
      if n.kind == ParRi:
        dest.addDotToken()
      else:
        wantDot c, dest, n # body
      # close it here so that pragmas like `requires` can refer to the params:
      c.closeScope()
      takeParRi dest, n
      if crucial.hasVarargs.isValid:
        addVarargsParameter c, dest, beforeParams, crucial.hasVarargs
    of InvokeT:
      semInvoke c, dest, n
    of ErrT:
      takeTree dest, n
    of ItertypeT:
      c.buildErr dest, info, "itertype not supported"
      skip n
  of DotToken:
    if context in {InReturnTypeDecl, InGenericConstraint}:
      takeToken dest, n
    else:
      c.buildErr dest, info, "not a type", n
      inc n
  else:
    if handleNilableType(c, dest, n, context):
      discard "handled"
    else:
      semTypeExpr c, dest, n, context, info
