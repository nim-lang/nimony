# included in sem.nim

proc semProcBody(c: var SemContext; itB: var Item) =
  var it = Item(n: itB.n, typ: c.types.autoType)
  var lastSonInfo = itB.n.info
  var beforeLastSon = c.dest.len
  var beforeLastSonCursor = default(Cursor)
  while it.n.kind != ParRi:
    if not isLastSon(it.n):
      semStmt c, it.n, false
    else:
      beforeLastSon = c.dest.len
      lastSonInfo = it.n.info
      beforeLastSonCursor = it.n
      semExpr c, it, {AllowEmpty}
  if c.routine.kind == TemplateY:
    case c.routine.returnType.typeKind
    of UntypedT:
      discard "ok"
    of VoidT:
      typecheck(c, lastSonInfo, it.typ, c.routine.returnType)
    else:
      commonType c, it, beforeLastSon, c.routine.returnType
  elif classifyType(c, it.typ) in {VoidT, UntypedT}:
    discard "ok"
  else:
    # transform `expr` to `result = expr`:
    if c.routine.resId != SymId(0):
      commonType c, it, beforeLastSon, c.routine.returnType

      var prefix = [
        parLeToken(AsgnS, lastSonInfo),
        symToken(c.routine.resId, lastSonInfo)]
      c.dest.insert prefix, beforeLastSon
      c.dest.addParRi()
    else:
      commonType c, it, beforeLastSon, c.routine.returnType
  takeParRi c, it.n # of (stmts)
  itB.n = it.n

proc exportMarkerBecomesNifTag(c: var SemContext; insertPos: int; crucial: CrucialPragma) =
  assert crucial.magic.len > 0
  let info = c.dest[insertPos].info

  if crucial.bits != 0:
    let nifTag = [
      parLeToken(pool.tags.getOrIncl(crucial.magic), info),
      intToken(pool.integers.getOrIncl(crucial.bits), info),
      parRiToken(info)
    ]
    c.dest.replace fromBuffer(nifTag), insertPos
  else:
    let nifTag = [
      parLeToken(pool.tags.getOrIncl(crucial.magic), info),
      parRiToken(info)
    ]
    c.dest.replace fromBuffer(nifTag), insertPos

proc semLocalValue(c: var SemContext; it: var Item; crucial: CrucialPragma) =
  if ThreadvarP in crucial.flags:
    c.buildErr it.n.info, "a `threadvar` cannot have an init value"
    skip it.n
  else:
    semExpr c, it

proc semLocal(c: var SemContext; n: var Cursor; kind: SymKind) =
  let declStart = c.dest.len
  takeToken c, n
  var delayed = handleSymDef(c, n, kind) # 0
  let beforeExportMarker = c.dest.len
  wantExportMarker c, n # 1
  var crucial = CrucialPragma(sym: delayed.s.name)
  semPragmas c, n, crucial, kind # 2
  if crucial.magic.len > 0:
    exportMarkerBecomesNifTag c, beforeExportMarker, crucial
  if delayed.status == OkExistingFresh and InjectP in crucial.flags:
    # symbol is injected, add it to scope
    delayed.status = OkNew

  var beforeType = -1

  case kind
  of TypevarY:
    discard semLocalType(c, n, InGenericConstraint)
    wantDot c, n
  of ParamY, LetY, VarY, ConstY, CursorY, ResultY, FldY, GletY, TletY, GvarY, TvarY:
    beforeType = c.dest.len
    if n.kind == DotToken:
      # no explicit type given:
      inc n # 3
      let orig = n
      var it = Item(n: n, typ: c.types.autoType)
      if kind == ConstY:
        withNewScope c:
          semConstExpr c, it # 4
      elif kind == ParamY and it.n.kind == DotToken:
        if delayed.lit in c.usingStmtMap:
          it.typ = c.usingStmtMap[delayed.lit]
        elif c.routine.kind in {TemplateY, MacroY}:
          it.typ = c.types.untypedType
        else:
          buildErr c, it.n.info, "type or init value expected"
        c.dest.takeToken it.n
      else:
        semLocalValue c, it, crucial # 4
      n = it.n
      let typ = skipModifier(it.typ)
      if classifyType(c, typ) == VoidT:
         c.buildErr n.info, "expression '" & asNimCode(orig) & "' has no type (or is ambiguous)"
      insertType c, typ, beforeType
    else:
      let typ = semLocalType(c, n) # 3
      if n.kind == DotToken:
        # empty value
        takeToken c, n
      else:
        var it = Item(n: n, typ: typ)
        if kind == ConstY:
          withNewScope c:
            semConstExpr c, it # 4
        else:
          semLocalValue c, it, crucial # 4
        n = it.n
        patchType c, it.typ, beforeType
  else:
    bug "semLocal"

  if beforeType != -1:
    let hasError = c.addSymForwardError delayed

    if hasError:
      var valueCursor = cursorAt(c.dest, beforeType)
      skip valueCursor # skips types
      let newValuePos = cursorToPosition(c.dest, valueCursor)
      var valueBuf = createTokenBuf()
      valueBuf.addSubtree valueCursor
      endRead(c.dest)
      shrink c.dest, newValuePos

      let orig = beginRead(valueBuf)
      c.buildErr delayed.info, "attempt to redeclare: " & pool.strings[delayed.lit], orig

  else:
    c.addSym delayed

  takeParRi c, n
  if kind == LetY:
    if ThreadvarP in crucial.flags:
      copyKeepLineInfo c.dest[declStart], parLeToken(TletS, NoLineInfo)
    elif GlobalP in crucial.flags or c.currentScope.kind == TopLevelScope:
      copyKeepLineInfo c.dest[declStart], parLeToken(GletS, NoLineInfo)
  elif kind == VarY:
    if ThreadvarP in crucial.flags:
      copyKeepLineInfo c.dest[declStart], parLeToken(TvarS, NoLineInfo)
    elif GlobalP in crucial.flags or c.currentScope.kind == TopLevelScope:
      copyKeepLineInfo c.dest[declStart], parLeToken(GvarS, NoLineInfo)
  publish c, delayed.s.name, declStart

proc semLocal(c: var SemContext; it: var Item; kind: SymKind) =
  let info = it.n.info
  semLocal c, it.n, kind
  producesVoid c, info, it.typ

proc semEnumField(c: var SemContext; n: var Cursor; state: var EnumTypeState) =
  let declStart = c.dest.len
  takeToken c, n
  var delayed = handleSymDef(c, n, EfldY) # 0
  if delayed.status == OkExistingFresh:
    # XXX original nim always injects enum fields regardless of the enum sym itself,
    # this does the same here
    delayed.status = OkNew
  if state.declaredNames.contains delayed.lit:
    delayed.status = ErrRedef
  else:
    state.declaredNames.incl delayed.lit
  let beforeExportMarker = c.dest.len
  if n.kind == DotToken:
    if state.isExported:
      # if enum type is exported, enum field is exported
      c.dest.add identToken(pool.strings.getOrIncl("x"), n.info)
    else:
      c.dest.add n
    inc n # 1
  else:
    wantExportMarker c, n # 1
  var crucial = CrucialPragma(sym: delayed.s.name)
  semPragmas c, n, crucial, EfldY # 2
  if state.isBoolType and crucial.magic.len == 0:
    # bool type, set magic to fields if unset
    if state.thisValue == zero():
      crucial.magic = "false"
    else:
      crucial.magic = "true"
  if crucial.magic.len > 0:
    exportMarkerBecomesNifTag c, beforeExportMarker, crucial
  if n.kind == DotToken or n.kind == Symbol:
    if state.isBoolType:
      c.dest.addParLe(BoolT, n.info)
      c.dest.addParRi()
    else:
      c.dest.add symToken(state.enumType, n.info)
    inc n # 3
  else:
    c.buildErr n.info, "enum field's type must be empty"

  if n.kind == DotToken:
    # empty value
    let info = c.dest[declStart].info
    c.dest.add parLeToken(TupX, info)
    c.addXint state.thisValue, info
    c.dest.add strToken(delayed.lit, info)
    c.dest.addParRi()
    inc n
  else:
    if n.kind == ParLe and n.exprKind == TupX:
      c.dest.add n
      inc n
      let explicitValue = evalConstIntExpr(c, n, c.types.autoType) # 4
      if explicitValue != state.thisValue:
        state.hasHole = true
        state.thisValue = explicitValue
      c.dest.add evalExpr(c, n)
      takeParRi c, n
    else:
      var ec = initEvalContext(addr c)
      var valueCursor = n
      let fieldValue = eval(ec, valueCursor)
      if fieldValue.kind == StringLit:
        c.dest.add parLeToken(TupX, n.info)
        c.addXint state.thisValue, n.info
        c.dest.add fieldValue
        c.dest.addParRi()
        n = valueCursor
      else:
        c.dest.add parLeToken(TupX, n.info)
        let explicitValue = evalConstIntExpr(c, n, c.types.autoType) # 4
        if explicitValue != state.thisValue:
          state.hasHole = true
          state.thisValue = explicitValue
        c.dest.add strToken(delayed.lit, n.info)
        c.dest.addParRi()
  takeParRi c, n
  if delayed.status == OkNew:
    addOverloadable(c.currentScope, delayed.lit, delayed.s)
  elif delayed.status == ErrRedef:
    c.buildErr delayed.info, "attempt to redeclare: " & pool.strings[delayed.lit]
  publish c, delayed.s.name, declStart

proc semGenericParam(c: var SemContext; n: var Cursor) =
  if n.substructureKind == TypevarU:
    semLocal c, n, TypevarY
  else:
    buildErr c, n.info, "expected 'typevar'"

proc semGenericParams(c: var SemContext; n: var Cursor) =
  if n.kind == DotToken:
    takeToken c, n
  elif n.substructureKind == TypevarsU:
    inc c.routine.inGeneric
    takeToken c, n
    while n.kind != ParRi:
      semGenericParam c, n
    takeParRi c, n
  elif n.typeKind == InvokeT:
    inc c.routine.inInst
    takeTree c, n
  else:
    buildErr c, n.info, "expected '.' or 'typevars'"

proc semParam(c: var SemContext; n: var Cursor) =
  if n.substructureKind == ParamU:
    semLocal c, n, ParamY
  else:
    buildErr c, n.info, "expected 'param'"

proc semParams(c: var SemContext; n: var Cursor) =
  if n.kind == DotToken:
    takeToken c, n
  elif n.substructureKind == ParamsU:
    takeToken c, n
    while n.kind != ParRi:
      semParam c, n
    takeParRi c, n
  else:
    buildErr c, n.info, "expected '.' or 'params'"

proc addReturnResult(c: var SemContext; resId: SymId; info: PackedLineInfo) =
  if resId != SymId(0):
    assert c.dest[c.dest.len-1].kind == ParRi
    c.dest.shrink c.dest.len-1 # remove the ParRi
    # maybe add `return result`:
    buildTree(c.dest, RetS, info):
      c.dest.addSymUse resId, info
    c.dest.addParRi() # add it back

proc semBorrow(c: var SemContext; fn: StrId; beforeParams: int) =
  let signature = cursorAt(c.dest, beforeParams)
  var procBody = genBorrowedProcBody(c, fn, signature, signature.info)
  endRead(c.dest)
  var n = cursorAt(procBody, 0)
  takeToken c, n # `(stmts`
  var it = Item(n: n, typ: c.types.autoType)
  let resId = declareResult(c, it.n.info)
  semProcBody c, it
  addReturnResult c, resId, procBody[procBody.len - 1].info

proc getParamsType(c: var SemContext; paramsAt: int): seq[TypeCursor] =
  result = @[]
  if c.dest[paramsAt].kind != DotToken:
    var n = cursorAt(c.dest, paramsAt)
    if n.substructureKind == ParamsU:
      inc n
      while n.kind != ParRi:
        if n.symKind == ParamY:
          var local = takeLocal(n, SkipFinalParRi)
          result.add local.typ
        else:
          break
      endRead(c.dest)

proc getObjSymId(c: var SemContext; obj: TypeCursor): SymId =
  var obj = skipModifier(obj)
  while true:
    if obj.typeKind == InvokeT:
      inc obj
    else:
      break
  if obj.kind == Symbol:
    result = obj.symId
  else:
    result = SymId(0)

proc checkTypeHook(c: var SemContext; params: seq[TypeCursor]; op: HookKind; info: PackedLineInfo) =
  var cond: bool
  case op
  of NoHook:
    return
  of DestroyH:
    cond = classifyType(c, c.routine.returnType) == VoidT and params.len == 1
    if not cond:
      buildErr c, info, "signature for '=destroy' must be proc[T: object](x: T)"
  of TraceH:
    cond = classifyType(c, c.routine.returnType) == VoidT and params.len == 2 and
      classifyType(c, params[0]) == MutT and classifyType(c, params[1]) == PointerT
  of WasmovedH:
    cond = classifyType(c, c.routine.returnType) == VoidT and
        params.len == 1 and classifyType(c, params[0]) == MutT
  of CopyH:
    cond = classifyType(c, c.routine.returnType) == VoidT and params.len == 2 and
      classifyType(c, params[0]) == MutT
  of SinkhH:
    cond = classifyType(c, c.routine.returnType) == VoidT and params.len == 2 and
      classifyType(c, params[0]) == MutT
  of DupH:
    cond = params.len == 1 and sameTrees(params[0], c.routine.returnType)

  if cond:
    let obj = getObjSymId(c, params[0])

    if obj == SymId(0):
      cond = false
    else:
      let res = tryLoadSym(obj)
      assert res.status == LacksNothing
      if res.decl.symKind == TypeY:
        let typeDecl = asTypeDecl(res.decl)

        if not (classifyType(c, typeDecl.body) == ObjectT):
          cond = false
      else:
        cond = false

  if not cond:
    case op
    of NoHook:
      discard
    of DestroyH:
      buildErr c, info, "signature for '=destroy' must be proc[T: object](x: T)"
    of TraceH:
      buildErr c, info, "signature for '=trace' must be proc[T: object](x: var T; env: pointer)"
    of WasmovedH:
      buildErr c, info, "signature for '=wasMoved' must be proc[T: object](x: var T)"
    of CopyH:
      buildErr c, info, "signature for '=copy' must be proc[T: object](x: var T; y: T)"
    of SinkhH:
      buildErr c, info, "signature for '=sink' must be proc[T: object](x: var T; y: T)"
    of DupH:
      buildErr c, info, "signature for '=dup' must be proc[T: object](x: T): T"

proc hookToAttachedOp(op: HookKind): AttachedOp =
  case op
  of DestroyH: attachedDestroy
  of WasmovedH: attachedWasMoved
  of CopyH: attachedCopy
  of SinkhH: attachedSink
  of DupH: attachedDup
  of TraceH, NoHook: attachedTrace

proc registerHook(c: var SemContext; obj: SymId, symId: SymId, op: HookKind; isGeneric: bool) =
  let attachedOp = hookToAttachedOp(op)
  c.hookIndexLog[attachedOp].add HookIndexEntry(typ: obj, hook: symId, isGeneric: isGeneric)
  programs.registerHook(c.thisModuleSuffix, obj, attachedOp, symId, isGeneric)

proc getHookName(symId: SymId): string =
  result = pool.syms[symId]
  extractBasename(result)
  #result = result.normalize

proc semHook(c: var SemContext; name: string; beforeParams: int; symId: SymId, info: PackedLineInfo): TypeCursor =
  let params = getParamsType(c, beforeParams)
  case name
  of "=destroy":
    checkTypeHook(c, params, DestroyH, info)
    result = params[0]
  of "=wasMoved":
    checkTypeHook(c, params, WasmovedH, info)
    result = params[0]
  of "=trace":
    checkTypeHook(c, params, TraceH, info)
    result = params[0]
  of "=copy":
    checkTypeHook(c, params, CopyH, info)
    result = params[0]
  of "=sink":
    checkTypeHook(c, params, SinkhH, info)
    result = params[0]
  of "=dup":
    checkTypeHook(c, params, DupH, info)
    result = params[0]
  else:
    bug "unreachable"

proc hookToKind(name: string): HookKind =
  case name
  of "=destroy": DestroyH
  of "=wasMoved": WasmovedH
  of "=trace": TraceH
  of "=copy": CopyH
  of "=sink": SinkhH
  of "=dup": DupH
  else: NoHook

proc attachConverter(c: var SemContext; symId: SymId;
                     declStart, beforeExportMarker, beforeGenericParams: int; info: PackedLineInfo) =
  let root = nominalRoot(c.routine.returnType)
  if root == SymId(0) and not c.g.config.compat:
    var errBuf = createTokenBuf(16)
    swap c.dest, errBuf
    buildErr c, info, "cannot attach converter to type " & typeToString(c.routine.returnType)
    swap c.dest, errBuf
    c.dest.insert errBuf, declStart
  else:
    c.converters.mgetOrPut(root, @[]).add(symId)
    if c.dest[beforeExportMarker].kind != DotToken:
      # exported
      if not (c.dest[beforeGenericParams].kind == ParLe and
          pool.tags[c.dest[beforeGenericParams].tagId] == $InvokeT):
        # don't register instances
        c.converterIndexMap.add((root, symId))

proc attachMethod(c: var SemContext; symId: SymId;
                  declStart, beforeParams, beforeGenericParams: int; info: PackedLineInfo) =
  if c.currentScope.up.kind != ToplevelScope:
    buildErr c, info, "'method' is only allowed at top level"

  var params = cursorAt(c.dest, beforeParams)
  var root = SymId(0)
  var signature = StrId(0)
  if params.kind == ParLe:
    inc params
    if params.substructureKind == ParamU:
      inc params
      skip params # name
      skip params # export marker
      skip params # pragmas
      root = getClass(params) # can be a generic instance symbol
      var rest = params
      skip rest # type
      skip rest # default value
      skipParRi rest
      var methodName = pool.syms[symId]
      extractBasename methodName
      signature = pool.strings.getOrIncl(methodKey(methodName, rest))
  if root == SymId(0) or not isObjectType(root):
    let typ = typeToString(params)
    c.dest.endRead()
    var errBuf = createTokenBuf(16)
    swap c.dest, errBuf
    buildErr c, info, "cannot attach method to type " & typ
    swap c.dest, errBuf
    c.dest.insert errBuf, declStart
  else:
    c.dest.endRead()
    let methodIsInstance = c.dest[beforeGenericParams].kind == ParLe and c.dest[beforeGenericParams].tagId == TagId(InvokeT)
    var symToRegister = symId
    if methodIsInstance:
      symToRegister = c.dest[beforeGenericParams+1].symId
    c.methods.mgetOrPut(root, @[]).add(symToRegister)
    if not methodIsInstance:
      # don't register instances
      for i in 0..<c.classIndexMap.len:
        if c.classIndexMap[i].cls == root:
          c.classIndexMap[i].methods.add MethodIndexEntry(fn: symId, signature: signature)
          return
      c.classIndexMap.add ClassIndexEntry(cls: root, methods: @[MethodIndexEntry(fn: symId, signature: signature)])

proc hookThatShouldBeMethod(c: var SemContext; hk: HookKind; beforeParams: int): bool =
  case hk
  of DestroyH, TraceH:
    result = false
    var params = cursorAt(c.dest, beforeParams)
    if params.kind == ParLe:
      inc params
      if params.substructureKind == ParamU:
        inc params
        skip params # name
        skip params # export marker
        skip params # pragmas
        result = isInheritable(params, true)
    endRead(c.dest)
  else:
    result = false

proc semProcImpl(c: var SemContext; it: var Item; kind: SymKind; pass: PassKind; newName = NoSymId) =
  let info = it.n.info
  let declStart = c.dest.len
  takeToken c, it.n
  let beforeName = c.dest.len

  let symId: SymId
  let status: SymStatus
  if it.n.kind == DotToken:
    symId = newName
    status = OkNew
    c.dest.add symdefToken(symId, it.n.info)
    inc it.n
  else:
    (symId, status) = declareOverloadableSym(c, it, kind)

  let beforeExportMarker = c.dest.len
  wantExportMarker c, it.n
  if it.n.kind == DotToken:
    takeToken c, it.n
  else:
    buildErr c, it.n.info, "TR pattern not implemented"
    skip it.n
  c.routine = createSemRoutine(kind, c.routine)
  # 'break' and 'continue' are valid in a template regardless of whether we
  # really have a loop or not:
  if kind == TemplateY:
    inc c.routine.inLoop
    inc c.routine.inGeneric

  try:
    c.openScope() # open parameter scope
    let beforeGenericParams = c.dest.len
    semGenericParams c, it.n
    if c.routine.inGeneric > 0 and c.routine.parent.kind != NoSym and c.routine.parent.inGeneric == 0:
      c.genericInnerProcs.incl(symId)
    let beforeParams = c.dest.len
    semParams c, it.n
    c.routine.returnType = semReturnType(c, it.n)
    var crucial = CrucialPragma(sym: symId)
    semPragmas c, it.n, crucial, kind
    c.routine.pragmas = crucial.flags
    if crucial.hasVarargs.isValid:
      addVarargsParameter c, beforeParams, crucial.hasVarargs
    if crucial.magic.len > 0:
      exportMarkerBecomesNifTag c, beforeExportMarker, crucial
    if status == OkExistingFresh and InjectP in crucial.flags:
      # symbol is injected, add it to scope
      let s = Sym(kind: kind, name: symId, pos: beforeName)
      var name = pool.syms[symId]
      extractBasename(name)
      # go up a scope for the parameter scope:
      c.currentScope.up.addOverloadable(pool.strings.getOrIncl(name), s)
    if it.n.kind == DotToken:
      takeToken c, it.n
    else:
      buildErr c, it.n.info, "`effects` must be empty"
      skip it.n

    publishSignature c.dest, symId, declStart
    let hookName = getHookName(symId)
    let hk = hookToKind(hookName)
    if status in {OkNew, OkExistingFresh}:
      if kind == ConverterY:
        attachConverter c, symId, declStart, beforeExportMarker, beforeGenericParams, info
      elif kind == MethodY:
        attachMethod c, symId, declStart, beforeParams, beforeGenericParams, info
      elif hookThatShouldBeMethod(c, hk, beforeParams):
        c.dest[declStart] = parLeToken(MethodS, info)
        attachMethod c, symId, declStart, beforeParams, beforeGenericParams, info
    let beforeBody = c.dest.len
    if it.n.kind != DotToken:
      case pass
      of checkGenericInst:
        if it.n.stmtKind != StmtsS:
          bug "(stmts) expected, but got ", it.n
        c.openScope() # open body scope
        takeToken c, it.n
        var resId = SymId(0)
        if UntypedP in crucial.flags:
          # for untyped generic procs, need to add result symbol now
          resId = declareResult(c, it.n.info)
        semProcBody c, it
        c.closeScope() # close body scope
        c.closeScope() # close parameter scope
        if resId != SymId(0):
          addReturnResult c, resId, it.n.info

        if hk != NoHook:
          let params = getParamsType(c, beforeParams)
          assert params.len >= 1
          let obj = getObjSymId(c, params[0])
          registerHook(c, obj, symId, hk, false)

      of checkBody:
        if it.n.stmtKind != StmtsS:
          bug "(stmts) expected, but got ", it.n
        c.openScope() # open body scope
        var resId = SymId(0)
        if UntypedP in crucial.flags and c.routine.inGeneric > 0: # includes templates
          # should eventually be default for compat mode
          let mode = if kind == TemplateY: UntypedTemplate else: UntypedGeneric
          var ctx = createUntypedContext(addr c, mode)
          addParams(ctx, beforeGenericParams)
          addParams(ctx, beforeParams)
          semTemplBody ctx, it.n
        else:
          takeToken c, it.n
          resId = declareResult(c, it.n.info)
          semProcBody c, it
        c.closeScope() # close body scope
        c.closeScope() # close parameter scope
        addReturnResult c, resId, it.n.info
        let name = getHookName(symId)
        let hk = hookToKind(name)
        if hk != NoHook:
          let objCursor = semHook(c, hookName, beforeParams, symId, info)
          let obj = getObjSymId(c, objCursor)

          # because it's a hook for sure
          registerHook(c, obj, symId, hk, c.routine.inGeneric > 0)

      of checkSignatures:
        c.takeTree it.n
        c.closeScope() # close parameter scope
      of checkConceptProc:
        c.closeScope() # close parameter scope
        if it.n.kind == DotToken:
          inc it.n
        else:
          c.buildErr it.n.info, "inside a `concept` a routine cannot have a body"
          skip it.n
    else:
      if ErrorP in crucial.flags and pass in {checkGenericInst, checkBody}:
        let name = getHookName(symId)
        let hk = hookToKind(name)
        if hk != NoHook:
          let objCursor = semHook(c, name, beforeParams, symId, info)
          let obj = getObjSymId(c, objCursor)
          registerHook(c, obj, symId, hk, c.routine.inGeneric > 0)
        takeToken c, it.n
      elif BorrowP in crucial.flags and pass in {checkGenericInst, checkBody}:
        if kind notin {ProcY, FuncY, ConverterY, TemplateY, MethodY}:
          c.buildErr it.n.info, ".borrow only valid for proc, func, converter, template or method"
        else:
          semBorrow(c, symToIdent(symId), beforeParams)
        inc it.n # skip DotToken
      else:
        takeToken c, it.n
      c.closeScope() # close parameter scope
    if c.routine.hasDefer:
      transformDefer c.dest, beforeBody
  finally:
    c.routine = c.routine.parent
  takeParRi c, it.n
  if newName == NoSymId:
    producesVoid c, info, it.typ
  publish c, symId, declStart

proc findMacroInvocs(c: SemContext; n: Cursor; kind: SymKind): seq[Cursor] =
  # find all macro/template identifiers in pragmas to invoke them with parent proc definition
  result = newSeq[Cursor]()
  if kind in RoutineKinds:
    var n = asRoutine(n).pragmas
    if n.substructureKind == PragmasU:
      inc n
      while n.kind != ParRi:
        if n.exprKind == ErrX or n.substructureKind == KvU:
          skip n
        elif pragmaKind(n) != NoPragma or callConvKind(n) != NoCallConv:
          skip n
        else:
          let hasParRi = n.kind == ParLe
          let start = n
          if n.exprKind == CallX:
            inc n
          let name = getIdent(n)
          if name != StrId(0) and not (name in c.userPragmas and not hasParRi):
            result.add start
            n = start
            skip n
          else:
            skip n

proc transformMacroInvoc(c: var SemContext; it: var Item; macroInvocsPos: seq[Cursor]) =
  # transform `proc foo() {.macrofoo, macrobar.}` to `macrobar: macrofoo: proc foo()`.
  var inBuf = createTokenBuf()

  # adds last one in macroInvocsPos to buf first as it is invoked last.
  let info = it.n.info
  for i in countdown(macroInvocsPos.len - 1, 0):
    inBuf.addParLe CallX, info
    var n = macroInvocsPos[i]
    let isCall = n.exprKind == CallX
    if isCall:
      inc n
    assert n.kind == Ident
    inBuf.add n
    if isCall:
      inc n
      while n.kind != ParRi:
        inBuf.takeTree n
    inBuf.addParLe StmtsS, info

  var n = it.n
  # copies the proc def to inBuf excepts all macros and templates to avoid
  # recursive macro invocations and invocations of them at unexpected places.
  var nested = 0
  var i = 0
  while true:
    if i < macroInvocsPos.len and n == macroInvocsPos[i]:
      skip n
      inc i
    else:
      if n.kind == ParLe: inc nested
      inBuf.takeToken n
    if n.kind == ParRi:
      dec nested
      if nested == 0: break
  inBuf.addParRi
  for i in 0 ..< macroInvocsPos.len:
    inBuf.addParRi  # close StmtsS
    inBuf.addParRi  # close CallX
  var it2 = Item(n: cursorAt(inBuf, 0), typ: c.types.autoType)
  #echo "macro invoc in: ", toString it2.n
  #let lastDestLen = c.dest.len
  semCall c, it2, {}
  endRead inBuf
  #[
  if c.dest.len > lastDestLen:
    echo "macro invoc out: ", toString cursorAt(c.dest, lastDestLen)
    endRead c.dest
  else:
    echo "macro invoc out: empty"
  ]#
  it.n = n
  it.typ = it2.typ
  skipParRi it.n

proc semProc(c: var SemContext; it: var Item; kind: SymKind; pass: PassKind) =
  let macroInvocsPos = findMacroInvocs(c, it.n, kind)
  if macroInvocsPos.len > 0:
    if pass == checkBody:
      transformMacroInvoc(c, it, macroInvocsPos)
    else:
      c.takeTree it.n
  elif it.n.firstSon.kind == DotToken:
    # anon routine
    let info = it.n.firstSon.info
    let name = identToSym(c, "`anonproc", ProcY)

    var anons = createTokenBuf()
    swap c.dest, anons
    semProcImpl c, it, kind, pass, name
    swap c.dest, anons
    c.dest.add parLeToken(ExprX, info)
    c.dest.add parLeToken(StmtsS, info)
    let anonTypePos = c.dest.len
    c.dest.add anons
    c.dest.addParRi()
    c.dest.add symToken(name, info)
    c.dest.addParRi()
    it.typ = typeToCursor(c, c.dest, anonTypePos)

  else:
    semProcImpl c, it, kind, pass

proc semTypePragmas(c: var SemContext; n: var Cursor; sym: SymId; beforeExportMarker: int): CrucialPragma =
  result = CrucialPragma(sym: sym)
  semPragmas c, n, result, TypeY # 2
  if result.magic.len > 0:
    exportMarkerBecomesNifTag c, beforeExportMarker, result

proc fitTypeToPragmas(c: var SemContext; pragmas: CrucialPragma; typeStart: int) =
  if {ImportcP, ImportcppP} * pragmas.flags != {}:
    let typ = cursorAt(c.dest, typeStart)
    if isNominal(typ.typeKind):
      # ok
      endRead(c.dest)
    elif typ.typeKind in {IntT, UintT, FloatT, CharT, PointerT}:
      let info = typ.info
      endRead(c.dest)
      let kind = if ImportcP in pragmas.flags: ImportcP else: ImportcppP
      var tokens = @[
        parLeToken(kind, info),
        strToken(pool.strings.getOrIncl(pragmas.externName), info),
        parRiToken(info)
      ]
      if HeaderP in pragmas.flags:
        assert pragmas.headerFileTok.kind == StringLit
        tokens.add [
          parLeToken(HeaderP, info),
          pragmas.headerFileTok,
          parRiToken(info)
        ]
      c.dest.insert tokens, typeStart+2
    else:
      let err = "cannot import type " & typeToString(typ)
      let info = typ.info
      endRead(c.dest)
      c.buildErr info, err

proc buildInnerObjDecl(c: var SemContext; decl: Cursor; sym: var SymId): TokenBuf =
  ## build inner object type declaration from full ref/ptr object decl
  result = createTokenBuf(64)

  # make anon object symbol from `sym` and set `sym` to it:
  var isGlobal = false
  let basename = extractBasename(pool.syms[sym], isGlobal)
  var objName = basename & ".Obj"
  if isGlobal: c.makeGlobalSym(objName)
  else: c.makeLocalSym(objName)
  sym = pool.syms.getOrIncl(objName)

  var n = decl
  result.add n # (type
  inc n
  result.add symdefToken(sym, n.info)
  skip n # name
  takeTree result, n # copy exported (?)
  takeTree result, n # copy typevars
  # ^ may need to build fresh identifiers
  takeTree result, n # copy pragmas
  assert n.typeKind in {RefT, PtrT}
  inc n # (ref/ptr
  takeTree result, n # copy (object)
  skipParRi n # ) from ref/ptr
  takeParRi result, n # ) from type

proc invokeInnerObj(c: var SemContext; genericsPos: int; objSym: SymId; info: PackedLineInfo) =
  var params = cursorAt(c.dest, genericsPos)
  if params.substructureKind == TypevarsU:
    # build an invocation of the inner object type
    inc params
    var invokeBuf = createTokenBuf(16)
    invokeBuf.buildTree InvokeT, info:
      invokeBuf.add symToken(objSym, info)
      while params.kind != ParRi:
        let typevar = asTypevar(params).name
        if typevar.kind == SymbolDef:
          invokeBuf.add symToken(typevar.symId, typevar.info)
        else:
          # assume it was left as an identifier
          invokeBuf.addSubtree typevar
        skip params
    endRead(c.dest)
    c.dest.add invokeBuf
  else:
    # enough to use object sym directly
    endRead(c.dest)
    c.dest.add symToken(objSym, info)

proc semTypeSection(c: var SemContext; n: var Cursor) =
  let startCursor = n
  let declStart = c.dest.len
  takeToken c, n
  # name, export marker, generic params, pragmas, body
  var delayed = handleSymDef(c, n, TypeY) # 0
  let beforeExportMarker = c.dest.len
  wantExportMarker c, n # 1

  var isEnumTypeDecl = false
  var isRefPtrObj = false
  var innerObjDecl = default(TokenBuf)

  let beforeGenerics = c.dest.len
  var isGeneric: bool
  let prevGeneric = c.routine.inGeneric
  let prevInst = c.routine.inInst
  if n.kind == DotToken:
    takeToken c, n
    isGeneric = false
  else:
    let oldScopeKind = c.currentScope.kind
    openScope c
    semGenericParams c, n
    # copy toplevel scope status for exported fields
    c.currentScope.kind = oldScopeKind
    isGeneric = true

  let crucial = semTypePragmas(c, n, delayed.s.name, beforeExportMarker)
  if delayed.status == OkExistingFresh and InjectP in crucial.flags:
    # symbol is injected, add it to scope
    delayed.status = OkNew

  if c.phase == SemcheckSignatures or
      (delayed.status in {OkNew, OkExistingFresh} and
        c.phase != SemcheckTopLevelSyms):
    # body:
    if n.kind == DotToken:
      takeToken c, n
    else:
      let typeStart = c.dest.len
      case n.typeKind
      of EnumT, HoleyEnumT:
        semEnumType(c, n, delayed.s.name, beforeExportMarker)
        isEnumTypeDecl = true
      of RefT, PtrT:
        var obj = n
        inc obj
        if obj.typeKind == ObjectT:
          isRefPtrObj = true
          var objSym = delayed.s.name
          innerObjDecl = buildInnerObjDecl(c, startCursor, objSym)
          takeToken c, n # ref/ptr tag
          invokeInnerObj(c, beforeGenerics, objSym, n.info)
          skip n
          takeParRi c, n
        else:
          semLocalTypeImpl c, n, InTypeSection
      else:
        semLocalTypeImpl c, n, InTypeSection
      fitTypeToPragmas(c, crucial, typeStart)
  else:
    if n.typeKind in {RefT, PtrT}:
      var obj = n
      inc obj
      if obj.typeKind == ObjectT:
        # handle these here too for better forward decls
        isRefPtrObj = true
        var objSym = delayed.s.name
        innerObjDecl = buildInnerObjDecl(c, startCursor, objSym)
        takeToken c, n # ref/ptr tag
        invokeInnerObj(c, beforeGenerics, objSym, n.info)
        skip n
        takeParRi c, n
    if not isRefPtrObj: # body not already handled
      if isGeneric:
        # capture typevars for instantiation of forward declared types to work
        var ctx = createUntypedContext(addr c, UntypedForwardGeneric)
        addParams(ctx, beforeGenerics)
        semTemplBody ctx, n # body
      else:
        c.takeTree n # body
  if isGeneric:
    closeScope c
    c.routine.inGeneric = prevGeneric # revert increase by semGenericParams
    c.routine.inInst = prevInst

  c.addSym delayed
  takeParRi c, n


  publish c, delayed.s.name, declStart

  if isEnumTypeDecl:
    var enumTypeDecl = tryLoadSym(delayed.s.name)
    assert enumTypeDecl.status == LacksNothing
    var pending = createTokenBuf()
    swap c.dest, pending
    genEnumToStrProc(c, enumTypeDecl.decl)
    swap c.dest, pending

    var dollorProcDecl = beginRead(pending)
    var it = Item(n: dollorProcDecl, typ: c.types.autoType)
    # semchecking is needed for publishing signature
    # and transforming `ret ...` into `ret result` for `controlflow.nim`
    semExpr(c, it)

  if isRefPtrObj:
    if c.phase > SemcheckTopLevelSyms:
      var topLevelDest = createTokenBuf(64)
      var topLevelRead = beginRead(innerObjDecl)
      var phase = SemcheckTopLevelSyms
      swap c.phase, phase
      swap c.dest, topLevelDest
      semTypeSection c, topLevelRead
      swap c.dest, topLevelDest
      swap c.phase, phase
      innerObjDecl = topLevelDest
    if c.phase > SemcheckSignatures:
      # need to go through signature phase if not applied yet since decl already has sym
      var sigDest = createTokenBuf(64)
      var sigRead = beginRead(innerObjDecl)
      var phase = SemcheckSignatures
      swap c.phase, phase
      swap c.dest, sigDest
      semTypeSection c, sigRead
      swap c.dest, sigDest
      swap c.phase, phase
      innerObjDecl = sigDest
    var decl = beginRead(innerObjDecl)
    semTypeSection c, decl

proc addTupleAccess(buf: var TokenBuf; lvalue: SymId; i: int; info: PackedLineInfo) =
  buf.add parLeToken(TupatX, info)
  buf.add symToken(lvalue, info)
  buf.addIntLit(i, info)
  buf.addParRi()

proc semUnpackDecl(c: var SemContext; it: var Item) =
  case c.phase
  of SemcheckTopLevelSyms:
    c.takeTree it.n
    return
  of SemcheckSignatures:
    var kindTag = it.n
    while kindTag.stmtKind == UnpackdeclS:
      inc kindTag # unpackdecl tag
      skip kindTag # value
      assert kindTag.substructureKind == UnpacktupU
      inc kindTag # unpacktup tag
    let kind = kindTag.symKind
    if kind != ConstY:
      c.takeTree it.n
      return
  of SemcheckBodies: discard

  let info = it.n.info
  inc it.n # skip tag
  var tup = Item(n: it.n, typ: c.types.autoType)
  let tupInfo = tup.n.info
  var tupBuf = createTokenBuf(16)
  swap c.dest, tupBuf
  semExpr c, tup
  swap c.dest, tupBuf
  it.n = tup.n
  let tupleType = skipModifier(tup.typ)
  if tupleType.typeKind != TupleT:
    c.buildErr tupInfo, "expected tuple for tuple unpacking"
    skipToEnd it.n
    return
  if it.n.substructureKind != UnpacktupU:
    error "illformed AST: `unpacktup` inside `unpackdecl` expected, got ", it.n
  inc it.n # skip unpacktup tag
  var kindTag = it.n
  while kindTag.stmtKind == UnpackdeclS:
    # skip nested unpacks as well
    inc kindTag # unpackdecl tag
    skip kindTag # value
    assert kindTag.substructureKind == UnpacktupU
    inc kindTag # unpacktup tag
  let kind = kindTag.symKind
  let tmpName = identToSym(c, "`tmptup", kind)

  # build local for tuple:
  let tmpStart = c.dest.len
  c.dest.buildTree kind, info:
    c.dest.add symdefToken(tmpName, info) # 0: name
    c.dest.addDotToken() # 1: export
    c.dest.addDotToken() # 2: pragma
    c.dest.addSubtree tupleType # 3: type
    c.dest.add tupBuf # 4: value
  publish c, tmpName, tmpStart

  # iterate over unpacktup:
  var declBuf = createTokenBuf(32)
  var i = 0
  while it.n.kind != ParRi:
    let declInfo = it.n.info
    if it.n.stmtKind == UnpackdeclS:
      declBuf.add it.n
      inc it.n
      assert it.n.kind == DotToken # value
      inc it.n
      declBuf.addTupleAccess(tmpName, i, declInfo)
      takeTree declBuf, it.n
      takeParRi declBuf, it.n
    else:
      declBuf.add it.n
      inc it.n
      takeTree declBuf, it.n # 0: name
      takeTree declBuf, it.n # 1: export
      takeTree declBuf, it.n # 2: pragma
      takeTree declBuf, it.n # 3: type
      assert it.n.kind == DotToken # value
      inc it.n
      declBuf.addTupleAccess(tmpName, i, declInfo) # 4: value
      takeParRi declBuf, it.n
    var decl = cursorAt(declBuf, 0)
    semStmt c, decl, false
    endRead(declBuf)
    declBuf.shrink 0
    inc i
  skipParRi it.n # close unpacktup
  skipParRi it.n # close unpackdecl
  producesVoid c, info, it.typ

proc semUsing(c: var SemContext; n: var Cursor) =
  takeToken c, n
  while n.kind != ParRi:
    assert n.substructureKind == FldU
    takeToken c, n
    var ident = StrId(0)
    if n.kind == Ident:
      ident = n.litId
      takeToken c, n
    else:
      c.buildErr n.info, "identifier is expected", n
      skip n

    # export marker
    if n.kind == DotToken:
      takeToken c, n
    else:
      c.buildErr n.info, "identifiers under using statements cannot be exported", n
      skip n

    # pragma
    # currently no pragmas can be used in using statements
    if n.kind == DotToken:
      takeToken c, n
    elif n.substructureKind == PragmasU:
      c.buildErr n.info, "using statements supports no pragmas", n
      skip n
    else:
      c.buildErr n.info, "illformed AST inside using statement", n
      skip n

    let typ = semLocalType(c, n)
    if ident != StrId(0):
      c.usingStmtMap[ident] = typ

    if n.kind == DotToken:
      takeToken c, n
    else:
      c.buildErr n.info, "illformed AST inside using statement", n
      skip n

    takeParRi c, n

  takeParRi c, n

proc semDo(c: var SemContext; it: var Item; pass: PassKind) =
  let info = it.n.info
  inc it.n
  var anons = createTokenBuf()
  # transform the do notation to an anon proc
  anons.addParLe(ProcS, info)
  anons.addEmpty info # name
  anons.addEmpty3 info # export, pattern, typevars
  anons.takeTree it.n  # params
  anons.takeTree it.n  # return type
  anons.addEmpty info  # pragma
  anons.addEmpty info  # effects
  anons.takeTree it.n  # body
  anons.takeParRi it.n
  var anonIt = Item(n: beginRead(anons), typ: it.typ)
  semProc c, anonIt, ProcY, pass
  endRead anons
  it.typ = anonIt.typ
