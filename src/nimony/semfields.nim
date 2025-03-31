type FieldsIter = object
  nameVar, fieldVar1, fieldVar2: StrId
  obj1, obj2: Cursor

proc buildFieldIter(buf: var TokenBuf; iter: FieldsIter; fieldName: StrId; body: Cursor) =
  buf.addParLe(ScopeS, body.info)
  var nested = 0
  var n = body
  while true:
    case n.kind
    of UnknownToken, EofToken, DotToken, StringLit, CharLit, IntLit, UIntLit, FloatLit, Symbol, SymbolDef:
      buf.add n
    of Ident:
      let s = n.litId
      if s == iter.nameVar:
        buf.add strToken(fieldName, n.info)
      elif s == iter.fieldVar1:
        buf.addParLe(DotX, n.info)
        buf.addSubtree iter.obj1
        buf.add identToken(fieldName, n.info)
        buf.addParRi()
      elif s == iter.fieldVar2:
        buf.addParLe(DotX, n.info)
        buf.addSubtree iter.obj2
        buf.add identToken(fieldName, n.info)
        buf.addParRi()
      else:
        buf.add n
    of ParLe:
      buf.add n
      inc nested
    of ParRi:
      buf.add n
      dec nested
    if nested == 0: break
    inc n
  buf.addParRi()

proc semForFields(c: var SemContext; it: var Item; call, orig: Cursor) =
  let fieldPairs = call.exprKind == FieldPairsX
  var iter = FieldsIter()
  let unpackInfo = it.n.info
  case it.n.substructureKind
  of UnpackflatU, UnpacktupU:
    inc it.n
    var names: seq[StrId] = @[]
    while it.n.kind != ParRi:
      let loopvar = takeLocal(it.n, SkipFinalParRi)
      names.add getIdent(loopvar.name)
    skipParRi it.n
    if fieldPairs:
      if names.len == 2 or names.len == 3:
        iter.nameVar = names[0]
        iter.fieldVar1 = names[1]
        if names.len == 3:
          iter.fieldVar2 = names[2]
      else:
        buildErr c, unpackInfo, "wrong number of variables"
        skipToEnd it.n
        return
    else:
      if names.len == 1 or names.len == 2:
        iter.fieldVar1 = names[0]
        if names.len == 2:
          iter.fieldVar2 = names[1]
      else:
        buildErr c, unpackInfo, "wrong number of variables"
        skipToEnd it.n
        return
  else:
    buildErr c, unpackInfo, "illformed AST: `unpackflat` or `unpacktup` inside `for` expected"
    skipToEnd it.n
    return

  var objType = call # call is typed magic so we don't have to call getType
  inc objType
  var obj1 = objType
  skip obj1
  iter.obj1 = obj1
  var obj2 = obj1
  skip obj2
  if obj2.kind != ParRi:
    iter.obj2 = obj2
    if iter.fieldVar2 == StrId(0):
      buildErr c, unpackInfo, "wrong number of variables"
      skipToEnd it.n
      return
  elif iter.fieldVar2 != StrId(0):
    buildErr c, unpackInfo, "wrong number of variables"
    skipToEnd it.n
    return
  let body = it.n
  skip it.n
  skipParRi it.n

  if objType.typeKind in {RefT, PtrT}: inc objType
  discard skipInvoke(objType)
  var objDecl = default(TypeDecl)
  if objType.kind == Symbol:
    objDecl = getTypeSection(objType.symId)
    if objDecl.kind == TypeY:
      objType = objDecl.objBody
    elif objDecl.kind == TypevarY:
      # iterating over fields of typevar, leave entire loop completely untyped
      var ctx = createUntypedContext(addr c, UntypedGeneric)
      var bodyRead = body
      semTemplBody ctx, bodyRead
      it.typ = c.types.untypedType
      return
    if objType.typeKind != ObjectT:
      c.buildErr call.info, "cannot iterate over fields of type: " & typeToString(objType)
      skipToEnd it.n
      return
  else:
    c.buildErr call.info, "cannot iterate over fields of type: " & typeToString(objType)
    skipToEnd it.n
    return

  var iterBuf = createTokenBuf(64)
  iterBuf.addParLe(WhileS, body.info)
  iterBuf.addParLe(TrueX, body.info)
  iterBuf.addParRi()
  iterBuf.addParLe(StmtsS, body.info)

  while true:
    var obj = asObjectDecl(objType)
    var currentField = obj.firstField
    if currentField.kind != DotToken:
      while currentField.kind != ParRi:
        let field = takeLocal(currentField, SkipfinalParRi)
        buildFieldIter(iterBuf, iter, getIdent(field.name), body)
    objType = obj.parentType
    if objType.kind == DotToken:
      break
    else:
      if objType.typeKind in {RefT, PtrT}: inc objType
      discard skipInvoke(objType)
      var parentDecl = default(TypeDecl)
      if objType.kind == Symbol:
        parentDecl = getTypeSection(objType.symId)
        if parentDecl.kind == TypeY:
          objType = parentDecl.objBody
        else:
          error "invalid parent object type", objType
      else:
        error "invalid parent object type", objType

  iterBuf.addParLe(BreakS, body.info)
  iterBuf.add dotToken(body.info)
  iterBuf.addParRi()
  iterBuf.addParRi() # (stmts)
  iterBuf.addParRi() # (while)

  var loop = beginRead(iterBuf)
  semStmt c, loop, false
  producesVoid c, orig.info, it.typ
