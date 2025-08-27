type FieldsIter = object
  nameVar, fieldVar1, fieldVar2: StrId
    # can be SymId if loopvars/body are processed before substituting
  obj1, obj2: Cursor

proc buildNamedFieldIter(buf: var TokenBuf; iter: FieldsIter; fieldName: StrId; fieldSym: SymId; body: Cursor) =
  # use `if true` to open a new scope without interfering with `break`:
  buf.addParLe(IfS, body.info)
  buf.addParLe(ElifU, body.info)
  buf.addParLe(TrueX, body.info)
  buf.addParRi()
  buf.addParLe(StmtsS, body.info)
  var nested = 0
  var n = body
  while true:
    case n.kind
    of UnknownToken, EofToken, DotToken, StringLit, CharLit, IntLit, UIntLit, FloatLit, Symbol, SymbolDef:
      buf.add n
    of Ident:
      # substitute direct idents for now, symbols would work the same way 
      let s = n.litId
      if s == iter.nameVar:
        if fieldSym == SymId(0):
          buf.add strToken(fieldName, n.info)
        else:
          buf.addStrLit pool.syms[fieldSym]
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
  buf.addParRi() # (stmts)
  buf.addParRi() # (elif)
  buf.addParRi() # (if)

proc buildTupleFieldIter(buf: var TokenBuf; iter: FieldsIter; i: int; name: StrId; body: Cursor) =
  # use `if true` to open a new scope without interfering with `break`:
  buf.addParLe(IfS, body.info)
  buf.addParLe(ElifU, body.info)
  buf.addParLe(TrueX, body.info)
  buf.addParRi()
  buf.addParLe(StmtsS, body.info)
  let intId = pool.integers.getOrIncl(i)
  var nested = 0
  var n = body
  while true:
    case n.kind
    of UnknownToken, EofToken, DotToken, StringLit, CharLit, IntLit, UIntLit, FloatLit, Symbol, SymbolDef:
      buf.add n
    of Ident:
      # substitute direct idents for now, symbols would work the same way 
      let s = n.litId
      if s == iter.nameVar:
        buf.add strToken(name, n.info)
      elif s == iter.fieldVar1:
        buf.addParLe(TupAtX, n.info)
        buf.addSubtree iter.obj1
        buf.add intToken(intId, n.info)
        buf.addParRi()
      elif s == iter.fieldVar2:
        buf.addParLe(TupAtX, n.info)
        buf.addSubtree iter.obj2
        buf.add intToken(intId, n.info)
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
  buf.addParRi() # (stmts)
  buf.addParRi() # (elif)
  buf.addParRi() # (if)

proc semForFields(c: var SemContext; it: var Item; call, orig: Cursor) =
  let fieldPairs = call.exprKind in {FieldPairsX, InternalFieldPairsX}
  let isInternalSym = call.exprKind == InternalFieldPairsX
  var iter = FieldsIter()
  let unpackInfo = it.n.info
  case it.n.substructureKind
  of UnpackflatU, UnpacktupU:
    inc it.n
    # take direct ident names for now,
    # defining and substituting full symbols would need prepass: 
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
  # it.n fully skipped

  let isTuple = objType.typeKind == TupleT
  if isTuple:
    discard "ok"
  else:
    # check if object or typevar
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
        var origRead = orig
        semTemplBody ctx, origRead
        producesVoid c, orig.info, it.typ
        return
      if objType.typeKind != ObjectT:
        c.buildErr call.info, "cannot iterate over fields of type: " & typeToString(objType)
        return
    else:
      c.buildErr call.info, "cannot iterate over fields of type: " & typeToString(objType)
      return

  var iterBuf = createTokenBuf(64)
  # use while loop so `break` works:
  iterBuf.addParLe(WhileS, body.info)
  iterBuf.addParLe(TrueX, body.info)
  iterBuf.addParRi()
  iterBuf.addParLe(StmtsS, body.info)

  if isTuple:
    var tup = objType
    inc tup
    var i = 0
    while tup.kind != ParRi:
      let fld = asTupleField(tup)
      let name =
        if not cursorIsNil(fld.name):
          getIdent(fld.name)
        else:
          pool.strings.getOrIncl("Field" & $(i+1))
      buildTupleFieldIter(iterBuf, iter, i, name, body)
      skip tup
      inc i
  else: # object
    # same order as original nim fields iterator:
    while true:
      var obj = asObjectDecl(objType)
      var currentField = obj.firstField
      if currentField.kind != DotToken:
        while currentField.kind != ParRi:
          let field = takeLocal(currentField, SkipfinalParRi)
          let fieldSym = if isInternalSym: field.name.symId else: SymId(0)
          # field name is enough:
          buildNamedFieldIter(iterBuf, iter, getIdent(field.name), fieldSym, body)
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
