type FieldsIter = object
  nameVar, fieldVar1, fieldVar2: StrId
    # can be SymId if loopvars/body are processed before substituting
  obj1, obj2: Cursor

proc expandNamedFieldBody(buf: var TokenBuf; iter: FieldsIter; fieldName: StrId; fieldSym: SymId; body: Cursor) =
  ## Copies the single tree/token at `body` into `buf`, substituting the
  ## loop variables.
  var n = body
  case n.kind
  of Ident:
    # substitute direct idents for now, symbols would work the same way
    let s = n.strId
    if s == iter.nameVar:
      if fieldSym == SymId(0):
        buf.addStrLit(fieldName, n.info)
      else:
        buf.addStrLit pool.syms[fieldSym]
    elif s == iter.fieldVar1:
      buf.addParLe(DotX, n.info)
      buf.addSubtree iter.obj1
      buf.addIdent(fieldName, n.info)
      buf.addParRi()
    elif s == iter.fieldVar2:
      buf.addParLe(DotX, n.info)
      buf.addSubtree iter.obj2
      buf.addIdent(fieldName, n.info)
      buf.addParRi()
    else:
      buf.addIdent(s, n.info)
  of TagLit:
    buf.addParLe(n.cursorTagId, n.info)
    n.into:
      while n.hasMore:
        expandNamedFieldBody(buf, iter, fieldName, fieldSym, n)
        skip n
      buf.addParRi(n.endInfo)
  else:
    # classic: a real ParRi cannot happen (subtree ends are consumed by the
    # bounded scope); nifcore has no ParRi kind at all.
    buf.addSubtree n

proc buildNamedFieldIter(buf: var TokenBuf; iter: FieldsIter; fieldName: StrId; fieldSym: SymId; body: Cursor) =
  # use `if true` to open a new scope without interfering with `break`:
  buf.addParLe(IfS, body.info)
  buf.addParLe(ElifU, body.info)
  buf.addParLe(TrueX, body.info)
  buf.addParRi()
  buf.addParLe(StmtsS, body.info)
  expandNamedFieldBody(buf, iter, fieldName, fieldSym, body)
  buf.addParRi() # (stmts)
  buf.addParRi() # (elif)
  buf.addParRi() # (if)

proc expandTupleFieldBody(buf: var TokenBuf; iter: FieldsIter; intVal: int64; name: StrId; body: Cursor) =
  ## Copies the single tree/token at `body` into `buf`, substituting the
  ## loop variables.
  var n = body
  case n.kind
  of Ident:
    # substitute direct idents for now, symbols would work the same way
    let s = n.strId
    if s == iter.nameVar:
      buf.addStrLit(name, n.info)
    elif s == iter.fieldVar1:
      buf.addParLe(TupatX, n.info)
      buf.addSubtree iter.obj1
      buf.addIntLit(intVal, n.info)
      buf.addParRi()
    elif s == iter.fieldVar2:
      buf.addParLe(TupatX, n.info)
      buf.addSubtree iter.obj2
      buf.addIntLit(intVal, n.info)
      buf.addParRi()
    else:
      buf.addIdent(s, n.info)
  of TagLit:
    buf.addParLe(n.cursorTagId, n.info)
    n.into:
      while n.hasMore:
        expandTupleFieldBody(buf, iter, intVal, name, n)
        skip n
      buf.addParRi(n.endInfo)
  else:
    # classic: a real ParRi cannot happen (subtree ends are consumed by the
    # bounded scope); nifcore has no ParRi kind at all.
    buf.addSubtree n

proc buildTupleFieldIter(buf: var TokenBuf; iter: FieldsIter; i: int; name: StrId; body: Cursor) =
  # use `if true` to open a new scope without interfering with `break`:
  buf.addParLe(IfS, body.info)
  buf.addParLe(ElifU, body.info)
  buf.addParLe(TrueX, body.info)
  buf.addParRi()
  buf.addParLe(StmtsS, body.info)
  expandTupleFieldBody(buf, iter, int64(i), name, body)
  buf.addParRi() # (stmts)
  buf.addParRi() # (elif)
  buf.addParRi() # (if)

proc semForFields(c: var SemContext; dest: var TokenBuf; it: var Item; call, orig: Cursor;
                  forStart: Cursor) =
  ## `it.n` sits inside the `(for …)` scope entered by `semFor`; this proc
  ## consumes the rest of that scope, including its close (via `forStart`).
  template bailOut(msg: string) =
    buildErr c, dest, unpackInfo, msg
    while it.n.hasMore: skip it.n
    it.n = forStart; skip it.n
    return

  let fieldPairs = call.exprKind in {FieldpairsX, InternalFieldPairsX}
  let isInternalSym = call.exprKind == InternalFieldPairsX
  var iter = FieldsIter()
  let unpackInfo = it.n.info
  case it.n.substructureKind
  of UnpackflatU, UnpacktupU:
    # take direct ident names for now,
    # defining and substituting full symbols would need prepass:
    var names: seq[StrId] = @[]
    it.n.into:
      while it.n.hasMore:
        let loopvar = takeLocal(it.n, SkipFinalParRi)
        names.add getIdent(loopvar.name)
    if fieldPairs:
      if names.len == 2 or names.len == 3:
        iter.nameVar = names[0]
        iter.fieldVar1 = names[1]
        if names.len == 3:
          iter.fieldVar2 = names[2]
      else:
        bailOut "wrong number of variables"
    else:
      if names.len == 1 or names.len == 2:
        iter.fieldVar1 = names[0]
        if names.len == 2:
          iter.fieldVar2 = names[1]
      else:
        bailOut "wrong number of variables"
  else:
    bailOut "illformed AST: `unpackflat` or `unpacktup` inside `for` expected"

  var callArgs = call # call is typed magic so we don't have to call getType
  callArgs = sub(callArgs) # bound to the call's children; never left,
                           # the cursors below only mark subtree starts
  var objType = callArgs
  skip callArgs
  iter.obj1 = callArgs
  skip callArgs
  if callArgs.hasMore:
    iter.obj2 = callArgs
    if iter.fieldVar2 == StrId(0):
      bailOut "wrong number of variables"
  elif iter.fieldVar2 != StrId(0):
    bailOut "wrong number of variables"
  let body = it.n
  skip it.n
  it.n = forStart; skip it.n
  # it.n fully skipped

  let isTuple = objType.typeKind == TupleT
  if isTuple:
    discard "ok"
  else:
    # check if object or typevar
    if objType.typeKind in {RefT, PtrT}: inc objType
    discard skipInvoke(objType)
    var objDecl = default(TypeDecl)
    if objType.isSymbol:
      objDecl = getTypeSection(objType.symId)
      if objDecl.kind == TypeY:
        objType = objDecl.objBody
      elif objDecl.kind == TypevarY:
        # iterating over fields of typevar, leave entire loop completely untyped
        var ctx = createUntypedContext(addr c, UntypedGeneric)
        var origRead = orig
        semTemplBody ctx, dest, origRead
        producesVoid c, dest, orig.info, it.typ
        return
      if objType.typeKind != ObjectT:
        c.buildErr dest, call.info, "cannot iterate over fields of type: " & typeToString(objType)
        return
    else:
      c.buildErr dest, call.info, "cannot iterate over fields of type: " & typeToString(objType)
      return

  var iterBuf = createTokenBuf(64)
  # use while loop so `break` works:
  iterBuf.addParLe(WhileS, body.info)
  iterBuf.addParLe(TrueX, body.info)
  iterBuf.addParRi()
  iterBuf.addParLe(StmtsS, body.info)

  if isTuple:
    var tup = objType
    var i = 0
    tup.into:
      while tup.hasMore:
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
      var currentField = obj.body
      currentField.into:
        if obj.kind == ObjectT:
          skip currentField, AnyType  # parent type / inheritance slot
        while currentField.hasMore:
          let field = takeLocal(currentField, SkipFinalParRi)
          let fieldSym = if isInternalSym: field.name.symId else: SymId(0)
          # field name is enough:
          buildNamedFieldIter(iterBuf, iter, getIdent(field.name), fieldSym, body)
      objType = obj.parentType
      if objType.isDotToken:
        break
      else:
        if objType.typeKind in {RefT, PtrT}: inc objType
        discard skipInvoke(objType)
        var parentDecl = default(TypeDecl)
        if objType.isSymbol:
          parentDecl = getTypeSection(objType.symId)
          if parentDecl.kind == TypeY:
            objType = parentDecl.objBody
          else:
            error "invalid parent object type", objType
        else:
          error "invalid parent object type", objType

  iterBuf.addParLe(BreakS, body.info)
  iterBuf.addDotToken(body.info)
  iterBuf.addParRi()
  iterBuf.addParRi() # (stmts)
  iterBuf.addParRi() # (while)

  var loop = beginRead(iterBuf)
  semStmt c, dest, loop, false
  producesVoid c, dest, orig.info, it.typ
