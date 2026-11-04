type FieldsIter = object
  nameVar, fieldVar1, fieldVar2: StrId
    # can be SymId if loopvars/body are processed before substituting
  obj1, obj2: Cursor

proc addFieldsAccessToken(buf: var TokenBuf; info: NifLineInfo; ownerSym: SymId) =
  ## `fields()` is compiler reflection: it enumerates EVERY field of the
  ## object, private ones included, and that is the point — it is what lets a
  ## generic `==`/`$` in `system` work on a foreign module's private fields.
  ## The field name came from walking the type's own declaration rather than
  ## from user code, so stamp the synthesized `(dot obj field)` with the module
  ## that declares the field: that is who authorizes the access. Without a
  ## token it would be judged against the module the `for` loop was written
  ## in, which for a generic `==` is `system`, and rejected (#1988).
  buf.addIntLit(0, info)
  let ownerModule = pool.symModule(ownerSym)
  if ownerModule.len > 0:
    buf.addStrLit(ownerModule, info)
  else:
    # A field of a type declared in the module being compiled has a local
    # symbol: its module suffix is empty even though the type has one. This
    # access was synthesized from the field declaration, so mark it as already
    # authorized instead of losing the owner module and re-judging it against
    # whichever module later re-checks the generated tree.
    buf.addStrLit(AlreadyChecked, info)

proc expandNamedFieldBody(buf: var TokenBuf; iter: FieldsIter; fieldName: StrId; fieldSym, ownerSym: SymId; internalName: bool; body: Cursor) =
  ## Copies the single tree/token at `body` into `buf`, substituting the
  ## loop variables.
  var n = body
  case n.kind
  of Ident:
    # substitute direct idents for now, symbols would work the same way
    let s = n.strId
    if s == iter.nameVar:
      if internalName and fieldSym != SymId(0):
        buf.addStrLit pool.symString(fieldSym)
      else:
        buf.addStrLit(fieldName, n.info)
    elif s == iter.fieldVar1:
      buf.addParLe(DotX, n.info)
      buf.addSubtree iter.obj1
      if fieldSym != SymId(0):
        # A resolved field symbol both avoids a second lookup and is the
        # compiler's certificate that guarded (sum-type) access is intentional.
        buf.addSymUse(fieldSym, n.info)
      else:
        buf.addIdent(fieldName, n.info)
      addFieldsAccessToken(buf, n.info, ownerSym)
      buf.addParRi()
    elif s == iter.fieldVar2:
      buf.addParLe(DotX, n.info)
      buf.addSubtree iter.obj2
      if fieldSym != SymId(0):
        buf.addSymUse(fieldSym, n.info)
      else:
        buf.addIdent(fieldName, n.info)
      addFieldsAccessToken(buf, n.info, ownerSym)
      buf.addParRi()
    else:
      buf.addIdent(s, n.info)
  of TagLit:
    buf.addParLe(n.cursorTagId, n.info)
    n.into:
      while n.hasMore:
        expandNamedFieldBody(buf, iter, fieldName, fieldSym, ownerSym,
                             internalName, n)
        skip n
      buf.addParRi(n.endInfo)
  else:
    # classic: a real ParRi cannot happen (subtree ends are consumed by the
    # bounded scope); nifcore has no ParRi kind at all.
    buf.addSubtree n

proc buildNamedFieldIter(buf: var TokenBuf; iter: FieldsIter; fieldName: StrId; fieldSym, ownerSym: SymId; internalName: bool; body: Cursor) =
  # use `if true` to open a new scope without interfering with `break`:
  buf.addParLe(IfS, body.info)
  buf.addParLe(ElifU, body.info)
  buf.addParLe(TrueX, body.info)
  buf.addParRi()
  buf.addParLe(StmtsS, body.info)
  expandNamedFieldBody(buf, iter, fieldName, fieldSym, ownerSym, internalName,
                       body)
  buf.addParRi() # (stmts)
  buf.addParRi() # (elif)
  buf.addParRi() # (if)

proc addObjectFieldAccess(buf: var TokenBuf; info: NifLineInfo; obj: Cursor;
                          fieldName: StrId; fieldSym, ownerSym: SymId) =
  ## Emit `obj.field` using the declaring module as the access token. This is
  ## the same privileged access used by the field body expansion below.
  buf.addParLe(DotX, info)
  buf.addSubtree obj
  if fieldSym != SymId(0):
    buf.addSymUse(fieldSym, info)
  else:
    buf.addIdent(fieldName, info)
  addFieldsAccessToken(buf, info, ownerSym)
  buf.addParRi()

proc emitObjectComponent(buf: var TokenBuf; iter: FieldsIter; n: var Cursor;
                         isInternalSym: bool; body: Cursor)

proc emitObjectBody(buf: var TokenBuf; iter: FieldsIter; n: var Cursor;
                    isInternalSym: bool; body: Cursor) =
  ## Emit the field iterations for one object-body fragment. The fragment is
  ## either a single field, `(stmts ...)`, `(nil)`, or a nested case section.
  case n.substructureKind
  of StmtsU:
    n.into:
      while n.hasMore:
        emitObjectComponent(buf, iter, n, isInternalSym, body)
  of NilU:
    discard
  of FldU, GfldU:
    emitObjectComponent(buf, iter, n, isInternalSym, body)
  of CaseU:
    let caseStart = n
    n.into:
      # The discriminator is a normal field and must still be visible to the
      # loop body. The remaining fields are only live in the selected branch.
      var selectorName = StrId(0)
      var selectorOwner = SymId(0)
      if n.hasMore and n.substructureKind in {FldU, GfldU}:
        let selectorStart = n
        let field = takeLocal(n, SkipFinalParRi)
        selectorName = getIdent(field.name)
        selectorOwner = field.name.symId
        buildNamedFieldIter(buf, iter, selectorName, field.name.symId,
                            field.name.symId, isInternalSym, body)
        n = selectorStart
        skip n
      if selectorName != StrId(0):
        buf.addParLe(CaseS, n.info)
        addObjectFieldAccess(buf, n.info, iter.obj1, selectorName,
                             selectorOwner, selectorOwner)
        while n.hasMore and n.substructureKind == OfU:
          let ofStart = n
          buf.addParLe(OfU, n.info)
          n.into:
            if n.hasMore:
              # Keep the original branch values verbatim: they may contain
              # symbols and ranges that have already been resolved.
              buf.addSubtree n
              skip n
            buf.addParLe(StmtsS, n.info)
            if n.hasMore:
              emitObjectBody(buf, iter, n, isInternalSym, body)
            buf.addParRi() # (stmts)
          buf.addParRi() # (of)
          n = ofStart
          skip n
        if n.hasMore and n.substructureKind == ElseU:
          let elseStart = n
          buf.addParLe(ElseU, n.info)
          n.into:
            buf.addParLe(StmtsS, n.info)
            if n.hasMore:
              emitObjectBody(buf, iter, n, isInternalSym, body)
            buf.addParRi() # (stmts)
          buf.addParRi() # (else)
          n = elseStart
          skip n
        buf.addParRi() # (case)
      while n.hasMore:
        skip n
    n = caseStart
    skip n
  of WhenU:
    # An unresolved object `when` only occurs in an uninstantiated generic
    # object; `fieldPairs` leaves the loop untyped in that case and never
    # reaches this emitter with a concrete object. Anything else is malformed.
    skip n
  else:
    skip n

proc emitObjectComponent(buf: var TokenBuf; iter: FieldsIter; n: var Cursor;
                         isInternalSym: bool; body: Cursor) =
  case n.substructureKind
  of FldU, GfldU:
    let field = takeLocal(n, SkipFinalParRi)
    buildNamedFieldIter(buf, iter, getIdent(field.name), field.name.symId,
                        field.name.symId, isInternalSym, body)
  of StmtsU:
    n.into:
      while n.hasMore:
        emitObjectComponent(buf, iter, n, isInternalSym, body)
  of NilU, WhenU:
    skip n
  of CaseU:
    emitObjectBody(buf, iter, n, isInternalSym, body)
  else:
    skip n

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
          emitObjectComponent(iterBuf, iter, currentField, isInternalSym, body)
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
