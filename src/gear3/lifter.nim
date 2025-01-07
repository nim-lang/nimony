#
#
#           Gear3 Compiler
#        (c) Copyright 2025 Andreas Rumpf
#
#    See the file "copying.txt", included in this
#    distribution, for details about the copyright.
#

##[

The lifter "lifts" a hook like `=destroy` or `=copy` from type `T`
to type `(T, T)`, etc.

]##

import std/[assertions, tables]

include nifprelude
import nifindexes, symparser, treemangler, typekeys
import ".." / nimony / [nimony_model, decls, programs, typenav]

type
  TypeCursor = Cursor

  GenHookRequest = object
    sym: SymId
    typ: TypeCursor
    op: AttachedOp

  LiftingCtx* = object
    dest: TokenBuf
    op: AttachedOp
    info: PackedLineInfo
    requests: seq[GenHookRequest]
    structuralTypeToHook: array[AttachedOp, Table[string, SymId]]


# Phase 1: Determine if the =hook is trivial:

when not defined(nimony):
  proc isTrivial*(c: var LiftingCtx; typ: TypeCursor): bool

proc hasHook(c: var LiftingCtx; s: SymId): bool =
  # XXX to implement
  false

proc isTrivialForFields(c: var LiftingCtx; n: Cursor): bool =
  var n = n
  while n.kind != ParRi:
    if n.substructureKind == FldS:
      let field = takeLocal(n)
      if field.kind == FldY:
        if not isTrivial(c, field.typ):
          return false
      else:
        skip n
    else:
      if not isTrivial(c, n):
        return false
      skip n
  return true

proc isTrivialObjectBody(c: var LiftingCtx; body: Cursor): bool =
  var n = body
  inc n # skip `(object` token
  if n.typeKind in {RefT, PtrT}:
    inc n

  var baseType = n
  skip n # skip basetype
  result = isTrivialForFields(c, n)
  if result:
    if baseType.kind == DotToken:
      result = true
    else:
      result = isTrivial(c, baseType)

proc isTrivialTypeDecl(c: var LiftingCtx; n: Cursor): bool =
  let r = asTypeDecl(n)
  assert(not r.isGeneric)
  case r.body.typeKind
  of PtrT:
    result = true
  of RefT:
    result = false
  of ObjectT:
    result = isTrivialObjectBody(c, r.body)
  else:
    result = true

proc isTrivial*(c: var LiftingCtx; typ: TypeCursor): bool =
  if typ.kind == Symbol:
    let res = tryLoadSym(typ.symId)
    if res.status == LacksNothing:
      if hasHook(c, typ.symId): return false
      return isTrivialTypeDecl(c, res.decl)
    else:
      quit "could not load: " & pool.syms[typ.symId]

  case typ.typeKind
  of IntT, UIntT, FloatT, BoolT, CharT, PtrT,
     MutT, OutT, SetT,
     EnumT, HoleyEnumT, VoidT, AutoT, SymKindT, ProcT,
     CstringT, PointerT, OrdinalT, OpenArrayT,
     UncheckedArrayT, VarargsT, RangeT, TypedescT:
    result = true
  of StringT, RefT:
    result = false
  of SinkT, ArrayT, LentT:
    result = isTrivial(c, typ.firstSon)
  of ObjectT:
    result = isTrivialObjectBody(c, typ)
  of TupleT:
    result = isTrivialForFields(c, typ)
  of NoType, NilT, OrT, AndT, NotT, ConceptT, DistinctT, StaticT, IterT, InvokeT,
     TypeKindT, UntypedT, TypedT:
    raiseAssert "bug here"

# Phase 2: Do the lifting

#[
Lifting =dup is done like this:

proc =dup(x: Obj): Obj =
  Obj(field: x.field, ..., complex: =dup(x.complex))

]#

proc genCallHook(c: var LiftingCtx; s: SymId; paramA, paramB: TokenBuf) =
  copyIntoKind c.dest, CallX, c.info:
    copyIntoSymUse c.dest, s, c.info
    case c.op
    of attachedWasMoved:
      if paramA[0].kind == Symbol:
        # &*param cancel out to `param`:
        copyTree c.dest, paramA
      else:
        copyIntoKind c.dest, HaddrX, c.info:
          copyTree c.dest, paramA
    of attachedDestroy, attachedDup:
      copyTree c.dest, paramA
    of attachedCopy, attachedTrace:
      copyTree c.dest, paramA
      copyTree c.dest, paramB

proc genTrivialOp(c: var LiftingCtx; paramA, paramB: TokenBuf) =
  case c.op
  of attachedDestroy, attachedWasMoved: discard
  of attachedDup:
    copyTree c.dest, paramA
  of attachedCopy:
    copyIntoKind c.dest, AsgnS, c.info:
      copyTree c.dest, paramA
      copyTree c.dest, paramB
  of attachedTrace: discard

proc requestLifting(c: var LiftingCtx; op: AttachedOp; t: TypeCursor): SymId =
  let key = mangle(t)
  result = c.structuralTypeToHook[op].getOrDefault(key)
  if result == SymId(0):
    result = declareSym(c.p[c.thisModule], ProcDecl, attachedOpToLitId(op))
    c.requests.add GenHookRequest(sym: result, typ: t, op: op)
    c.structuralTypeToHook[op][key] = result

proc getStringHook(c: var LiftingCtx): SymId =
  case c.op
  of attachedDestroy:
    result = getCodegenProc(c.p, "nimStrDestroy")
  of attachedWasMoved:
    result = getCodegenProc(c.p, "nimStrWasMoved")
  of attachedDup:
    result = getCodegenProc(c.p, "nimStrDup")
  of attachedCopy:
    result = getCodegenProc(c.p, "nimStrCopy")
  of attachedTrace:
    result = SymId(m: ModuleId(0), s: SymId(-1))

proc maybeCallHook(c: var LiftingCtx; s: SymId; paramA, paramB: TokenBuf) =
  if s.s != SymId(0):
    genCallHook c, dest, s, paramA, paramB

proc lift(c: var LiftingCtx; typ, orig: TypeCursor): SymId =
  # Goal: We produce a call to some function. Maybe this function must be
  # synthesized, if so this will be done by calling `requestLifting`.
  if isTrivial(c, typ):
    return NoSymId
  case c.p[typ].kind
  of StringTy:
    result = getStringHook(c)
  of PtrTy, ArrayPtrTy:
    assert false, "ptr T should have been a 'trivial' type"
  of ObjectTy, DistinctTy:
    let symId = fromModuleSymUse(c.p, c.p[typ.m], typ.t)
    result = requestLifting(c, c.op, symId, orig)
  of AliasTy:
    let t = typeImpl(c.p, typ)
    result = lift(c, t, t)
  of BracketTy:
    let ct = canonType(c, c.p[typ.m], typ.t)
    let fn = getOrDefault(c.p[c.thisModule].instantiatedOps[c.op], ct)
    if fn != SymId(0):
      result = SymId(m: c.thisModule, s: fn)
    else:
      let impl = skipGenericAlias(c.p, typ)
      if c.p[impl].kind == BracketTy:
        result = lift(c, instantiateType(c.p, impl, c.types), typ)
      else:
        result = lift(c, impl, typ)
  of TupleTy, ArrayTy, RefTy:
    result = requestLiftingForStructuralType(c, c.op, orig)
  else:
    result = InvalidSymId

when not defined(nimony):
  proc unravel(c: var LiftingCtx; typ, orig: TypeCursor; paramA, paramB: TokenBuf)

proc needsDeref(c: var LiftingCtx; obj: TokenBuf; isSecondParam: bool): bool {.inline.} =
  result = (c.op in {attachedTrace, attachedWasMoved} or (c.op == attachedCopy and not isSecondParam)) and
    obj[0].kind == Symbol # still access to the parameter directly

proc accessObjField(c: var LiftingCtx; obj: TokenBuf; name, typ: Cursor; isSecondParam = false): TokenBuf =
  result = createTempTree(c.thisModule)
  let nd = needsDeref(c, obj, isSecondParam)
  copyIntoKind(result, if nd: VarFieldAccess else: FieldAccess, c.info):
    copyTree result, obj, 0
    copyIntoSymUse c.p, result, SymId(m: tree.m, s: name.symId), c.info
    copyTreeX result, typ, c.p

proc accessTupField(c: var LiftingCtx; tup: TokenBuf; idx: int; isSecondParam = false): TokenBuf =
  result = createTempTree(c.thisModule)
  let nd = needsDeref(c, tup, isSecondParam)
  copyIntoKind(result, TupleFieldAccess, c.info):
    if nd:
      copyIntoKind(result, HiddenDeref, c.info):
        copyTree result, tup, 0
    else:
      copyTree result, tup, 0
    result.add Position, c.info, uint32(idx)

proc unravelObj(c: var LiftingCtx;
                n: Cursor; paramA, paramB: TokenBuf) =
  var n = n
  if n.kind in {RefTy, PtrTy}:
    n = n.firstSon
  assert n.kind == ObjectBody
  let fieldList = ithSon(n, objectBodyPos)
  for ch in sonsReadonly(fieldList):
    assert ch.kind == FieldDecl
    let r = asLocal(ch)
    # create `paramA.field` because we need to do `paramA.field = paramB.field` etc.
    let fieldType = TypeCursor(m: tree.id, t: r.typ)
    case c.op
    of attachedDestroy, attachedTrace, attachedWasMoved:
      let a = accessObjField(c, paramA, r.name, r.typ)
      unravel c, dest, fieldType, fieldType, a, paramB
    of attachedDup:
      assert false, "cannot happen"
    of attachedCopy:
      let a = accessObjField(c, paramA, r.name, r.typ)
      let b = accessObjField(c, paramB, r.name, r.typ, true)
      unravel c, dest, fieldType, fieldType, a, b

proc unravelObjForDup(c: var LiftingCtx;
                      n: Cursor; paramA, paramB: TokenBuf;
                      orig: TypeCursor) =
  assert n.kind == ObjectBody
  let fieldList = ithSon(n, objectBodyPos)
  copyIntoKind dest, ObjConstr, c.info:
    copyTree dest, c.p[orig.m], orig.t
    for ch in sonsReadonly(fieldList):
      assert ch.kind == FieldDecl
      let r = asLocal(ch)
      copyIntoKind dest, ExprColonExpr, c.info:
        addSymUse dest, r.name.symId, r.name.info
        let a = accessObjField(c, paramA, r.name, r.typ)
        let fieldType = TypeCursor(m: tree.id, t: r.typ)
        unravel c, dest, fieldType, fieldType, a, paramB

proc unravelTuple(c: var LiftingCtx;
                  n: Cursor; paramA, paramB: TokenBuf) =
  var idx = 0
  for ch in sonsReadonly(n):
    let fieldType = TypeCursor(m: tree.id, t: ch)
    case c.op
    of attachedDestroy, attachedTrace, attachedWasMoved:
      let a = accessTupField(c, paramA, idx)
      unravel c, dest, fieldType, fieldType, a, paramB
    of attachedDup:
      assert false, "cannot happen"
    of attachedCopy:
      let a = accessTupField(c, paramA, idx)
      let b = accessTupField(c, paramB, idx, true)
      unravel c, dest, fieldType, fieldType, a, b
    inc idx

proc unravelTupleForDup(c: var LiftingCtx;
                        n: Cursor; paramA, paramB: TokenBuf;
                        orig: TypeCursor) =
  assert n.kind == TupleTy
  copyIntoKind dest, TupleConstr, c.info:
    copyTree dest, c.p[orig.m], orig.t
    var idx = 0
    for ch in sonsReadonly(n):
      let a = accessTupField(c, paramA, idx)
      let fieldType = TypeCursor(m: tree.id, t: ch)
      unravel c, dest, fieldType, fieldType, a, paramB
      inc idx

template copyIntoEmit(c, result, body: untyped) =
  copyIntoKind result, EmitStmt, c.info:
    body

template verbatim(result: TokenBuf; s: string) =
  addVerbatim result, getOrIncl(c.p[result.m].strings, s), c.info

proc accessArrayAt(c: var LiftingCtx; arr: TokenBuf; indexVar: SymId): TokenBuf =
  result = createTempTree(c.thisModule)
  copyIntoKind result, ArrayAt, c.info:
    copyTree result, arr, 0
    addSymUse result, indexVar, c.info

proc indexVarLowerThanArrayLen(c: var LiftingCtx; arr: TokenBuf; indexVar: SymId; arrayLen: int64) =
  copyIntoEmit c, dest:
    addSymUse dest, indexVar, c.info
    verbatim dest, " < "
    copyIntoKind dest, ConvExpr, c.info:
      add dest, IntTy, c.info, uint32(c.p.config.pointerWidth)
      dest.add IntLit, c.info, c.p[dest.m].numbers.getOrIncl(arrayLen)

proc incIndexVar(c: var LiftingCtx; indexVar: SymId) =
  copyIntoEmit c, dest:
    verbatim dest, "++"
    addSymUse dest, indexVar, c.info
    verbatim dest, ";"

proc declareIndexVar(c: var LiftingCtx; indexVar: SymId) =
  copyIntoKind dest, VarDecl, c.info:
    addSymDef dest, indexVar, c.info
    dest.addEmpty2 c.info # not exported, no pragmas
    add dest, IntTy, c.info, uint32(c.p.config.pointerWidth)
    copyIntoKind dest, ConvExpr, c.info:
      add dest, IntTy, c.info, uint32(c.p.config.pointerWidth)
      add dest, IntLit, c.info, c.p[dest.m].numbers.getOrIncl(0)

proc unravelArray(c: var LiftingCtx;
                  n: Cursor; paramA, paramB: TokenBuf) =
  assert n.kind == ArrayTy
  let (_, elem) = sons2(n)
  let arrayLen = getArrayLen(n, c.p)

  let indexVar = declareSym(c.p[c.thisModule], VarDecl, getOrIncl(c.p[dest.m].strings, "idx"))
  declareIndexVar c, dest, indexVar

  let baseType = TypeCursor(m: tree.id, t: elem)
  copyIntoKind dest, WhileStmt, c.info:
    indexVarLowerThanArrayLen c, dest, paramA, indexVar, arrayLen
    copyIntoKind dest, StmtList, c.info:
      case c.op
      of attachedDestroy, attachedTrace, attachedWasMoved:
        let a = accessArrayAt(c, paramA, indexVar)
        #unravel c, dest, fieldType, fieldType, a, paramB
        let fn = lift(c, baseType, baseType)
        maybeCallHook c, dest, fn, a, paramA
      of attachedDup:
        assert false, "cannot happen"
      of attachedCopy:
        let a = accessArrayAt(c, paramA, indexVar)
        let b = accessArrayAt(c, paramB, indexVar)
        let fn = lift(c, baseType, baseType)
        maybeCallHook c, dest, fn, a, b

      incIndexVar c, dest, indexVar

proc unravelArrayForDup(c: var LiftingCtx;
                        n: Cursor; paramA, paramB: TokenBuf;
                        orig: TypeCursor) =
  assert n.kind == ArrayTy
  # We generate: (;
  # var result: array;
  # for i: result[i] = dup(src[i]); result)
  copyIntoKind dest, StmtListExpr, c.info:
    copyTreeX(dest, c.p[orig.m], orig.t, c.p)
    copyIntoKind dest, StmtList, c.info:

      let (_, elem) = sons2(n)
      let arrayLen = getArrayLen(n, c.p)

      let indexVar = declareSym(c.p[c.thisModule], VarDecl, getOrIncl(c.p[dest.m].strings, "idx"))
      declareIndexVar c, dest, indexVar

      let res = declareSym(c.p[c.thisModule], VarDecl, getOrIncl(c.p[dest.m].strings, "result"))
      copyIntoKind dest, VarDecl, c.info:
        addSymDef dest, res, c.info
        dest.addEmpty2 c.info # not exported, no pragmas
        copyTreeX(dest, c.p[orig.m], orig.t, c.p)
        add dest, Empty, c.info # no initial value

      var resTree = createTempTree(c.thisModule)
      addSymUse resTree, res, c.info

      let fieldType = TypeCursor(m: tree.id, t: elem)
      copyIntoKind dest, WhileStmt, c.info:
        indexVarLowerThanArrayLen c, dest, paramA, indexVar, arrayLen
        copyIntoKind dest, StmtList, c.info:
          let a = accessArrayAt(c, paramA, indexVar)
          let r = accessArrayAt(c, resTree, indexVar)
          copyIntoKind dest, FirstAsgn, c.info:
            copyTree dest, r, 0
            unravel c, dest, fieldType, fieldType, a, paramB

          incIndexVar c, dest, indexVar

    addSymUse dest, res, c.info

proc derefInner(c: var LiftingCtx; x: TokenBuf): TokenBuf =
  result = createTempTree(x.m)
  copyIntoEmit c, result:
    copyTree result, x, 0
    verbatim result, "->inner"

proc emitRefDestructor(c: var LiftingCtx; paramA: TokenBuf; baseType: TypeCursor) =
  copyIntoKinds dest, [IfStmt, ElifBranch], c.info:
    copyTree dest, paramA, 0
    copyIntoKinds dest, [StmtList, IfStmt], c.info:
      # here we know that `x` is not nil:
      copyIntoKind dest, ElifBranch, c.info:
        copyIntoEmit c, dest:
          copyTree dest, paramA, 0
          verbatim dest, "->rc == 0"
        copyIntoKind dest, StmtList, c.info:
          let oldOp = c.op
          c.op = attachedDestroy
          unravel c, dest, baseType, baseType, c.derefInner(paramA), paramA
          when false:
            if c.p[baseType].kind == ObjectBody:
              unravelObj c, dest, c.p[baseType.m], baseType.t, c.derefInner(paramA), paramA
            else:
              let fn = lift(c, baseType, baseType)
              maybeCallHook c, dest, fn, c.derefInner(paramA), paramA
          c.op = oldOp
          copyIntoEmit c, dest:
            verbatim dest, "QdeallocFixed("
            copyTreeX dest, c.p[baseType.m], baseType.t, c.p
            verbatim dest, ", "
            copyTree dest, paramA, 0
            verbatim dest, ");"
      copyIntoKind dest, ElseBranch, c.info:
        copyIntoEmit c, dest:
          verbatim dest, "--"
          copyTree dest, paramA, 0
          verbatim dest, "->rc;"

when false:
  proc deref(x: TokenBuf): TokenBuf =
    result = createTempTree(x.m)
    copyIntoKind result, DerefExpr, x[0].info:
      copyTree result, x, 0

proc emitIncRef(c: var LiftingCtx; x: TokenBuf) =
  copyIntoKinds dest, [IfStmt, ElifBranch], c.info:
    copyTree dest, x, 0
    copyIntoKind dest, StmtList, c.info:
      copyIntoEmit c, dest:
        verbatim dest, "++"
        copyTree dest, x, 0
        verbatim dest, "->rc;"

proc unravelRef(c: var LiftingCtx; refType: TypeCursor;
                n: Cursor; paramA, paramB: TokenBuf) =
  assert n.kind == RefTy
  let baseType = TypeCursor(m: tree.id, t: n.firstSon)
  case c.op
  of attachedDestroy:
    emitRefDestructor c, dest, paramA, baseType
  of attachedTrace:
    discard "to implement"
  of attachedWasMoved:
    copyIntoEmit c, dest:
      copyTree dest, paramA, 0
      verbatim dest, " = nullptr;"
  of attachedDup:
    emitIncRef c, dest, paramA
    copyIntoKind dest, RetS, c.info:
      copyTree dest, paramA, 0
  of attachedCopy:
    # if src != nil: inc src.rc
    # destroy dest[]
    # dest[] = src
    emitIncRef c, dest, paramB
    let oldOp = c.op
    c.op = attachedDestroy
    let fn = lift(c, refType, refType)
    #let ad = deref(paramA)
    maybeCallHook c, dest, fn, paramA, paramA
    c.op = oldOp
    copyIntoKind dest, Asgn, c.info:
      copyTree dest, paramA, 0
      copyTree dest, paramB, 0

proc unravel(c: var LiftingCtx; typ, orig: TypeCursor; paramA, paramB: TokenBuf) =
  # `unravel`'s job is to "expand" the object fields in contrast to `lift`.
  if isTrivial(c, typ):
    genTrivialOp c, dest, paramA, paramB
    return
  case c.p[typ].kind
  of ObjectTy:
    let declPos = typeImpl(c.p, typ)
    if c.op == attachedDup:
      unravelObjForDup c, dest, c.p[declPos.m], declPos.t, paramA, paramB, orig
    else:
      unravelObj c, dest, c.p[declPos.m], declPos.t, paramA, paramB

  of ObjectBody:
    let declPos = typ
    if c.op == attachedDup:
      unravelObjForDup c, dest, c.p[declPos.m], declPos.t, paramA, paramB, orig
    else:
      unravelObj c, dest, c.p[declPos.m], declPos.t, paramA, paramB

  of AliasTy:
    unravel(c, dest, typeImpl(c.p, typ), orig, paramA, paramB)
  of DistinctTy:
    unravel(c, dest, typeImpl(c.p, typ).firstSon, orig, paramA, paramB)
  of BracketTy:
    let ct = canonType(c, c.p[typ.m], typ.t)
    let fn = getOrDefault(c.p[c.thisModule].instantiatedOps[c.op], ct)
    if fn != SymId(0):
      genCallHook c, dest, SymId(m: dest.m, s: fn), paramA, paramB
    else:
      let impl = skipGenericAlias(c.p, typ)
      if c.p[impl].kind == BracketTy:
        let inst = instantiateType(c.p, impl, c.types)
        #echo "unravelling ", typeToString(c.p, inst)
        unravel(c, dest, inst, orig, paramA, paramB)
      else:
        unravel c, dest, impl, orig, paramA, paramB
  of TupleTy:
    if c.op == attachedDup:
      unravelTupleForDup c, dest, c.p[typ.m], typ.t, paramA, paramB, orig
    else:
      unravelTuple c, dest, c.p[typ.m], typ.t, paramA, paramB
  of ArrayTy:
    if c.op == attachedDup:
      unravelArrayForDup c, dest, c.p[typ.m], typ.t, paramA, paramB, orig
    else:
      unravelArray c, dest, c.p[typ.m], typ.t, paramA, paramB
  else:
    #lift c, dest, typ, paramA, paramB
    let fn = lift(c, typ, orig)
    maybeCallHook c, dest, fn, paramA, paramB

proc unravelFirstLevel(c: var LiftingCtx; typ, orig: TypeCursor; paramA, paramB: TokenBuf) =
  if c.p[typ].kind == BracketTy:
    let impl = skipGenericAlias(c.p, typ)
    if c.p[impl].kind == BracketTy:
      let inst = instantiateType(c.p, impl, c.types)
      unravel(c, dest, inst, orig, paramA, paramB)
    else:
      unravel c, dest, impl, orig, paramA, paramB
  else:
    unravel c, dest, typ, orig, paramA, paramB

proc addParamWithModifier(c: var LiftingCtx; param: SymId; typ: TypeCursor; modifier: NodeKind) =
  copyIntoKind(dest, ParamDecl, c.info):
    addSymDef dest, param, c.info
    dest.addEmpty2 c.info # export marker, pragmas
    copyIntoKind(dest, modifier, c.info):
      copyTreeX dest, c.p[typ.m], typ.t, c.p
    dest.addEmpty c.info # value

proc addParam(c: var LiftingCtx; param: SymId; typ: TypeCursor) =
  copyIntoKind(dest, ParamDecl, c.info):
    addSymDef dest, param, c.info
    dest.addEmpty2 c.info # export marker, pragmas
    copyTreeX dest, c.p[typ.m], typ.t, c.p
    dest.addEmpty c.info # value

proc genProcDecl(c: var LiftingCtx; sym: SymId; typ: TypeCursor) =
  #let name = attachedOpToLitId(c.op)
  #echo "generating ", c.p[dest.m].strings[name] # name
  #result = declareSym(c.p[c.thisModule], ProcDecl, name)
  let paramA = declareSym(c.p[c.thisModule], ParamDecl, "dest")
  var paramTreeA = createTempTree(c.thisModule)
  addSymUse paramTreeA, paramA, c.info

  var paramB = SymId(-1)
  var paramTreeB = createTempTree(c.thisModule)

  case c.op
  of attachedDestroy, attachedWasMoved, attachedDup: discard
  of attachedCopy, attachedTrace:
    paramB = declareSym(c.p[c.thisModule], ParamDecl, "src")
    addSymUse paramTreeB, paramB, c.info

  copyIntoKind(dest, ProcDecl, c.info):
    addSymDef dest, sym, c.info
    dest.addEmpty3 c.info # export marker, pattern, generics

    copyIntoKind(dest, Params, c.info):
      case c.op
      of attachedDestroy:
        dest.add VoidTy, c.info # return type
        #addParamWithModifier c, dest, paramA, typ, SinkTy
        addParam c, dest, paramA, typ
      of attachedWasMoved:
        dest.add VoidTy, c.info # return type
        addParamWithModifier c, dest, paramA, typ, VarTy
      of attachedDup:
        copyTree dest, c.p[typ.m], typ.t
        addParam c, dest, paramA, typ
      of attachedCopy:
        dest.add VoidTy, c.info # return type
        addParamWithModifier c, dest, paramA, typ, VarTy
        addParam c, dest, paramB, typ
      of attachedTrace:
        dest.add VoidTy, c.info # return type
        addParamWithModifier c, dest, paramA, typ, VarTy
        addParam c, dest, paramB, charType # XXX Change this to PointerTy once we have it

    copyIntoKind dest, Pragmas, c.info:
      addSystemPragma(dest, "nodestroy", c.info, c.p)

    dest.addEmpty c.info # exc

    var a = skipGenericInsts(c.p, typ)
    let k = c.p[a].kind

    if k == RefTy:
      copyIntoKind(dest, StmtList, c.info):
        unravelRef(c, dest, typ, c.p[a.m], a.t, paramTreeA, paramTreeB)
    elif c.op == attachedDup:
      copyIntoKind(dest, StmtList, c.info):
        let d = takePos dest
        copyIntoKind(dest, RetS, c.info):
          unravelFirstLevel(c, dest, typ, typ, paramTreeA, paramTreeB)
        assert hasAtLeastXsons(dest, d, 1), typeToString(c.p, typ)
    else:
      copyIntoKind(dest, StmtList, c.info):
        unravelFirstLevel(c, dest, typ, typ, paramTreeA, paramTreeB)

proc genMissingHooks*(c: var LiftingCtx) =
  # remember that genProcDecl does mutate c.requests so be robust against that:
  while c.requests.len:
    let reqs = move(c.requests)
    for i in 0 ..< reqs.len:
      c.op = reqs[i].op
      genProcDecl(c, reqs[i].sym, reqs[i].typ)

proc createLiftingCtx*(p: Program; thisModule: ModuleId; types: TreeId): ref LiftingCtx =
  (ref LiftingCtx)(p: p, thisModule: thisModule, types: types, op: attachedDestroy,
    info: NoLineInfo)

proc requestHook*(c: var LiftingCtx; sym: SymId; typ: TypeCursor; op: AttachedOp) =
  c.op = op
  genProcDecl c, dest, sym, typ
  genMissingHooks(c, dest)

proc getHook*(c: var LiftingCtx; op: AttachedOp; typ: TypeCursor; info: PackedLineInfo): SymId =
  c.op = op
  c.info = info
  let t = if typ.typeKind == SinkT: typ.firstSon else: typ
  result = lift(c, t, t)

proc getDestructor*(c: var LiftingCtx; typ: TypeCursor; info: PackedLineInfo): SymId =
  getHook(c, attachedDestroy, typ, info)
