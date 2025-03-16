#
#
#           Hexer Compiler
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
import nifindexes, symparser, treemangler, typekeys, hexer_context
import ".." / nimony / [nimony_model, decls, programs, typenav, expreval, xints, builtintypes]

type
  TypeCursor = Cursor

  GenHookRequest = object
    sym: SymId
    typ: TypeCursor
    op: AttachedOp

  LiftingCtx* = object
    dest*: TokenBuf
    op: AttachedOp
    calledErrorHook: PackedLineInfo
    info: PackedLineInfo
    requests: seq[GenHookRequest]
    structuralTypeToHook: array[AttachedOp, Table[string, SymId]]
    nominalTypeToHook: array[AttachedOp, Table[SymId, SymId]]
    hookNames: Table[string, int]
    thisModuleSuffix: string

# Phase 1: Determine if the =hook is trivial:

when not defined(nimony):
  proc isTrivial*(c: var LiftingCtx; typ: TypeCursor): bool

proc loadHook(c: var LiftingCtx; op: AttachedOp; s: SymId): SymId =
  result = c.nominalTypeToHook[op].getOrDefault(s)
  if result == SymId(0):
    result = tryLoadHook(op, s, false)
    if result != SymId(0):
      c.nominalTypeToHook[op][s] = result

proc hasHook(c: var LiftingCtx; s: SymId): bool =
  result = loadHook(c, c.op, s) != SymId(0)

proc getCompilerProc(c: var LiftingCtx; name: string): SymId =
  result = pool.syms.getOrIncl(name & ".0." & SystemModuleSuffix)

proc isTrivialForFields(c: var LiftingCtx; n: Cursor): bool =
  var n = n
  while n.kind != ParRi:
    if n.substructureKind == FldU:
      let field = takeLocal(n, SkipFinalParRi)
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
  if n.typeKind in {RefT, PtrT}:
    inc n
  inc n # skip `(object` token

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
     EnumT, HoleyEnumT, VoidT, AutoT, SymKindT, ProctypeT,
     CstringT, PointerT, OrdinalT,
     UarrayT, VarargsT, RangetypeT, TypedescT,
     ParamsT:
    result = true
  of RefT:
    result = false
  of SinkT, ArrayT, LentT:
    result = isTrivial(c, typ.firstSon)
  of ObjectT:
    result = isTrivialObjectBody(c, typ)
  of TupleT:
    var tup = typ
    inc tup
    while tup.kind != ParRi:
      let field = getTupleFieldType(tup)
      if not isTrivial(c, field):
        return false
      skip tup
    result = true
  of NoType, ErrT, NiltT, OrT, AndT, NotT, ConceptT, DistinctT, StaticT, InvokeT,
     TypeKindT, UntypedT, TypedT, IteratorT, ItertypeT:
    raiseAssert "bug here"

# Phase 2: Do the lifting

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
    of attachedDestroy:
      copyTree c.dest, paramA
    of attachedDup:
      copyTree c.dest, paramB
    of attachedCopy, attachedTrace, attachedSink:
      copyTree c.dest, paramA
      copyTree c.dest, paramB

proc genTrivialOp(c: var LiftingCtx; paramA, paramB: TokenBuf) =
  case c.op
  of attachedDestroy, attachedWasMoved: discard
  of attachedCopy, attachedSink, attachedDup:
    copyIntoKind c.dest, AsgnS, c.info:
      copyTree c.dest, paramA
      copyTree c.dest, paramB
  of attachedTrace: discard

proc generateHookName(c: var LiftingCtx; op: AttachedOp; key: string): string =
  result = "=" & hookName(op) & "_" & key
  var counter = addr c.hookNames.mgetOrPut(result, -1)
  counter[] += 1
  result.add '.'
  result.addInt counter[]
  result.add '.'
  result.add c.thisModuleSuffix

proc requestLifting(c: var LiftingCtx; op: AttachedOp; t: TypeCursor): SymId =
  if t.kind in {Symbol, SymbolDef}:
    result = loadHook(c, op, t.symId)
    if result != SymId(0):
      return result

  let key = mangle(t)
  result = c.structuralTypeToHook[op].getOrDefault(key)
  if result == SymId(0):
    let name = generateHookName(c, op, key)
    result = pool.syms.getOrIncl(name)
    c.requests.add GenHookRequest(sym: result, typ: t, op: op)
    c.structuralTypeToHook[op][key] = result

proc maybeCallHook(c: var LiftingCtx; s: SymId; paramA, paramB: TokenBuf) =
  if s != NoSymId:
    let res = tryLoadSym(s)
    if res.status == LacksNothing:
      let r = asRoutine(res.decl)
      if hasPragma(r.pragmas, ErrorP):
        c.calledErrorHook = r.name.info
    if c.op == attachedDup:
      copyIntoKind c.dest, AsgnS, c.info:
        copyTree c.dest, paramA
        genCallHook c, s, paramA, paramB
    else:
      genCallHook c, s, paramA, paramB

proc lift(c: var LiftingCtx; typ: TypeCursor): SymId =
  # Goal: We produce a call to some function. Maybe this function must be
  # synthesized, if so this will be done by calling `requestLifting`.
  if isTrivial(c, typ):
    return NoSymId

  let orig = typ
  let typ = toTypeImpl typ
  case typ.typeKind
  of PtrT:
    raiseAssert "ptr T should have been a 'trivial' type"
  of ObjectT, DistinctT, TupleT, ArrayT, RefT:
    result = requestLifting(c, c.op, orig)
  else:
    result = NoSymId

when not defined(nimony):
  proc unravel(c: var LiftingCtx; typ: TypeCursor; paramA, paramB: TokenBuf)

proc needsDeref(c: var LiftingCtx; obj: TokenBuf; paramPos: int): bool {.inline.} =
  result = (c.op in {attachedTrace, attachedWasMoved} or (c.op in {attachedCopy, attachedSink} and paramPos == 0)) and
    obj[0].kind == Symbol # still access to the parameter directly

proc accessObjField(c: var LiftingCtx; obj: TokenBuf; name: Cursor; paramPos = 0): TokenBuf =
  assert name.kind == SymbolDef
  let nameSym = name.symId
  result = createTokenBuf(4)
  let nd = needsDeref(c, obj, paramPos)
  copyIntoKind(result, DotX, c.info):
    if nd:
      result.addParLe HderefX, c.info
    copyTree result, obj
    if nd:
      result.addParRi()
    copyIntoSymUse result, nameSym, c.info
    result.addIntLit(0, c.info)

proc accessTupField(c: var LiftingCtx; tup: TokenBuf; idx: int; paramPos = 0): TokenBuf =
  result = createTokenBuf(4)
  let nd = needsDeref(c, tup, paramPos)
  copyIntoKind(result, TupatX, c.info):
    if nd:
      copyIntoKind(result, HderefX, c.info):
        copyTree result, tup
    else:
      copyTree result, tup
    result.add intToken(pool.integers.getOrIncl(idx), c.info)

proc unravelObj(c: var LiftingCtx; n: Cursor; paramA, paramB: TokenBuf) =
  var n = n
  if n.typeKind in {RefT, PtrT}:
    inc n
  assert n.typeKind == ObjectT
  inc n
  # recurse for the inherited object type, if any:
  if n.kind != DotToken:
    unravelObj c, n, paramA, paramB
  skip n # inheritance is gone
  while n.kind != ParRi:
    let r = takeLocal(n, SkipFinalParRi)
    assert r.kind == FldY
    # create `paramA.field` because we need to do `paramA.field = paramB.field` etc.
    let fieldType = r.typ
    case c.op
    of attachedDestroy, attachedTrace, attachedWasMoved:
      let a = accessObjField(c, paramA, r.name)
      unravel c, fieldType, a, paramB
    of attachedCopy, attachedSink, attachedDup:
      let a = accessObjField(c, paramA, r.name, 0)
      let b = accessObjField(c, paramB, r.name, 1)
      unravel c, fieldType, a, b

proc unravelTuple(c: var LiftingCtx;
                  n: Cursor; paramA, paramB: TokenBuf) =
  assert n.typeKind == TupleT
  var n = n
  inc n
  var idx = 0
  while n.kind != ParRi:
    let fieldType = getTupleFieldType(n)
    skip n

    case c.op
    of attachedDestroy, attachedTrace, attachedWasMoved:
      let a = accessTupField(c, paramA, idx)
      unravel c, fieldType, a, paramB
    of attachedCopy, attachedSink, attachedDup:
      let a = accessTupField(c, paramA, idx, 0)
      let b = accessTupField(c, paramB, idx, 1)
      unravel c, fieldType, a, b
    inc idx

proc accessArrayAt(c: var LiftingCtx; arr: TokenBuf; indexVar: SymId; paramPos = 0): TokenBuf =
  result = createTokenBuf(4)
  let nd = needsDeref(c, arr, paramPos)
  copyIntoKind result, ArrAtX, c.info:
    if nd:
      result.addParLe HderefX, c.info
    copyTree result, arr
    if nd:
      result.addParRi()
    copyIntoSymUse result, indexVar, c.info

proc indexVarLowerThanArrayLen(c: var LiftingCtx; indexVar: SymId; arrayLen: xint) =
  copyIntoKind c.dest, LtX, c.info:
    copyIntoKind c.dest, IntT, c.info:
      c.dest.add intToken(pool.integers.getOrIncl(-1), c.info)
    copyIntoSymUse c.dest, indexVar, c.info
    var err = false
    let alen = asSigned(arrayLen, err)
    if not err:
      c.dest.add intToken(pool.integers.getOrIncl(alen), c.info)
    else:
      err = false
      let ualen = asUnsigned(arrayLen, err)
      assert(not err)
      c.dest.add uintToken(pool.uintegers.getOrIncl(ualen), c.info)

proc addIntType(c: var LiftingCtx) =
  copyIntoKind c.dest, IntT, c.info:
    c.dest.add intToken(pool.integers.getOrIncl(-1), c.info)

proc incIndexVar(c: var LiftingCtx; indexVar: SymId) =
  copyIntoKind c.dest, AsgnS, c.info:
    copyIntoSymUse c.dest, indexVar, c.info
    copyIntoKind c.dest, AddX, c.info:
      addIntType c
      copyIntoSymUse c.dest, indexVar, c.info
      c.dest.add intToken(pool.integers.getOrIncl(+1), c.info)

proc declareIndexVar(c: var LiftingCtx; indexVar: SymId) =
  copyIntoKind c.dest, VarY, c.info:
    addSymDef c.dest, indexVar, c.info
    c.dest.addEmpty2 c.info # not exported, no pragmas
    addIntType c
    c.dest.add intToken(pool.integers.getOrIncl(0), c.info)

proc unravelArray(c: var LiftingCtx;
                  n: Cursor; paramA, paramB: TokenBuf) =
  assert n.typeKind == ArrayT
  let arrayLen = getArrayLen(n)
  var n = n
  inc n
  let baseType = n

  let indexVar = pool.syms.getOrIncl("idx.0")
  declareIndexVar c, indexVar

  copyIntoKind c.dest, WhileS, c.info:
    indexVarLowerThanArrayLen c, indexVar, arrayLen
    copyIntoKind c.dest, StmtsS, c.info:
      case c.op
      of attachedDestroy, attachedTrace, attachedWasMoved:
        let a = accessArrayAt(c, paramA, indexVar)
        #unravel c, fieldType, fieldType, a, paramB
        let fn = lift(c, baseType)
        maybeCallHook c, fn, a, paramA
      of attachedCopy, attachedDup, attachedSink:
        let a = accessArrayAt(c, paramA, indexVar, 0)
        let b = accessArrayAt(c, paramB, indexVar, 1)
        let fn = lift(c, baseType)
        maybeCallHook c, fn, a, b

      incIndexVar c, indexVar

proc derefInner(c: var LiftingCtx; x: TokenBuf): TokenBuf =
  result = createTokenBuf(10)
  copyIntoKind result, DotX, c.info:
    copyIntoKind result, DerefX, c.info:
      copyTree result, x
    copyIntoSymUse result, pool.syms.getOrIncl(DataField), c.info
    result.addIntLit(0, c.info)

proc refcountOf(c: var LiftingCtx; x: TokenBuf) =
  copyIntoKind c.dest, AddrX, c.info:
    copyIntoKind c.dest, DotX, c.info:
      copyIntoKind c.dest, DerefX, c.info:
        copyTree c.dest, x
      copyIntoSymUse c.dest, pool.syms.getOrIncl(RcField), c.info
      c.dest.addIntLit(0, c.info)

proc emitRefDestructor(c: var LiftingCtx; paramA: TokenBuf; baseType: TypeCursor) =
  c.dest.addParLe IfS, c.info
  c.dest.addParLe ElifU, c.info

  copyTree c.dest, paramA
  copyIntoKinds c.dest, [StmtsS, IfS], c.info:
    # here we know that `x` is not nil:
    copyIntoKind c.dest, ElifU, c.info:
      copyIntoKind c.dest, CallS, c.info:
        copyIntoSymUse c.dest, getCompilerProc(c, "arcDec"), c.info
        refcountOf(c, paramA)

      copyIntoKind c.dest, StmtsS, c.info:
        let oldOp = c.op
        c.op = attachedDestroy
        unravel c, baseType, c.derefInner(paramA), paramA
        c.op = oldOp
        copyIntoKind c.dest, CallS, c.info:
          copyIntoSymUse c.dest, getCompilerProc(c, "deallocFixed"), c.info
          copyTree c.dest, paramA

  c.dest.addParRi()
  c.dest.addParRi()

proc emitIncRef(c: var LiftingCtx; x: TokenBuf) =
  c.dest.addParLe IfS, c.info
  c.dest.copyIntoKind ElifU, c.info:
    copyTree c.dest, x
    copyIntoKind c.dest, StmtsS, c.info:
      copyIntoKind c.dest, CallS, c.info:
        copyIntoSymUse c.dest, getCompilerProc(c, "arcInc"), c.info
        refcountOf(c, x)
  c.dest.addParRi()

proc unravelRef(c: var LiftingCtx; n: Cursor; paramA, paramB: TokenBuf) =
  assert n.typeKind == RefT
  let baseType = n.firstSon
  case c.op
  of attachedDestroy:
    emitRefDestructor c, paramA, baseType
  of attachedTrace:
    discard "to implement"
  of attachedWasMoved:
    copyIntoKind c.dest, AsgnS, c.info:
      copyIntoKind c.dest, DerefX, c.info:
        copyTree c.dest, paramA
      copyIntoKind c.dest, NilX, c.info: discard
  of attachedDup:
    emitIncRef c, paramB
    copyIntoKind c.dest, AsgnS, c.info:
      copyTree c.dest, paramA
      copyTree c.dest, paramB
  of attachedCopy, attachedSink:
    # if src != nil: inc src.rc
    # destroy dest[]
    # dest[] = src
    emitIncRef c, paramB
    let oldOp = c.op
    c.op = attachedDestroy
    let fn = lift(c, n)
    maybeCallHook c, fn, paramA, paramA
    c.op = oldOp
    copyIntoKind c.dest, AsgnS, c.info:
      copyTree c.dest, paramA
      copyTree c.dest, paramB

proc unravel(c: var LiftingCtx; typ: TypeCursor; paramA, paramB: TokenBuf) =
  # `unravel`'s job is to "expand" the object fields in contrast to `lift`.
  if isTrivial(c, typ):
    genTrivialOp c, paramA, paramB
  else:
    let fn = lift(c, typ)
    maybeCallHook c, fn, paramA, paramB

proc unravelDispatch(c: var LiftingCtx; typ: TypeCursor; paramA, paramB: TokenBuf) =
  #if isTrivial(c, typ):
  #  genTrivialOp c, paramA, paramB
  #  return
  let typ = toTypeImpl typ
  case typ.typeKind
  of ObjectT:
    unravelObj c, typ, paramA, paramB
  of DistinctT:
    unravelDispatch(c, typ.firstSon, paramA, paramB)
  of TupleT:
    unravelTuple c, typ, paramA, paramB
  of ArrayT:
    unravelArray c, typ, paramA, paramB
  else:
    discard "nothing to do"
    #let fn = lift(c, typ)
    #maybeCallHook c, fn, paramA, paramB

proc addParamWithModifier(c: var LiftingCtx; param: SymId; typ: TypeCursor; modifier: TypeKind) =
  copyIntoKind(c.dest, ParamY, c.info):
    addSymDef c.dest, param, c.info
    c.dest.addEmpty2 c.info # export marker, pragmas
    copyIntoKind(c.dest, modifier, c.info):
      copyTree c.dest, typ
    c.dest.addEmpty c.info # value

proc addParam(c: var LiftingCtx; param: SymId; typ: TypeCursor) =
  copyIntoKind(c.dest, ParamY, c.info):
    addSymDef c.dest, param, c.info
    c.dest.addEmpty2 c.info # export marker, pragmas
    copyTree c.dest, typ
    c.dest.addEmpty c.info # value

proc maybeAddResultDecl(c: var LiftingCtx; res: SymId; typ: TypeCursor) =
  if c.op == attachedDup:
    copyIntoKind(c.dest, VarS, c.info):
      addSymDef c.dest, res, c.info
      c.dest.addEmpty2 c.info # export marker, pragmas
      copyTree c.dest, typ
      c.dest.addEmpty c.info # value

proc maybeAddReturn(c: var LiftingCtx; res: SymId) =
  if c.op == attachedDup:
    copyIntoKind(c.dest, RetS, c.info):
      copyIntoSymUse c.dest, res, c.info

proc publishProc(sym: SymId; dest: TokenBuf; procStart: int) =
  var buf = createTokenBuf(100)
  for i in procStart ..< dest.len: buf.add dest[i]
  programs.publish(sym, buf)

proc genProcDecl(c: var LiftingCtx; sym: SymId; typ: TypeCursor) =
  let paramA = pool.syms.getOrIncl("dest.0")
  var paramTreeA = createTokenBuf(4)
  copyIntoSymUse paramTreeA, paramA, c.info

  var paramB = SymId(0)
  var paramTreeB = createTokenBuf(4)

  case c.op
  of attachedDestroy, attachedWasMoved: discard
  of attachedCopy, attachedSink, attachedTrace, attachedDup:
    paramB = pool.syms.getOrIncl("src.0")
    copyIntoSymUse paramTreeB, paramB, c.info

  let procStart = c.dest.len
  copyIntoKind(c.dest, ProcS, c.info):
    addSymDef c.dest, sym, c.info
    c.dest.addEmpty3 c.info # export marker, pattern, generics

    c.dest.addParLe ParamsT, c.info
    case c.op
    of attachedDestroy:
      addParam c, paramA, typ
      c.dest.addParRi()
      c.dest.addEmpty() # void return type
    of attachedWasMoved:
      addParamWithModifier c, paramA, typ, MutT
      c.dest.addParRi()
      c.dest.addEmpty() # void return type
    of attachedDup:
      addParam c, paramB, typ
      c.dest.addParRi()
      c.dest.copyTree typ
    of attachedCopy, attachedSink:
      addParamWithModifier c, paramA, typ, MutT
      addParam c, paramB, typ
      c.dest.addParRi()
      c.dest.addEmpty() # void return type
    of attachedTrace:
      addParamWithModifier c, paramA, typ, MutT
      copyIntoKind(c.dest, ParamY, c.info):
        addSymDef c.dest, paramB, c.info
        c.dest.addEmpty2 c.info # export marker, pragmas
        copyIntoKind(c.dest, PointerT, c.info): discard
        c.dest.addEmpty c.info # value

      c.dest.addParRi()
      c.dest.addEmpty() # void return type

    copyIntoKind c.dest, PragmasS, c.info:
      copyIntoKind c.dest, NodestroyP, c.info: discard
      let pragmasPos = c.dest.len

    c.dest.addEmpty c.info # exc

    let a = toTypeImpl typ
    copyIntoKind(c.dest, StmtsS, c.info):
      maybeAddResultDecl c, paramA, typ
      let beforeUnravel = c.dest.len
      if a.typeKind == RefT:
        unravelRef(c, typ, paramTreeA, paramTreeB)
      else:
        unravelDispatch(c, typ, paramTreeA, paramTreeB)
      if c.dest.len == beforeUnravel:
        assert false, "empty hook created"
      maybeAddReturn c, paramA

  if c.calledErrorHook != NoLineInfo:
    c.dest.insert [parLeToken(ErrorP, c.calledErrorHook), parRiToken(c.calledErrorHook)], pragmasPos

  publishProc(sym, c.dest, procStart)

proc genMissingHooks*(c: var LiftingCtx) =
  # remember that genProcDecl does mutate c.requests so be robust against that:
  while c.requests.len > 0:
    let reqs = move(c.requests)
    for i in 0 ..< reqs.len:
      c.op = reqs[i].op
      c.calledErrorHook = NoLineInfo
      genProcDecl(c, reqs[i].sym, reqs[i].typ)

proc createLiftingCtx*(thisModuleSuffix: string): ref LiftingCtx =
  (ref LiftingCtx)(op: attachedDestroy, info: NoLineInfo, thisModuleSuffix: thisModuleSuffix)

proc getHook*(c: var LiftingCtx; op: AttachedOp; typ: TypeCursor; info: PackedLineInfo): SymId =
  c.op = op
  c.calledErrorHook = NoLineInfo
  c.info = info
  let t = if typ.typeKind == SinkT: typ.firstSon else: typ
  result = lift(c, t)

proc getDestructor*(c: var LiftingCtx; typ: TypeCursor; info: PackedLineInfo): SymId =
  getHook(c, attachedDestroy, typ, info)

when isMainModule:
  import std/os
  setupProgramForTesting getCurrentDir() / "nimcache", "test.nim", ".nif"
  let res = tryLoadHook(attachedDestroy, pool.syms.getOrIncl(StringName))
  if res != SymId(0):
    echo pool.syms[res]
  else:
    echo "no hook"
