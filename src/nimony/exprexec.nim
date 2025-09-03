#       Nimony
# (c) Copyright 2024 Andreas Rumpf
#
# See the file "license.txt", included in this
# distribution, for details about the copyright.

## Can run arbitrary expressions at compile-time by using `selfExec`.

include nifprelude
import std / [assertions, sets, tables]
import nimony_model, decls, programs, xints, semdata, symparser, renderer, builtintypes, typeprops, typenav, typekeys, expreval, semos

const
  writeNifModuleSuffix = "wriwhv7qv"

proc addSubtreeAndSyms(result: var TokenBuf; c: Cursor; stack: var seq[SymId]) =
  assert c.kind != ParRi, "cursor at end?"
  if c.kind != ParLe:
    # atom:
    result.add c.load
  else:
    var c = c
    var nested = 0
    while true:
      let item = c.load
      result.add item
      if item.kind == ParRi:
        dec nested
        if nested == 0: break
      elif item.kind == ParLe: inc nested
      elif item.kind == Symbol: stack.add item.symId
      inc c

proc collectUsedSyms(c: var SemContext; dest: var TokenBuf; usedModules: var HashSet[string]; routine: Routine) =
  var stack = newSeq[SymId]()
  var handledSyms = initHashSet[SymId]()
  stack.add routine.name.symId
  while stack.len > 0:
    let sym = stack.pop()
    if not handledSyms.containsOrIncl(sym):
      let owner = extractModule(pool.syms[sym])
      if owner == c.thisModuleSuffix:
        # add sym's declaration to `dest`:
        let res = tryLoadSym(sym)
        if res.status != LacksNothing:
          dest.addSubtreeAndSyms res.decl, stack
      elif owner.len > 0:
        usedModules.incl(owner)

type
  GenProcRequest = object
    sym: SymId
    typ: TypeCursor

  LiftingCtx = object
    dest: TokenBuf
    info: PackedLineInfo
    routineKind: SymKind
    hookNames: Table[string, int]
    thisModuleSuffix: string
    bits: int
    structuralTypeToProc: Table[string, SymId]
    requests: seq[GenProcRequest]
    usedModules: HashSet[string]
    errorMsg: string

proc generateName(c: var LiftingCtx; key: string): string =
  result = "`toNif" & "_" & key
  var counter = addr c.hookNames.mgetOrPut(result, -1)
  counter[] += 1
  result.add '.'
  result.addInt counter[]
  result.add '.'
  result.add c.thisModuleSuffix

proc requestProc(c: var LiftingCtx; t: TypeCursor): SymId =
  let key = mangle(t, Frontend, c.bits)
  result = c.structuralTypeToProc.getOrDefault(key)
  if result == SymId(0):
    let name = generateName(c, key)
    result = pool.syms.getOrIncl(name)
    c.requests.add GenProcRequest(sym: result, typ: t)
    c.structuralTypeToProc[key] = result

when not defined(nimony):
  proc unravel(c: var LiftingCtx; orig: TypeCursor; param: TokenBuf)
  proc entryPoint(c: var LiftingCtx; orig: TypeCursor; arg: Cursor)

proc genStringCall(c: var LiftingCtx; name, arg: string) =
  c.dest.copyIntoKind CallS, c.info:
    c.dest.addSymUse pool.syms.getOrIncl(name & ".0." & writeNifModuleSuffix), c.info
    c.dest.addStrLit arg, c.info

proc genParRiCall(c: var LiftingCtx) =
  c.dest.copyIntoKind CallS, c.info:
    c.dest.addSymUse pool.syms.getOrIncl("writeNifParRi.0." & writeNifModuleSuffix), c.info

proc accessObjField(c: var LiftingCtx; obj: TokenBuf; name: Cursor; needsDeref: bool; depth = 0): TokenBuf =
  assert name.kind == SymbolDef
  let nameSym = name.symId
  result = createTokenBuf(4)
  copyIntoKind(result, DotX, c.info):
    if needsDeref:
      result.addParLe HderefX, c.info
    copyTree result, obj
    if needsDeref:
      result.addParRi()
    copyIntoSymUse result, nameSym, c.info
    result.addIntLit(depth, c.info)

proc accessTupField(c: var LiftingCtx; tup: TokenBuf; idx: int): TokenBuf =
  result = createTokenBuf(4)
  copyIntoKind(result, TupatX, c.info):
    copyTree result, tup
    result.add intToken(pool.integers.getOrIncl(idx), c.info)

proc unravelObjField(c: var LiftingCtx; n: var Cursor; param: TokenBuf; needsDeref: bool; depth: int) =
  let r = takeLocal(n, SkipFinalParRi)
  assert r.kind == FldY
  # create `paramA.field` because we need to do `paramA.field = paramB.field` etc.
  let fieldType = r.typ
  let a = accessObjField(c, param, r.name, needsDeref, depth = depth)

  genStringCall(c, "writeNifParLe", "kv")
  genStringCall(c, "writeNifSymbol", pool.syms[r.name.symId])

  entryPoint(c, fieldType, readOnlyCursorAt(a, 0))
  genParRiCall c

proc unravelObjFields(c: var LiftingCtx; n: var Cursor; param: TokenBuf; needsDeref: bool; depth: int) =
  while n.kind != ParRi:
    case n.substructureKind
    of CaseU:
      let info = n.info
      inc n
      var selector = n
      unravelObjField c, selector, param, needsDeref, depth

      var selectorField = takeLocal(n, SkipFinalParRi)
      let sel = accessObjField(c, param, selectorField.name, needsDeref)

      c.dest.addParLe CaseU, info
      c.dest.add sel

      while n.kind != ParRi:
        case n.substructureKind
        of OfU:
          c.dest.takeToken(n)
          c.dest.takeTree(n)
          assert n.stmtKind == StmtsS
          c.dest.takeToken(n)
          unravelObjFields c, n, param, needsDeref, depth
          takeParRi(c.dest, n)
          takeParRi(c.dest, n)
        of ElseU:
          c.dest.takeToken(n)
          assert n.stmtKind == StmtsS
          c.dest.takeToken(n)
          unravelObjFields c, n, param, needsDeref, depth
          takeParRi(c.dest, n)
          takeParRi(c.dest, n)
        else:
          error "expected `of` or `else` inside `case`"

      takeParRi(c.dest, n) # end of case

    of FldU:
      unravelObjField c, n, param, needsDeref, depth
    of NilU:
      skip n
    else:
      error "illformed AST inside object: ", n


proc unravelObj(c: var LiftingCtx; orig: Cursor; param: TokenBuf; depth: int) =
  genStringCall(c, "writeNifParLe", "oconstr")
  # we simply generate the type as a raw string:
  genStringCall(c, "writeNifRaw", toString(orig, false))

  var n = orig
  let needsDeref = n.typeKind in {RefT, PtrT}
  if n.typeKind in {RefT, PtrT}:
    inc n
  assert n.typeKind == ObjectT
  inc n
  # recurse for the inherited object type, if any:
  if n.kind != DotToken:
    var parent = n
    if parent.typeKind in {RefT, PtrT}:
      inc parent
    unravelObj c, toTypeImpl(parent), param, depth+1
  skip n # inheritance is gone
  unravelObjFields c, n, param, needsDeref, depth
  genParRiCall c

proc unravelTuple(c: var LiftingCtx;
                  orig: Cursor; param: TokenBuf) =
  assert orig.typeKind == TupleT
  genStringCall(c, "writeNifParLe", "tupconstr")
  # we simply generate the type as a raw string:
  genStringCall(c, "writeNifRaw", toString(orig, false))

  var n = orig
  inc n
  var idx = 0
  while n.kind != ParRi:
    let fieldType = getTupleFieldType(n)
    skip n

    let a = accessTupField(c, param, idx)
    unravel c, fieldType, a
    inc idx
  genParRiCall c


proc accessArrayAt(c: var LiftingCtx; arr: TokenBuf; indexVar: SymId): TokenBuf =
  result = createTokenBuf(4)
  copyIntoKind result, ArrAtX, c.info:
    copyTree result, arr
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
                  orig: Cursor; param: TokenBuf) =
  assert orig.typeKind == ArrayT
  let arrayLen = getArrayLen(orig)
  var n = orig
  inc n
  let baseType = n

  let indexVar = pool.syms.getOrIncl("idx.0")
  declareIndexVar c, indexVar

  genStringCall(c, "writeNifParLe", "aconstr")
  # we simply generate the type as a raw string:
  genStringCall(c, "writeNifRaw", toString(orig, false))

  copyIntoKind c.dest, WhileS, c.info:
    indexVarLowerThanArrayLen c, indexVar, arrayLen
    copyIntoKind c.dest, StmtsS, c.info:
      let a = accessArrayAt(c, param, indexVar)
      unravel c, baseType, a

      incIndexVar c, indexVar
  genParRiCall c

proc unravelSet(c: var LiftingCtx; orig: TypeCursor; param: TokenBuf) =
  assert orig.typeKind == SetT
  let baseType = orig.firstSon
  let maxValue = bitsetSizeInBytes(orig) * createXint(8'i64)
  genStringCall(c, "writeNifParLe", "setconstr")
  genStringCall(c, "writeNifRaw", toString(orig, false))

  let indexVar = pool.syms.getOrIncl("idx.0")
  declareIndexVar c, indexVar
  var indexVarAsBuf = createTokenBuf(1)
  indexVarAsBuf.addSymUse indexVar, c.info

  copyIntoKind c.dest, WhileS, c.info:
    indexVarLowerThanArrayLen c, indexVar, maxValue
    copyIntoKind c.dest, StmtsS, c.info:
      copyIntoKind c.dest, IfS, c.info:
        copyIntoKind c.dest, ElifU, c.info:
          copyIntoKind c.dest, InSetX, c.info:
            c.dest.addSubtree orig
            c.dest.add param # param is the set, so it comes first
            # the element is our indexVar
            c.dest.addSymUse indexVar, c.info
          copyIntoKind c.dest, StmtsS, c.info:
           unravel c, baseType, indexVarAsBuf

      incIndexVar c, indexVar
  genParRiCall c

proc unravelEnum(c: var LiftingCtx; orig: TypeCursor; param: TokenBuf) =
  c.dest.addParLe CaseS, c.info
  c.dest.add param
  var enumDecl = orig
  inc enumDecl # skips enum
  skip enumDecl # skips base type
  while enumDecl.kind != ParRi:
    let enumDeclInfo = enumDecl.info
    c.dest.copyIntoKind OfU, enumDeclInfo:
      c.dest.copyIntoKind RangesU, enumDeclInfo:
        let enumField = takeLocal(enumDecl, SkipFinalParRi)
        let esym = enumField.name.symId
        c.dest.addSymUse esym, enumDeclInfo
      c.dest.copyIntoKind StmtsS, enumDeclInfo:
        genStringCall(c, "writeNifSymbol", pool.syms[esym])
  c.dest.addParRi() # case

proc primitiveCall(c: var LiftingCtx; name: string; arg: Cursor) =
  c.dest.copyIntoKind CallS, c.info:
    c.dest.addSymUse pool.syms.getOrIncl(name & ".0." & writeNifModuleSuffix), c.info
    c.dest.addSubtree arg

proc entryPoint(c: var LiftingCtx; orig: TypeCursor; arg: Cursor) =
  if isStringType(orig):
    primitiveCall(c, "writeNifStr", arg)
    return
  elif orig.typeKind == CstringT:
    genStringCall(c, "writeNifParLe", "conv")
    genStringCall(c, "writeNifRaw", toString(orig, false))
    primitiveCall(c, "writeNifStr", arg)
    genParRiCall c
    return

  let typ = toTypeImpl orig
  case typ.typeKind
  of DistinctT, RangetypeT:
    # do not lose the type:
    genStringCall(c, "writeNifParLe", "conv")
    # we simply generate the type as a raw string:
    genStringCall(c, "writeNifRaw", toString(orig, false))
    entryPoint(c, typ.firstSon, arg)
    genParRiCall c
  of IT:
    primitiveCall(c, "writeNifInt", arg)
  of UT:
    primitiveCall(c, "writeNifUInt", arg)
  of CT:
    primitiveCall(c, "writeNifChar", arg)
  of FloatT:
    primitiveCall(c, "writeNifFloat", arg)
  of BoolT:
    primitiveCall(c, "writeNifBool", arg)
  else:
    let procId = requestProc(c, orig)
    c.dest.copyIntoKind CallS, c.info:
      c.dest.addSymUse procId, c.info
      c.dest.addSubtree arg

proc unravel(c: var LiftingCtx; orig: TypeCursor; param: TokenBuf) =
  if isSomeStringType(orig):
    entryPoint(c, orig, readOnlyCursorAt(param, 0))
    return

  let typ = toTypeImpl orig
  case typ.typeKind
  of ObjectT:
    if orig.kind == Symbol and hasRtti(orig.symId):
      c.routineKind = MethodY
    unravelObj c, typ, param, 0
  of TupleT:
    unravelTuple c, typ, param
  of ArrayT:
    unravelArray c, typ, param
  of IT, UT, FT, CT, BoolT, DistinctT, RangetypeT:
    entryPoint(c, typ, readOnlyCursorAt(param, 0))
  of EnumT, OnumT:
    unravelEnum c, typ, param
  of SetT:
    unravelSet c, typ, param
  of NoType, ErrT, AtT, AndT, OrT, NotT, ProcT, FuncT, IteratorT, ConverterT, MethodT, MacroT, TemplateT,
     ProctypeT,  VoidT, PtrT, VarargsT, StaticT,
     RefT, MutT, OutT, LentT, SinkT, NiltT, ConceptT, ItertypeT, UarrayT, AutoT,
     SymkindT, TypekindT, TypedescT, UntypedT, TypedT, CstringT, PointerT, OrdinalT:
    c.errorMsg = "unsupported type for compile-time evaluation: " & asNimCode(orig)

proc publishProc(sym: SymId; dest: TokenBuf; procStart: int) =
  var buf = createTokenBuf(100)
  for i in procStart ..< dest.len: buf.add dest[i]
  programs.publish(sym, buf)

proc genProcDecl(c: var LiftingCtx; sym: SymId; typ: TypeCursor) =
  let paramA = pool.syms.getOrIncl("dest.0")
  var paramTreeA = createTokenBuf(4)
  copyIntoSymUse paramTreeA, paramA, c.info

  let procStart = c.dest.len
  copyIntoKind(c.dest, ProcS, c.info):
    addSymDef c.dest, sym, c.info
    c.dest.addEmpty3 c.info # export marker, pattern, generics

    c.dest.addParLe ParamsU, c.info

    copyIntoKind(c.dest, ParamY, c.info):
      addSymDef c.dest, paramA, c.info
      c.dest.addEmpty2 c.info # export marker, pragmas
      copyTree c.dest, typ
      c.dest.addEmpty c.info # value

    c.dest.addParRi()
    c.dest.addEmpty() # void return type

    c.dest.addEmpty() # pragmas
    c.dest.addEmpty c.info # exc

    copyIntoKind(c.dest, StmtsS, c.info):
      let beforeUnravel = c.dest.len
      unravel(c, typ, paramTreeA)
      if c.dest.len == beforeUnravel:
        assert false, "empty hook created"
  # tell vtables.nim we need dynamic binding here:
  if c.routineKind == MethodY:
    c.dest[procStart] = parLeToken(MethodS, c.info)

  publishProc(sym, c.dest, procStart)

proc genMissingProcs*(c: var LiftingCtx) =
  # remember that genProcDecl does mutate c.requests so be robust against that:
  while c.requests.len > 0:
    let reqs = move(c.requests)
    for i in 0 ..< reqs.len:
      c.routineKind = ProcY
      genProcDecl(c, reqs[i].sym, reqs[i].typ)

proc executeCall*(s: var SemContext; routine: Routine; dest: var TokenBuf; call: Cursor; info: PackedLineInfo): string {.nimcall.} =
  var c = LiftingCtx(dest: createTokenBuf(150), info: info, routineKind: ProcY, bits: s.g.config.bits, errorMsg: "", thisModuleSuffix: s.thisModuleSuffix)

  c.dest.addParLe StmtsS, info
  collectUsedSyms s, c.dest, c.usedModules, routine

  # now that we have all dependencies in the module, we can add the call, but wrap it in a new `toNif` tag:
  if isVoidType(routine.retType):
    # if the call is void, we can just emit the code for it directly here:
    c.dest.addSubtree call
  else:
    # else we produce `toNif fn(args)` where `toNif` is built by the complex `lifter` machinery.
    entryPoint(c, routine.retType, call)

  genMissingProcs c
  c.dest.addParRi() # StmtsS

  if c.errorMsg.len > 0:
    result = ensureMove c.errorMsg
  else:
    result = runEval(s, dest, c.dest, c.usedModules)
