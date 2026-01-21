#       Nimony
# (c) Copyright 2024 Andreas Rumpf
#
# See the file "license.txt", included in this
# distribution, for details about the copyright.

## Can run arbitrary expressions at compile-time by using `selfExec`.

include nifprelude
import ".." / lib / nifchecksums
from std / os import `/`
import std / [assertions, sets, tables]
import nimony_model, decls, programs, xints, semdata, symparser, renderer, builtintypes, typeprops,
  typenav, typekeys, expreval, semos, derefs

const
  ParamSymName = "dest.0"

when false:
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

type
  GenProcRequest = object
    sym: SymId
    typ: TypeCursor

  LiftingCtx = object
    dest: TokenBuf
    info: PackedLineInfo
    routineKind: SymKind
    hookNames: Table[string, int]
    thisModuleSuffix, newModuleSuffix: string
    bits: int
    structuralTypeToProc: Table[string, SymId]
    requests: seq[GenProcRequest]
    usedModules: HashSet[string]
    errorMsg: string

proc collectSyms(n: Cursor; stack: var seq[SymId]) =
  assert n.kind != ParRi, "cursor at end?"
  if n.kind != ParLe:
    # atom:
    if n.kind == Symbol: stack.add n.symId
  else:
    var n = n
    var nested = 0
    while true:
      case n.kind
      of ParRi:
        dec nested
        if nested == 0: break
      of ParLe: inc nested
      of Symbol: stack.add n.symId
      else: discard
      inc n

proc rewriteSyms*(c: var LiftingCtx) =
  for i in 0 ..< c.dest.len:
    if c.dest[i].kind in {Symbol, SymbolDef}:
      let m = splitSymName(pool.syms[c.dest[i].symId])
      if m.module == c.thisModuleSuffix:
        let newSym = pool.syms.getOrIncl(m.name & "." & c.newModuleSuffix)
        c.dest[i].setSymId newSym

proc collectUsedSyms(c: var LiftingCtx; s: var SemContext; routine: Routine) =
  var stack = newSeq[SymId]()
  var handledSyms = initHashSet[SymId]()
  stack.add routine.name.symId
  # Always add `system.nim` as a dependency:
  c.usedModules.incl(s.g.config.nifcachePath / SystemModuleSuffix)
  while stack.len > 0:
    let sym = stack.pop()
    if not handledSyms.containsOrIncl(sym):
      let owner = extractModule(pool.syms[sym])
      if owner == c.thisModuleSuffix:
        # add sym's declaration to `dest`:
        let res = tryLoadSym(sym)
        if res.status == LacksNothing:
          let before = c.dest.len
          # we need to copy res.decl here as it aliases prog.mem which the semchecker will overwrite nilly-willy!
          var newDecl = createTokenBuf(50)
          newDecl.addSubtree res.decl
          s.semStmtCallback(s, c.dest, cursorAt(newDecl, 0))
          collectSyms(cursorAt(c.dest, before), stack)
          endRead(c.dest)
          #dest.addSubtreeAndSyms res.decl, stack
      elif owner.len > 0:
        c.usedModules.incl(s.g.config.nifcachePath / owner)

proc generateName(c: var LiftingCtx; key: string): string =
  result = "`toNif" & "_" & key
  var counter = addr c.hookNames.mgetOrPut(result, -1)
  counter[] += 1
  result.add '.'
  result.addInt counter[]
  result.add '.'
  result.add c.thisModuleSuffix # will later be rewired to use `c.newModuleSuffix`

proc genProcHeader(c: var LiftingCtx; dest: var TokenBuf; sym: SymId; typ: TypeCursor) =
  # Leaves the declaration open at the position of the body.
  dest.addParLe ProcS, c.info
  addSymDef dest, sym, c.info
  dest.addEmpty3 c.info # export marker, pattern, generics
  copyIntoKind dest, ParamsU, c.info:
    copyIntoKind dest, ParamY, c.info:
      addSymDef dest, pool.syms.getOrIncl(ParamSymName), c.info
      dest.addEmpty2 c.info # export marker, pragmas
      copyTree dest, typ
      dest.addEmpty c.info # value
  dest.addEmpty() # void return type
  dest.addEmpty() # pragmas
  dest.addEmpty c.info # exc

proc requestProc(c: var LiftingCtx; t: TypeCursor): SymId =
  let key = mangle(t, Frontend, c.bits)
  result = c.structuralTypeToProc.getOrDefault(key)
  if result == SymId(0):
    let name = generateName(c, key)
    result = pool.syms.getOrIncl(name)
    c.requests.add GenProcRequest(sym: result, typ: t)
    c.structuralTypeToProc[key] = result

    var header = createTokenBuf(30)
    genProcHeader(c, header, result, t)
    header.addEmpty() # body is empty
    header.addParRi() # close ProcS declaration
    programs.publish(result, header)

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
  freeze result

proc accessTupField(c: var LiftingCtx; tup: TokenBuf; idx: int): TokenBuf =
  result = createTokenBuf(4)
  copyIntoKind(result, TupatX, c.info):
    copyTree result, tup
    result.add intToken(pool.integers.getOrIncl(idx), c.info)
  freeze result

proc unravelObjField(c: var LiftingCtx; n: var Cursor; param: TokenBuf; needsDeref: bool; depth: int) =
  let r = takeLocal(n, SkipFinalParRi)
  assert r.kind == FldY
  # create `paramA.field` because we need to do `paramA.field = paramB.field` etc.
  let fieldType = r.typ
  let a = accessObjField(c, param, r.name, needsDeref, depth = depth)

  genStringCall(c, "writeNifParLe", "kv")
  genStringCall(c, "writeNifRaw", " ")
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
  freeze result

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
  freeze indexVarAsBuf

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

proc genProcDecl(c: var LiftingCtx; sym: SymId; typ: TypeCursor) =
  let paramA = pool.syms.getOrIncl(ParamSymName)
  var paramTreeA = createTokenBuf(4)
  copyIntoSymUse paramTreeA, paramA, c.info
  freeze paramTreeA

  let procStart = c.dest.len
  genProcHeader(c, c.dest, sym, typ)

  copyIntoKind(c.dest, StmtsS, c.info):
    let beforeUnravel = c.dest.len
    unravel(c, typ, paramTreeA)
    if c.dest.len == beforeUnravel:
      assert false, "empty hook created"
  c.dest.addParRi() # close ProcS declaration
  # tell vtables.nim we need dynamic binding here:
  if c.routineKind == MethodY:
    c.dest[procStart] = parLeToken(MethodS, c.info)

proc genMissingProcs*(c: var LiftingCtx) =
  # remember that genProcDecl does mutate c.requests so be robust against that:
  while c.requests.len > 0:
    let reqs = move(c.requests)
    for i in 0 ..< reqs.len:
      c.routineKind = ProcY
      genProcDecl(c, reqs[i].sym, reqs[i].typ)

proc executeCall*(s: var SemContext; routine: Routine; dest: var TokenBuf; call: Cursor; info: PackedLineInfo): string {.nimcall.} =
  let prepResult = semos.prepareEval(s)
  if prepResult.len > 0: return prepResult

  var c = LiftingCtx(dest: createTokenBuf(150), info: info, routineKind: ProcY, bits: s.g.config.bits,
    errorMsg: "", thisModuleSuffix: s.thisModuleSuffix,
    newModuleSuffix: s.thisModuleSuffix.substr(0, 2) & computeChecksum(mangle(call, Frontend, s.g.config.bits)))

  c.dest.addParLe StmtsS, info

  c.dest.copyIntoKind CallS, info:
    c.dest.addSymUse pool.syms.getOrIncl("setup.0." & writeNifModuleSuffix), info
    c.dest.addStrLit s.g.config.nifcachePath / c.newModuleSuffix & ".out.nif", info

  var retTypeBuf = createTokenBuf(4) # the aliasing also causes `routine.retType` to alias `prog.mem`!
  retTypeBuf.addSubtree routine.retType
  var retType = cursorAt(retTypeBuf, 0)

  let beforeUsercode = c.dest.len

  c.dest.copyIntoKind StmtsS, info:
    collectUsedSyms c, s, routine

    # now that we have all dependencies in the module, we can add the call, but wrap it in a new `toNif` tag:
    if isVoidType(retType):
      # if the call is void, we can just emit the code for it directly here:
      c.dest.addSubtree call
    else:
      # else we produce `toNif fn(args)` where `toNif` is built by the complex `lifter` machinery.
      entryPoint(c, retType, call)

  let toDeref = cursorAt(c.dest, beforeUsercode)
  #echo "synthesized ", toString(toDeref)
  let withDerefs = injectDerefs(toDeref)
  endRead(c.dest)
  c.dest.shrink beforeUsercode
  # do not copy the `(stmts)` here:
  assert withDerefs.len >= 2
  assert withDerefs[0].kind == ParLe
  assert withDerefs[withDerefs.len-1].kind == ParRi
  for i in 1..<withDerefs.len-1:
    c.dest.add withDerefs[i]

  c.dest.copyIntoKind CallS, info:
    c.dest.addSymUse pool.syms.getOrIncl("teardown.0." & writeNifModuleSuffix), info

  # we know that the generated code doesn't require any deref
  # insertions so we do it after `injectDerefs`:
  genMissingProcs c
  c.dest.addParRi() # StmtsS

  rewriteSyms c

  if c.errorMsg.len > 0:
    result = ensureMove c.errorMsg
  else:
    result = runEval(s, dest, c.newModuleSuffix, c.dest, c.usedModules)
