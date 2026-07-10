#       Nimony
# (c) Copyright 2024 Andreas Rumpf
#
# See the file "license.txt", included in this
# distribution, for details about the copyright.

## Can run arbitrary expressions at compile-time by using `selfExec`.

include ".." / lib / nifprelude
include ".." / lib / compat2
import ".." / lib / nifchecksums
from std / os import `/`
import std / [assertions, sets, tables, hashes, syncio]
import ".." / models / tags
import nimony_model, decls, programs, xints, semdata, renderer, builtintypes, typeprops,
  typenav, typekeys, expreval, semos, derefs
import ".." / lib / symparser

const
  ParamSymName = "dest.0"

when false:
  proc addSubtreeAndSyms(result: var TokenBuf; c: Cursor; stack: var seq[SymId]) =
    assert c.hasMore, "cursor at end?"
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

  SynthesizeSerializerCtx = object
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
    semCtx: ptr SemContext   # used to resolve `InvokeT` types via sem's
                             # `instantiateType` (kept as a ptr so this
                             # struct stays trivially-copyable; the SemContext
                             # outlives `executeExpr`'s whole call).

proc collectSyms(n: var Cursor; stack: var seq[SymId]) =
  ## Collects every `Symbol` in the subtree at `n`, advancing `n` past it.
  if n.kind != ParLe:
    # atom:
    if n.kind == Symbol: stack.add n.symId
    inc n
  else:
    n.loopInto:
      collectSyms(n, stack)

proc instantiationTypevars(sym: SymId): Cursor =
  ## Returns the instantiated proc/type decl's `typevars` slot if `sym`
  ## is a generic instance (typevars is an `InvokeT` recording the origin
  ## and the type args). Returns `default(Cursor)` otherwise. Procs use
  ## counter-based names that don't match `isInstantiation`, so this
  ## decl-based check is the reliable cross-kind detector.
  result = default(Cursor)
  let res = tryLoadSym(sym)
  if res.status == LacksNothing:
    let decl = res.decl
    if isRoutine(decl.symKind):
      let tv = asRoutine(decl).typevars
      if tv.typeKind == InvokeT:
        result = tv
    elif decl.symKind == TypeY:
      let tv = asTypeDecl(decl).typevars
      if tv.typeKind == InvokeT:
        result = tv

proc emitSymAsIdent(buf: var TokenBuf; sym: SymId; info: PackedLineInfo;
                    thisMod: string)

proc emitTreeAsIdents(buf: var TokenBuf; n: var Cursor; thisMod: string) =
  ## Copies the subtree at `n`, rewriting every `Symbol` to an ident (or
  ## `(at ...)` form for instantiations) via `emitSymAsIdent`.
  case n.kind
  of Symbol:
    emitSymAsIdent(buf, n.symId, n.info, thisMod)
    inc n
  of ParLe:
    buf.add n
    n.into:
      while n.hasMore:
        emitTreeAsIdents(buf, n, thisMod)
    buf.addParRi()
  else:
    buf.add n
    inc n

proc emitSymAsIdent(buf: var TokenBuf; sym: SymId; info: PackedLineInfo;
                    thisMod: string) =
  ## Strip an instantiated/non-instantiated Symbol to its basename Ident.
  ## For an instantiated Symbol, also emit the surrounding `(at <basename>
  ## <typeArgs>)` so the sub-compile can re-instantiate from the generic
  ## origin. The type args themselves may contain instantiated Symbols;
  ## those are rewritten recursively (an arg subtree is walked and each
  ## Symbol inside passes through this same path).
  ##
  ## *Foreign type Symbols* are kept verbatim as Symbol tokens — Symbol
  ## references in NIF resolve via direct pool lookup in the sub-compile
  ## (no ident-name resolution, so no visibility check). That lets the
  ## generated serializer reference private types from other modules
  ## (e.g. `HashEntry` in `std/tables`) without those libraries needing
  ## to re-export them just for the const-eval machinery.
  let typevars = instantiationTypevars(sym)
  if not cursorIsNil(typevars):
    buf.add parLeToken(AtX, info)
    var tv = typevars
    tv.into: # past `invok` tag
      # Origin sym → recurse (it may also be instantiated, e.g. nested generics).
      emitSymAsIdent(buf, tv.symId, info, thisMod)
      inc tv
      # Type args: each arg is a subtree (may contain Symbols).
      while tv.hasMore:
        var argCur = tv
        emitTreeAsIdents(buf, argCur, thisMod)
        skip tv
    buf.addParRi()
    return
  let symStr = pool.syms[sym]
  let owner = extractModule(symStr)
  if owner.len > 0 and owner != thisMod:
    let res = tryLoadSym(sym)
    if res.status == LacksNothing and res.decl.symKind == TypeY:
      # Foreign type — keep as Symbol so visibility is bypassed.
      buf.add symToken(sym, info)
      return
  var basename = symStr
  extractBasename basename
  buf.add identToken(pool.strings.getOrIncl(basename), info)

proc rewriteTreeToIdents(newDest: var TokenBuf; n: var Cursor; thisMod: string) =
  case n.kind
  of Symbol:
    emitSymAsIdent(newDest, n.symId, n.info, thisMod)
    inc n
  of SymbolDef:
    # SymbolDefs only appear at decl sites; always strip to basename
    # so the sub-compile creates fresh decls via re-semchecking.
    var basename = pool.syms[n.symId]
    extractBasename basename
    newDest.add identToken(pool.strings.getOrIncl(basename), n.info)
    inc n
  of ParLe:
    if n.exprKind in {OchoiceX, CchoiceX}:
      # eliminate the choice: keep only its first alternative, rewritten
      n.peekInto:
        if n.kind == Symbol:
          emitSymAsIdent(newDest, n.symId, n.info, thisMod)
        else:
          rewriteTreeToIdents(newDest, n, thisMod)
    else:
      newDest.add n
      n.into:
        while n.hasMore:
          rewriteTreeToIdents(newDest, n, thisMod)
      newDest.addParRi()
  of ParRi:
    bug "unpaired ')'"
  else:
    newDest.add n
    inc n

proc rewriteSymsToIdents*(c: var SynthesizeSerializerCtx) =
  ## Convert all Symbols and SymbolDefs to Idents so that the semchecker can resolve them fresh.
  ## Also eliminates choice nodes (ochoice, cchoice) by turning them back to idents.
  ## For instantiated Symbols, emit `(at <basename> <typeArgs>)` so the
  ## sub-compile can re-instantiate (header-only stubs don't cross the
  ## boundary; the generic origin is in the imported scope).
  ## This allows the compiled code to go through the full nimony pipeline.
  var newDest = createTokenBuf(c.dest.len)
  var n = beginRead(c.dest)
  rewriteTreeToIdents(newDest, n, c.thisModuleSuffix)
  endRead(c.dest)
  c.dest = ensureMove newDest

proc generateName(c: var SynthesizeSerializerCtx; key: string): string =
  result = "`toNif" & "_" & key
  var counter = addr c.hookNames.mgetOrPut(result, -1)
  counter[] += 1
  result.add '.'
  result.addInt counter[]
  result.add '.'
  result.add c.thisModuleSuffix # will later be rewired to use `c.newModuleSuffix`

proc genProcHeader(c: var SynthesizeSerializerCtx; dest: var TokenBuf; sym: SymId; typ: TypeCursor) =
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

proc requestProc(c: var SynthesizeSerializerCtx; t: TypeCursor): SymId =
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
  proc unravel(c: var SynthesizeSerializerCtx; orig: TypeCursor; param: TokenBuf)
  proc entryPoint(c: var SynthesizeSerializerCtx; orig: TypeCursor; arg: Cursor)
  proc unravelPtrUarrayField(c: var SynthesizeSerializerCtx; fieldType: Cursor; param: TokenBuf)

proc genStringCall(c: var SynthesizeSerializerCtx; name, arg: string) =
  c.dest.copyIntoKind CallS, c.info:
    c.dest.addSymUse pool.syms.getOrIncl(name & ".0." & writeNifModuleSuffix), c.info
    c.dest.addStrLit arg, c.info

proc genParRiCall(c: var SynthesizeSerializerCtx) =
  c.dest.copyIntoKind CallS, c.info:
    c.dest.addSymUse pool.syms.getOrIncl("writeNifParRi.0." & writeNifModuleSuffix), c.info

proc accessObjField(c: var SynthesizeSerializerCtx; obj: TokenBuf; name: Cursor; needsDeref: bool; depth = 0): TokenBuf =
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
    # trailing access-token: tells the dot-expression sem-check to bypass
    # field visibility so private fields can be read by the synthesized
    # serializer that runs in a separate sub-compile module.
    result.addStrLit "x", c.info
  freeze result

proc accessTupField(c: var SynthesizeSerializerCtx; tup: TokenBuf; idx: int): TokenBuf =
  result = createTokenBuf(4)
  copyIntoKind(result, TupatX, c.info):
    copyTree result, tup
    result.add intToken(pool.integers.getOrIncl(idx), c.info)
  freeze result

proc unravelObjField(c: var SynthesizeSerializerCtx; n: var Cursor; param: TokenBuf; needsDeref: bool; depth: int) =
  let r = takeLocal(n, SkipFinalParRi)
  assert r.kind in {FldY, GfldY}
  # create `paramA.field` because we need to do `paramA.field = paramB.field` etc.
  let fieldType = r.typ

  genStringCall(c, "writeNifParLe", "kv")
  genStringCall(c, "writeNifRaw", " ")
  genStringCall(c, "writeNifSymbol", pool.syms[r.name.symId])

  # ptr-to-nif special case: a `ptr UncheckedArray[T]` field can't be
  # serialised structurally — there's no pointer value that survives the
  # sub-compile boundary. Instead, dump the pointed-to data by iterating
  # the parent through `len`/`[]`, emitting an aconstr with type slot
  # `(ptr (uarray T))` which nifcgen hoists to a static.
  var isPtrUarray = false
  if fieldType.typeKind == PtrT:
    var inner = fieldType
    inc inner
    if inner.typeKind == UarrayT:
      isPtrUarray = true
  if isPtrUarray:
    unravelPtrUarrayField(c, fieldType, param)
  else:
    let a = accessObjField(c, param, r.name, needsDeref, depth = depth)
    entryPoint(c, fieldType, readonlyCursorAt(a, 0))
  genParRiCall c

proc unravelObjFields(c: var SynthesizeSerializerCtx; n: var Cursor; param: TokenBuf; needsDeref: bool; depth: int) =
  while n.hasMore:
    case n.substructureKind
    of CaseU:
      let info = n.info
      n.into:
        var selector = n
        unravelObjField c, selector, param, needsDeref, depth

        var selectorField = takeLocal(n, SkipFinalParRi)
        let sel = accessObjField(c, param, selectorField.name, needsDeref)

        c.dest.addParLe CaseU, info
        c.dest.add sel

        while n.hasMore:
          case n.substructureKind
          of OfU:
            copyInto c.dest, n:
              c.dest.takeTree(n)
              assert n.stmtKind == StmtsS
              copyInto c.dest, n:
                unravelObjFields c, n, param, needsDeref, depth
          of ElseU:
            copyInto c.dest, n:
              assert n.stmtKind == StmtsS
              copyInto c.dest, n:
                unravelObjFields c, n, param, needsDeref, depth
          else:
            error "expected `of` or `else` inside `case`"

      c.dest.addParRi() # end of case

    of FldU, GfldU:
      unravelObjField c, n, param, needsDeref, depth
    of NilU:
      skip n
    else:
      error "illformed AST inside object: ", n


proc unravelObj(c: var SynthesizeSerializerCtx; orig: Cursor; param: TokenBuf; depth: int) =
  genStringCall(c, "writeNifParLe", "oconstr")
  # we simply generate the type as a raw string:
  genStringCall(c, "writeNifRaw", toString(orig, false))

  var n = orig
  let needsDeref = n.typeKind in {RefT, PtrT}
  if n.typeKind in {RefT, PtrT}:
    inc n
  assert n.typeKind == ObjectT
  discard enterScope(n) # bound the field walk; `n` is a copy
  # recurse for the inherited object type, if any:
  if n.kind != DotToken:
    var parent = n
    if parent.typeKind in {RefT, PtrT}:
      inc parent
    unravelObj c, toTypeImpl(parent), param, depth+1
  skip n # inheritance is gone
  unravelObjFields c, n, param, needsDeref, depth
  genParRiCall c

proc unravelTuple(c: var SynthesizeSerializerCtx;
                  orig: Cursor; param: TokenBuf) =
  assert orig.typeKind == TupleT
  genStringCall(c, "writeNifParLe", "tupconstr")
  # we simply generate the type as a raw string:
  genStringCall(c, "writeNifRaw", toString(orig, false))

  var n = orig
  var idx = 0
  n.into:  # (tuple …)
    while n.hasMore:
      let fieldType = getTupleFieldType(n)
      skip n

      let a = accessTupField(c, param, idx)
      unravel c, fieldType, a
      inc idx
  genParRiCall c


proc accessArrayAt(c: var SynthesizeSerializerCtx; arr: TokenBuf; indexVar: SymId): TokenBuf =
  result = createTokenBuf(4)
  copyIntoKind result, ArratX, c.info:
    copyTree result, arr
    copyIntoSymUse result, indexVar, c.info
  freeze result

proc indexVarLowerThanArrayLen(c: var SynthesizeSerializerCtx; indexVar: SymId; arrayLen: xint) =
  copyIntoKind c.dest, LtX, c.info:
    copyIntoKind c.dest, IntT, c.info:
      c.dest.add intToken(pool.integers.getOrIncl(c.bits), c.info)
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

proc addIntType(c: var SynthesizeSerializerCtx) =
  copyIntoKind c.dest, IntT, c.info:
    c.dest.add intToken(pool.integers.getOrIncl(c.bits), c.info)

proc incIndexVar(c: var SynthesizeSerializerCtx; indexVar: SymId) =
  copyIntoKind c.dest, AsgnS, c.info:
    copyIntoSymUse c.dest, indexVar, c.info
    copyIntoKind c.dest, AddX, c.info:
      addIntType c
      copyIntoSymUse c.dest, indexVar, c.info
      c.dest.add intToken(pool.integers.getOrIncl(+1), c.info)

proc declareIndexVar(c: var SynthesizeSerializerCtx; indexVar: SymId) =
  copyIntoKind c.dest, VarY, c.info:
    addSymDef c.dest, indexVar, c.info
    c.dest.addEmpty2 c.info # not exported, no pragmas
    addIntType c
    c.dest.add intToken(pool.integers.getOrIncl(0), c.info)

proc unravelArray(c: var SynthesizeSerializerCtx;
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

proc unravelPtrUarrayField(c: var SynthesizeSerializerCtx;
                            fieldType: Cursor; param: TokenBuf) =
  ## ptr-to-nif for a `ptr UncheckedArray[T]` field. Emits runtime code
  ## that writes `(addr (aconstr (uarray T) e1 ... eN))` to NIF — the
  ## aconstr is the inline array data and `addr` lifts it to a pointer,
  ## matching how `addr` of a literal would compose naturally. The
  ## length comes from `len(param)` and elements from `param[i]` at
  ## runtime; both must resolve in the sub-compile (true for seq and
  ## string). Hexer's nifcgen pass hoists the aconstr to an anon
  ## module-level static and rewrites `addr` to point at it.
  var ft = fieldType
  inc ft # past ptr tag
  assert ft.typeKind == UarrayT
  let uarrayType = ft # (uarray T) — used as the aconstr's type slot
  inc ft # past uarray tag
  let baseType = ft

  let indexVar = pool.syms.getOrIncl("idx.0")
  declareIndexVar c, indexVar

  genStringCall(c, "writeNifParLe", "addr")
  genStringCall(c, "writeNifParLe", "aconstr")
  genStringCall(c, "writeNifRaw", toString(uarrayType, false))

  copyIntoKind c.dest, WhileS, c.info:
    # while idx < len(param):
    copyIntoKind c.dest, LtX, c.info:
      addIntType c
      copyIntoSymUse c.dest, indexVar, c.info
      copyIntoKind c.dest, CallS, c.info:
        c.dest.addIdent "len", c.info
        c.dest.add param
    copyIntoKind c.dest, StmtsS, c.info:
      # entryPoint(baseType, param[idx]) — emitted as `(call [] param idx)`
      # so the sub-compile resolves it against the user-defined `[]`
      # overload. Using `(arrat …)` would only work for raw arrays, not
      # for objects (seq, string, …) that route subscript through `[]`.
      var elemAccess = createTokenBuf(8)
      copyIntoKind elemAccess, CallS, c.info:
        elemAccess.addIdent "[]", c.info
        copyTree elemAccess, param
        copyIntoSymUse elemAccess, indexVar, c.info
      freeze elemAccess
      entryPoint(c, baseType, readonlyCursorAt(elemAccess, 0))
      incIndexVar c, indexVar

  genParRiCall c # close aconstr
  genParRiCall c # close addr

proc unravelSet(c: var SynthesizeSerializerCtx; orig: TypeCursor; param: TokenBuf) =
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
          copyIntoKind c.dest, InsetX, c.info:
            c.dest.addSubtree orig
            c.dest.add param # param is the set, so it comes first
            # the element is our indexVar
            c.dest.addSymUse indexVar, c.info
          copyIntoKind c.dest, StmtsS, c.info:
           unravel c, baseType, indexVarAsBuf

      incIndexVar c, indexVar
  genParRiCall c

proc unravelEnum(c: var SynthesizeSerializerCtx; orig: TypeCursor; param: TokenBuf) =
  c.dest.addParLe CaseS, c.info
  c.dest.add param
  var enumDecl = orig
  enumDecl.into:  # (enum baseType field1 field2 …)
    skip enumDecl, SkipType  # base type
    while enumDecl.hasMore:
      let enumDeclInfo = enumDecl.info
      c.dest.copyIntoKind OfU, enumDeclInfo:
        c.dest.copyIntoKind RangesU, enumDeclInfo:
          let enumField = takeLocal(enumDecl, SkipFinalParRi)
          let esym = enumField.name.symId
          c.dest.addSymUse esym, enumDeclInfo
        c.dest.copyIntoKind StmtsS, enumDeclInfo:
          genStringCall(c, "writeNifSymbol", pool.syms[esym])
  c.dest.addParRi() # case

proc primitiveCall(c: var SynthesizeSerializerCtx; name: string; arg: Cursor) =
  c.dest.copyIntoKind CallS, c.info:
    c.dest.addSymUse pool.syms.getOrIncl(name & ".0." & writeNifModuleSuffix), c.info
    c.dest.addSubtree arg

proc entryPoint(c: var SynthesizeSerializerCtx; orig: TypeCursor; arg: Cursor) =
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

proc unravel(c: var SynthesizeSerializerCtx; orig: TypeCursor; param: TokenBuf) =
  if isSomeStringType(orig):
    entryPoint(c, orig, readonlyCursorAt(param, 0))
    return

  var typ = toTypeImpl orig
  # `(invok Base arg1 arg2 …)` — generic instantiation. Resolve via sem's
  # own `instantiateType` callback, which substitutes typevars and
  # canonicalises the result. Routing through sem keeps the substitution
  # rule in one place (rather than re-implementing it here).
  var instBuf: TokenBuf
  if typ.typeKind == InvokeT and c.semCtx != nil and
      c.semCtx.semInstantiateType != nil:
    # `(invok Base arg1 arg2 …)` → ask sem to resolve it. `semLocalType`
    # (inside `instantiateType`) routes through the eager-instantiation
    # path in `semtypes.semInvoke`, producing a fresh Symbol that points
    # at the materialised object/tuple body. We then `toTypeImpl` to
    # follow that Symbol to the body for the walk below.
    let emptyBindings = initTable[SymId, Cursor]()
    let inst = c.semCtx.semInstantiateType(c.semCtx[], typ, emptyBindings)
    instBuf = createTokenBuf(8)
    instBuf.addSubtree inst
    typ = toTypeImpl(beginRead(instBuf))
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
    entryPoint(c, typ, readonlyCursorAt(param, 0))
  of EnumT, OnumT, AnumT:
    unravelEnum c, typ, param
  of SetT:
    unravelSet c, typ, param
  of NoType, ErrT, AtT, AndT, OrT, NotT, ProcT, FuncT, IteratorT, ConverterT, MethodT, MacroT, TemplateT,
     ProctypeT,  VoidT, PtrT, VarargsT, StaticT,
     RefT, MutT, OutT, LentT, SinkT, NiltT, ConceptT, ItertypeT, UarrayT, AutoT,
     SymkindT, TypekindT, TypedescT, UntypedT, TypedT, CstringT, PointerT, OrdinalT:
    c.errorMsg = "unsupported type for compile-time evaluation: " & asNimCode(orig)

proc genProcDecl(c: var SynthesizeSerializerCtx; sym: SymId; typ: TypeCursor) =
  let paramA = pool.syms.getOrIncl(ParamSymName)
  var paramTreeA = createTokenBuf(4)
  copyIntoSymUse paramTreeA, paramA, c.info
  freeze paramTreeA

  let procStart = c.dest.len
  var headerBuf = move c.dest
  genProcHeader(c, headerBuf, sym, typ)
  c.dest = move headerBuf

  copyIntoKind(c.dest, StmtsS, c.info):
    let beforeUnravel = c.dest.len
    unravel(c, typ, paramTreeA)
    if c.dest.len == beforeUnravel:
      # `unravel`'s unsupported-type case sets `c.errorMsg` instead of
      # emitting anything; `executeExpr` propagates that to the caller
      # as a proper error message. Anything *else* getting us here is a
      # real bug — assert only when `errorMsg` is still empty so the
      # user-facing error path stays unblocked.
      assert c.errorMsg.len > 0, "empty serializer created"

  c.dest.addParRi() # close ProcS declaration
  # tell vtables.nim we need dynamic binding here:
  if c.routineKind == MethodY:
    setTag(c.dest[procStart], TagId(MethodS)) # keeps the sealed jump
    c.dest[procStart] = withLineInfo(c.dest[procStart], c.info)

proc genMissingProcs*(c: var SynthesizeSerializerCtx) =
  # remember that genProcDecl does mutate c.requests so be robust against that:
  while c.requests.len > 0:
    let reqs = move(c.requests)
    for i in 0 ..< reqs.len:
      c.routineKind = ProcY
      genProcDecl(c, reqs[i].sym, reqs[i].typ)

proc collectSymDefsAux(n: var Cursor; defs: var HashSet[SymId]) =
  if n.kind == ParLe:
    n.loopInto:
      collectSymDefsAux(n, defs)
  else:
    if n.kind == SymbolDef: defs.incl n.symId
    inc n

proc collectSymDefs(n: Cursor; defs: var HashSet[SymId]) =
  ## Walks `n` and records every `SymbolDef` token. Used by
  ## `collectUsedSymsFromExpr` to avoid pulling in top-level decls for
  ## symbols that are actually defined inline in the expression itself
  ## (local `var`s, nested procs, etc.).
  var n = n
  collectSymDefsAux(n, defs)

proc collectUsedSymsFromExpr(c: var SynthesizeSerializerCtx; s: var SemContext; expr: Cursor) =
  ## Like `collectUsedSyms` but seeded from the symbols referenced *in the
  ## expression itself* rather than a routine. Used by `executeExpr` to set
  ## up the sub-compile when the const initializer is e.g. a `block:` rather
  ## than a call. Symbols defined inline by the expression (local vars,
  ## nested procs) are skipped — they would otherwise be re-emitted as
  ## top-level decls and clash with their inline definitions.
  var stack = newSeq[SymId]()
  var handledSyms = initHashSet[SymId]()
  var inlineDefs = initHashSet[SymId]()
  collectSymDefs(expr, inlineDefs)
  var e = expr
  collectSyms(e, stack)
  c.usedModules.incl(s.g.config.nifcachePath / SystemModuleSuffix)
  while stack.len > 0:
    let sym = stack.pop()
    if sym in inlineDefs: continue
    if not handledSyms.containsOrIncl(sym):
      let symStr = pool.syms[sym]
      if isInstantiation(symStr):
        # Instantiated generic procs/types don't cross sub-compile
        # boundaries — they're stored header-only here. After
        # `rewriteSymsToIdents` the call becomes an ident lookup; the
        # sub-compile finds the generic original in its imported scopes
        # and re-instantiates on its own. Inlining the header-only stub
        # would miscompile (e.g. an instantiated `@` with empty body
        # returns garbage and the loop iterates billions of times).
        continue
      let owner = extractModule(symStr)
      if owner == c.thisModuleSuffix:
        let res = tryLoadSym(sym)
        if res.status == LacksNothing:
          let before = c.dest.len
          c.dest.addSubtree res.decl
          var d = cursorAt(c.dest, before)
          collectSyms(d, stack)
          endRead(c.dest)
      elif owner.len > 0:
        c.usedModules.incl(s.g.config.nifcachePath / owner)

proc executeExpr*(s: var SemContext; expr: Cursor; expectedType: TypeCursor;
                  dest: var TokenBuf; info: PackedLineInfo): string {.nimcall.} =
  ## Sub-compile and run an arbitrary expression (typically a `block:` whose
  ## body is too complex for `expreval.eval` — local `var`s, nested procs,
  ## `for` loops, etc.). The result is serialised to NIF in `dest` using the
  ## same `entryPoint` machinery that `executeCall` uses.
  let prepResult = semos.prepareEval(s)
  if prepResult.len > 0: return prepResult

  var c = SynthesizeSerializerCtx(dest: createTokenBuf(150), info: info,
    routineKind: ProcY, bits: s.g.config.bits, errorMsg: "",
    thisModuleSuffix: s.thisModuleSuffix,
    newModuleSuffix: s.thisModuleSuffix.substr(0, 2) &
      computeChecksum(mangle(expr, Frontend, s.g.config.bits)),
    semCtx: addr s)

  c.dest.addParLe StmtsS, info

  c.dest.copyIntoKind ImportS, info:
    c.dest.copyIntoKind InfixX, info:
      c.dest.addIdent "/", info
      c.dest.addIdent "std", info
      c.dest.addIdent "writenif", info

  if s.importSnippets.len > 0:
    var cur = beginRead(s.importSnippets)
    for i in 0 ..< s.importSnippets.len:
      c.dest.add cur.load
      inc cur
    endRead(s.importSnippets)

  c.dest.copyIntoKind CallS, info:
    c.dest.addSymUse pool.syms.getOrIncl("setup.0." & writeNifModuleSuffix), info
    c.dest.addStrLit toAbsolutePath(s.g.config.nifcachePath / c.newModuleSuffix & ".out.nif"), info

  var retTypeBuf = createTokenBuf(4)
  if cursorIsNil(expectedType):
    retTypeBuf.addSubtree s.types.autoType
  else:
    retTypeBuf.addSubtree expectedType
  var retType = cursorAt(retTypeBuf, 0)

  c.dest.copyIntoKind StmtsS, info:
    collectUsedSymsFromExpr c, s, expr
    if isVoidType(retType):
      c.dest.addSubtree expr
    else:
      # Const-eval snippets may call `.raises` stdlib (e.g. `readFile`). A
      # catch-all `except` arm gives the body `CanRaise` in NJVL/derefs.
      c.dest.copyIntoKind TryS, info:
        c.dest.copyIntoKind StmtsS, info:
          entryPoint(c, retType, expr)
        c.dest.copyIntoKind ExceptU, info:
          c.dest.addDotToken()
          c.dest.copyIntoKind StmtsS, info:
            discard

  c.dest.copyIntoKind CallS, info:
    c.dest.addSymUse pool.syms.getOrIncl("teardown.0." & writeNifModuleSuffix), info

  genMissingProcs c
  c.dest.addParRi() # StmtsS

  rewriteSymsToIdents c

  if c.errorMsg.len > 0:
    result = ensureMove c.errorMsg
  else:
    let sourceDir = absoluteParentDir(getFile(info))
    result = runEval(s, dest, c.newModuleSuffix, c.dest, c.usedModules, sourceDir)

