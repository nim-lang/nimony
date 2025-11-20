#
#
#           Hexer Compiler
#        (c) Copyright 2024 Andreas Rumpf
#
#    See the file "license.txt", included in this
#    distribution, for details about the copyright.
#

import std / [hashes, os, tables, sets, assertions]

include nifprelude
import symparser
import ".." / models / tags
import ".." / nimony / [nimony_model, programs, typenav, expreval, xints, decls, builtintypes, sizeof,
  typeprops, langmodes, typekeys, sigmatch]
import hexer_context, pipeline, dce1, lifter
import  ".." / lib / [stringtrees, treemangler]


proc setOwner(c: var EContext; newOwner: SymId): SymId =
  result = c.currentOwner
  c.currentOwner = newOwner

proc demand(c: var EContext; s: SymId) =
  if not c.declared.contains(s):
    #if pool.syms[s] == "=wasmoved_SX50ath0pat4k2dls.0.tem6twvye1":
    #  writeStackTrace()
    #  echo "YES, DEMANDED! ", int(s), " ", c.declared.len
    #  quit "wtf"
    c.requires.add s

proc offer(c: var EContext; s: SymId) =
  c.declared.incl s


proc skipExportMarker(c: var EContext; n: var Cursor) =
  if n.kind == DotToken:
    inc n
  elif n.kind == Ident and pool.strings[n.litId] == "x":
    inc n
  elif n.kind == ParLe:
    # can now also be `(tag)` or `(tag <bits>)`:
    skip n
  else:
    error c, "expected '.' or 'x' for an export marker: ", n

proc expectSymdef(c: var EContext; n: var Cursor) =
  if n.kind != SymbolDef:
    error c, "expected symbol definition, but got: ", n

proc getSymDef(c: var EContext; n: var Cursor): (SymId, PackedLineInfo) =
  expectSymdef(c, n)
  result = (n.symId, n.info)
  inc n

proc expectSym(c: var EContext; n: var Cursor) =
  if n.kind != Symbol:
    error c, "expected symbol, but got: ", n

proc getSym(c: var EContext; n: var Cursor): (SymId, PackedLineInfo) =
  expectSym(c, n)
  result = (n.symId, n.info)
  inc n

proc expectStrLit(c: var EContext; n: var Cursor) =
  if n.kind != StringLit:
    error c, "expected string literal, but got: ", n

proc expectIntLit(c: var EContext; n: var Cursor) =
  if n.kind != IntLit:
    error c, "expected int literal, but got: ", n



proc add(c: var EContext; tag: string; info: PackedLineInfo) =
  c.dest.add tagToken(tag, info)

type
  TraverseMode = enum
    TraverseAll, TraverseInner, TraverseSig, TraverseTopLevel

proc trExpr(c: var EContext; n: var Cursor)
proc trStmt(c: var EContext; n: var Cursor; mode = TraverseInner)
proc trLocal(c: var EContext; n: var Cursor; tag: SymKind; mode: TraverseMode)

type
  TypeFlag = enum
    IsTypeBody
    IsPointerOf
    IsNodecl
    IsInheritable
    IsUnion
    IsImportExternal

proc trType(c: var EContext; n: var Cursor; flags: set[TypeFlag] = {})

type
  CollectedPragmas = object
    externName: string
    flags: set[PragmaKind]
    align, bits: IntId
    header: StrId
    dynlib: StrId
    callConv: CallConv

proc parsePragmas(c: var EContext; n: var Cursor): CollectedPragmas

proc toExtern(c: var EContext; s: SymId; externName: string; isInline=false): string {.inline.} =
  let m = extractModule(pool.syms[s])
  result = toExtern(externName, if m.len == 0 or isInline: c.main else: m)

proc trField(c: var EContext; n: var Cursor; flags: set[TypeFlag] = {}) =
  c.dest.add n # fld
  inc n

  expectSymdef(c, n)
  let (s, sinfo) = getSymDef(c, n)
  c.dest.add symdefToken(s, sinfo)
  c.offer s

  skipExportMarker c, n

  let prag = parsePragmas(c, n)

  c.dest.addDotToken() # adds pragmas

  if IsImportExternal in flags and prag.externName.len == 0:
    var baseName = pool.syms[s]
    extractBasename baseName
    prag.externName = baseName

  if prag.externName.len > 0:
    c.registerMangle(s, c.toExtern(s, prag.externName))

  trType c, n, flags

  skip n # skips value
  takeParRi c, n

proc ithTupleField(c: var EContext; counter: int, typ: Cursor): SymId {.inline.} =
  #var typ = typ
  pool.syms.getOrIncl("fld." & $counter)
  # & "." & takeMangle(typ, Backend, c.bits))

proc genTupleField(c: var EContext; typ: var Cursor; counter: int) =
  c.dest.add tagToken("fld", typ.info)
  let name = ithTupleField(c, counter, typ)
  c.dest.add symdefToken(name, typ.info)
  c.offer name
  c.dest.addDotToken() # pragmas
  c.trType(typ, {})
  c.dest.addParRi() # "fld"

proc trEnumField(c: var EContext; n: var Cursor; flags: set[TypeFlag] = {}) =
  c.dest.add n # efld
  inc n

  expectSymdef(c, n)
  let (s, sinfo) = getSymDef(c, n)
  c.dest.add symdefToken(s, sinfo)
  c.offer s

  skipExportMarker c, n

  skip n # pragmas: must be empty

  skip n # type: must be the enum itself

  inc n # skips TupleConstr
  trExpr c, n
  skip n
  skipParRi c, n

  takeParRi c, n

proc genStringType(c: var EContext; info: PackedLineInfo) {.used.} =
  # now unused
  let s = pool.syms.getOrIncl(StringName)
  c.dest.add tagToken("type", info)
  c.dest.add symdefToken(s, info)
  c.offer s

  c.dest.addDotToken()
  c.dest.add tagToken("object", info)
  c.dest.addDotToken()

  c.dest.add tagToken("fld", info)
  let strField = pool.syms.getOrIncl(StringAField)
  c.dest.add symdefToken(strField, info)
  c.offer strField
  c.dest.addDotToken()
  c.dest.add tagToken("ptr", info)
  c.dest.add tagToken("c", info)
  c.dest.addIntLit(8, info)
  c.dest.addParRi() # "c"
  c.dest.addParRi() # "ptr"
  c.dest.addParRi() # "fld"

  c.dest.add tagToken("fld", info)
  let lenField = pool.syms.getOrIncl(StringIField)
  c.dest.add symdefToken(lenField, info)
  c.offer lenField
  c.dest.addDotToken()
  c.dest.add tagToken("i", info)
  c.dest.addIntLit(-1, info)
  c.dest.addParRi() # "i"
  c.dest.addParRi() # "fld"

  c.dest.addParRi() # "object"
  c.dest.addParRi() # "type"

proc useStringType(c: var EContext; info: PackedLineInfo) =
  let s = pool.syms.getOrIncl(StringName)
  c.dest.add symToken(s, info)
  c.demand s

proc trTupleBody(c: var EContext; n: var Cursor) =
  let info = n.info
  inc n
  c.dest.add tagToken("object", info)
  c.dest.addDotToken()
  var counter = 0
  while n.kind != ParRi:
    if n.substructureKind == KvU:
      inc n # skip tag
      skip n # skip name
      genTupleField(c, n, counter)
      skipParRi c, n
    else:
      genTupleField(c, n, counter)
    inc counter
  takeParRi c, n

proc trArrayBody(c: var EContext; n: var Cursor) =
  c.dest.add n
  inc n
  trType c, n
  if n.typeKind == RangetypeT:
    inc n
    skip n
    expectIntLit c, n
    let first = pool.integers[n.intId]
    inc n
    expectIntLit c, n
    let last = pool.integers[n.intId]
    inc n
    skipParRi c, n
    c.dest.addIntLit(last - first + 1, n.info)
  else:
    # should not be possible, but assume length anyway
    trExpr c, n
  takeParRi c, n

type
  GenPragmas = object
    opened: bool

proc openGenPragmas(): GenPragmas = GenPragmas(opened: false)

proc maybeOpen(c: var EContext; g: var GenPragmas; info: PackedLineInfo) {.inline.} =
  if not g.opened:
    g.opened = true
    c.dest.add tagToken("pragmas", info)

proc addKey(c: var EContext; g: var GenPragmas; key: string; info: PackedLineInfo) =
  maybeOpen c, g, info
  c.dest.add tagToken(key, info)
  c.dest.addParRi()

proc addKeyVal(c: var EContext; g: var GenPragmas; key: string; val: PackedToken; info: PackedLineInfo) =
  maybeOpen c, g, info
  c.dest.add tagToken(key, info)
  c.dest.add val
  c.dest.addParRi()

proc closeGenPragmas(c: var EContext; g: GenPragmas) =
  if g.opened:
    c.dest.addParRi()
  else:
    c.dest.addDotToken()

proc trParams(c: var EContext; n: var Cursor)

proc trProcTypeBody(c: var EContext; n: var Cursor) =
  c.dest.add tagToken("proctype", n.info)
  # This is really stupid...
  c.dest.addDotToken() # name
  inc n # proc
  # name, export marker, pattern, type vars:
  for i in 0..<ParamsPos: skip n
  trParams c, n

  let pinfo = n.info
  let prag = parsePragmas(c, n)
  var genPragmas = openGenPragmas()
  if prag.callConv != NoCallConv:
    let name = $prag.callConv
    c.addKey genPragmas, name, pinfo
  closeGenPragmas c, genPragmas

  # ignore, effects and body:
  if n.kind != ParRi:
    skip n
    if n.kind != ParRi:
      skip n
  takeParRi c, n

proc trRefBody(c: var EContext; n: var Cursor; key: string) =
  # We translate `ref T` to:
  # ptr OuterT;
  # OuterT = object
  #  r: int
  #  d: T
  # This means `deref x` becomes `x->d` and `x.field` becomes `x->d.field`
  # `cast` must also be adjusted by the offset of `d` within `OuterT` but this seems
  # to be optional.

  let info = n.info
  inc n
  c.dest.add tagToken("object", info)
  c.dest.addDotToken()

  c.dest.add tagToken("fld", info)
  let rcField = pool.syms.getOrIncl(RcField)
  c.dest.add symdefToken(rcField, info)
  c.offer rcField
  c.dest.addDotToken() # pragmas
  c.dest.add tagToken("i", info)
  c.dest.addIntLit(-1, info)
  c.dest.addParRi() # "i"
  c.dest.addParRi() # "fld"

  let dataField = pool.syms.getOrIncl(DataField)
  c.dest.add tagToken("fld", info)
  c.dest.add symdefToken(dataField, info)
  c.offer dataField
  c.dest.addDotToken() # pragmas
  c.trType(n, {})
  c.dest.addParRi() # "fld"

  c.dest.addParRi() # "object"

proc takeMangleProctype(c: var EContext; n: var Cursor): string =
  inc n
  # name, export marker, pattern, type vars:
  for i in 0..<ParamsPos: skip n

  var b = createMangler(60)
  if n.kind != DotToken:
    inc n # params tag
    while n.kind != ParRi:
      let pa = takeLocal(n, SkipFinalParRi)
      assert pa.kind == ParamY
      mangle b, pa.typ, Backend
  inc n # DotToken or ParRi
  # also add return type:
  mangle b, n, Backend
  skip n
  # handle pragmas:
  let props = extractProcProps(n)
  b.addKeyw $props.cc
  b.addKeyw $props.usesRaises
  b.addKeyw $props.usesClosure
  result = b.extract()
  if n.kind != ParRi:
    skip n # effects
    if n.kind != ParRi:
      skip n # body
  skipParRi c, n

proc trAsNamedType(c: var EContext; n: var Cursor) =
  let info = n.info
  var body = n
  let k = body.typeKind
  let key: string
  if k in RoutineTypes:
    key = takeMangleProctype(c, n)
  else:
    key = takeMangle(n, Backend, c.bits)

  var val = c.newTypes.getOrDefault(key)
  if val == SymId(0):
    val = pool.syms.getOrIncl(genericTypeName(key, c.main))
    c.newTypes[key] = val

    var buf = createTokenBuf(30)
    swap c.dest, buf

    c.dest.add tagToken("type", info)
    c.dest.add symdefToken(val, info)
    c.offer val

    c.dest.addDotToken()
    case k
    of TupleT:
      trTupleBody c, body
    of ArrayT:
      trArrayBody c, body
    of RoutineTypes:
      trProcTypeBody c, body
    of RefT:
      trRefBody c, body, key
    else:
      error c, "expected tuple or array, but got: ", body
    c.dest.addParRi() # "type"

    swap c.dest, buf
    c.pending.add buf
    # Convert NifC type decl to Nim-gear2 type decl by
    # inserting empty export marker and type vars
    buf.insert [dotToken(NoLineInfo), dotToken(NoLineInfo)], 1
    programs.publish val, buf
  # regardless of what we had to do, we still need to add the typename:
  if k == RefT:
    c.dest.add tagToken("ptr", info)
    c.dest.add symToken(val, info)
    c.dest.addParRi()
  else:
    c.dest.add symToken(val, info)

proc addRttiField(c: var EContext; info: PackedLineInfo) =
  c.dest.add tagToken("fld", info)
  c.dest.add symdefToken(pool.syms.getOrIncl(VTableField), info)
  c.dest.addEmpty() # pragmas
  c.dest.addParLe PtrT, info
  let rttiSym = pool.syms.getOrIncl("Rtti.0." & SystemModuleSuffix)
  c.dest.addSymUse rttiSym, info
  c.demand rttiSym
  c.dest.addParRi() # "ptr"
  c.dest.addParRi() # "fld"

proc trObjFields(c: var EContext; n: var Cursor; flags: set[TypeFlag]) =
  while n.kind != ParRi:
    case n.substructureKind
    of FldU:
      trField(c, n, flags)
    of CaseU:
      # XXX for now counts each case object field as separate
      inc n
      trField(c, n, flags)
      c.dest.add tagToken("union", n.info)
      while n.kind != ParRi:
        case n.substructureKind
        of OfU:
          inc n
          skip n
          assert n.stmtKind == StmtsS
          inc n
          if n.exprKind == NilX:
            skip n
          else:
            c.dest.add tagToken("object", n.info)
            c.dest.addDotToken  # base type
            trObjFields(c, n, flags)
            c.dest.addParRi # end of object
          skipParRi c, n
          skipParRi c, n
        of ElseU:
          inc n
          assert n.stmtKind == StmtsS
          inc n
          if n.exprKind == NilX:
            skip n
          else:
            c.dest.add tagToken("object", n.info)
            c.dest.addDotToken  # base type
            trObjFields(c, n, flags)
            c.dest.addParRi # end of object
          skipParRi c, n
          skipParRi c, n
        else:
          error "expected `of` or `else` inside `case`"
      c.dest.addParRi # end of union
      skipParRi c, n
    of NilU:
      skip n
    else:
      error "illformed AST inside object: ", n

proc trType(c: var EContext; n: var Cursor; flags: set[TypeFlag] = {}) =
  case n.kind
  of DotToken:
    c.dest.add n
    inc n
  of Symbol:
    let s = n.symId
    let ext = maybeMangle(c, s)
    if ext.len != 0:
      c.dest.addSymUse pool.syms.getOrIncl(ext), n.info
      inc n
      return
    let res = tryLoadSym(s)
    if res.status == LacksNothing:
      var typeDecl = asTypeDecl(res.decl)
      var body = typeDecl.body
      if body.typeKind == DistinctT: # skips DistinctT
        let prag = parsePragmas(c, typeDecl.pragmas)

        if prag.flags * {ImportcP, ImportcppP} == {}:
          inc body
          trType(c, body, flags)
          inc n
        else:
          c.demand s
          c.dest.add n
          inc n
      else:
        c.demand s
        c.dest.add n
        inc n
    else:
      error c, "could not find symbol: " & pool.syms[s]
  of ParLe:
    case n.typeKind
    of NoType, ErrT, OrT, AndT, NotT, TypedescT, UntypedT, TypedT, TypeKindT, OrdinalT:
      error c, "type expected but got: ", n
    of IntT, UintT, FloatT, CharT:
      let start = c.dest.len
      c.dest.add n
      inc n
      c.dest.add n
      inc n
      if n.kind != ParRi and n.pragmaKind in {ImportcP, ImportcppP}:
        c.dest.shrink start
        inc n
        c.dest.addSymUse pool.syms.getOrIncl(toExtern(pool.strings[n.litId], c.main)), n.info
        inc n
        skipParRi c, n
        if n.kind != ParRi and n.pragmaKind == HeaderP:
          inc n
          c.headers.incl n.litId
          inc n
          skipParRi c, n
        while n.kind != ParRi and n.pragmaKind in {ImportcP, ImportcppP, HeaderP}:
          skip n
        skipParRi c, n
      else:
        takeParRi c, n
    of BoolT, AutoT, SymKindT:
      c.loop n:
        c.dest.add n
        inc n
    of MutT, LentT:
      c.dest.add tagToken("ptr", n.info)
      inc n
      if isViewType(n):
        c.dest.shrink c.dest.len-1 # remove the "ptr" again
        trType c, n, {}
        skipParRi n
      else:
        c.loop n:
          trType c, n, {IsPointerOf}
    of PtrT, OutT:
      c.dest.add tagToken("ptr", n.info)
      inc n
      c.loop n:
        trType c, n, {IsPointerOf}
    of RefT:
      trAsNamedType c, n
    of ArrayT, RoutineTypes:
      if IsNodecl in flags:
        trArrayBody c, n
      else:
        trAsNamedType c, n
    of RangetypeT:
      # skip to base type
      inc n
      trType c, n
      skip n
      skip n
      skipParRi c, n
    of UarrayT:
      if IsPointerOf in flags:
        inc n
        trType c, n
        skipParRi c, n
      else:
        c.dest.add tagToken("flexarray", n.info)
        inc n
        trType c, n
        takeParRi c, n
    of PointerT:
      c.dest.add tagToken("ptr", n.info)
      c.dest.add tagToken("void", n.info)
      c.dest.addParRi()
      inc n
      takeParRi c, n
    of CstringT:
      c.dest.add tagToken("ptr", n.info)
      c.dest.add tagToken($CharT, n.info)
      c.dest.addIntLit(8, n.info)
      c.dest.addParRi()
      inc n
      takeParRi c, n
    of StaticT, SinkT, DistinctT:
      inc n
      trType c, n, flags
      skipParRi c, n
    of TupleT:
      trAsNamedType c, n
    of ObjectT:
      if IsUnion in flags:
        c.dest.add tagToken("union", n.info)
        inc n
        # Union types don't inherit any types.
        assert n.kind == DotToken
        inc n
      else:
        c.dest.add n
        inc n
        if n.kind == DotToken:
          c.dest.add n
          inc n
        else:
          # inherited symbol
          let isPtr = n.typeKind in {RefT, PtrT}
          if isPtr: inc n
          let (s, sinfo) = getSym(c, n)
          if isPtr: skipParRi c, n
          c.dest.add symToken(s, sinfo)
          c.demand s

        if IsInheritable in flags:
          addRttiField c, n.info

      if n.kind == DotToken:
        c.dest.add n
        inc n
      else:
        trObjFields(c, n, flags)

      takeParRi c, n
    of EnumT, HoleyEnumT:
      c.dest.add tagToken("enum", n.info)
      inc n
      trType c, n, flags # base type

      while n.substructureKind == EfldU:
        trEnumField(c, n, flags)

      takeParRi c, n
    of SetT:
      let info = n.info
      inc n
      let sizeOrig = bitsetSizeInBytes(n)
      var err = false
      let size = asSigned(sizeOrig, err)
      if err:
        error c, "invalid set element type: ", n
      else:
        case size
        of 1, 2, 4, 8:
          c.dest.add tagToken("u", info)
          c.dest.addIntLit(size * 8, info)
          c.dest.addParRi()
        else:
          var arrBuf = createTokenBuf(16)
          arrBuf.add tagToken("array", info)
          arrBuf.add tagToken("u", info)
          arrBuf.addIntLit(8, info)
          arrBuf.addParRi()
          arrBuf.addIntLit(size, info)
          arrBuf.addParRi()
          var arrCursor = cursorAt(arrBuf, 0)
          trAsNamedType(c, arrCursor)
      skip n
      skipParRi c, n
    of VoidT, VarargsT, NiltT, ConceptT, InvokeT, ItertypeT:
      error c, "unimplemented type: ", n
  else:
    error c, "type expected but got: ", n

proc maybeByConstRef(c: var EContext; n: var Cursor) =
  let param = asLocal(n)
  if param.typ.typeKind in {TypedescT, StaticT}:
    # do not produce any code for this as it's a compile-time parameter
    skip n
  elif passByConstRef(param.typ, param.pragmas, c.bits div 8) or typeprops.isInheritable(param.typ, false):
    var paramBuf = createTokenBuf()
    paramBuf.add tagToken("param", n.info)
    paramBuf.addSubtree param.name
    paramBuf.addSubtree param.exported
    paramBuf.addSubtree param.pragmas
    copyIntoKind paramBuf, PtrT, param.typ.info:
      paramBuf.addSubtree param.typ
    paramBuf.addDotToken()
    paramBuf.addParRi()
    var paramCursor = beginRead(paramBuf)
    trLocal(c, paramCursor, ParamY, TraverseSig)
    endRead(paramBuf)
    skip n
  else:
    trLocal(c, n, ParamY, TraverseSig)

proc trParams(c: var EContext; n: var Cursor) =
  if n.kind == DotToken:
    c.dest.add n
    inc n
  elif n.kind == ParLe and n.substructureKind == ParamsU:
    c.dest.add n
    inc n
    loop c, n:
      if n.symKind != ParamY:
        error c, "expected (param) but got: ", n
      maybeByConstRef(c, n)
  else:
    error c, "expected (params) but got: ", n
  # the result type
  var retType = n
  skip n
  # n is now at the pragmas position:
  if hasPragma(n, RaisesP):
    # use a tuple type:
    var ret = createTokenBuf(6)
    if isVoidType(retType):
      ret.add symToken(pool.syms.getOrIncl(ErrorCodeName), NoLineInfo)
    else:
      ret.addParLe TupleT, NoLineInfo
      ret.add symToken(pool.syms.getOrIncl(ErrorCodeName), NoLineInfo)
      ret.addSubtree retType
      ret.addParRi()
    retType = cursorAt(ret, 0)
    trType c, retType
  else:
    trType c, retType

proc parsePragmas(c: var EContext; n: var Cursor): CollectedPragmas =
  result = default(CollectedPragmas)
  if n.kind == DotToken:
    inc n
  elif n.kind == ParLe and pool.tags[n.tag] == $PragmasS:
    inc n
    while true:
      case n.kind
      of ParRi:
        inc n
        break
      of EofToken:
        error c, "expected ')', but EOF reached"
      else: discard
      if n.kind == ParLe:
        let pk = n.pragmaKind
        case pk
        of NoPragma:
          let cc = n.callConvKind
          if cc == NoCallConv:
            error c, "unknown pragma: ", n
          else:
            result.callConv = cc
          inc n
        of MagicP:
          inc n
          if n.kind notin {StringLit, Ident}:
            error c, "expected string literal or ident, but got: ", n
          result.flags.incl NodeclP
          inc n
        of ImportcP, ImportcppP:
          inc n
          expectStrLit c, n
          result.externName = pool.strings[n.litId]
          result.flags.incl pk
          inc n
        of ExportcP:
          inc n
          expectStrLit c, n
          result.externName = pool.strings[n.litId]
          inc n
        of NodeclP, SelectanyP, ThreadvarP, GlobalP, DiscardableP, NoReturnP,
           VarargsP, NoSideEffectP, NoDestroyP, ByCopyP, ByRefP,
           InlineP, NoinlineP, NoInitP, InjectP, GensymP, UntypedP, ViewP,
           InheritableP, PureP, ClosureP, PackedP, UnionP, IncompleteStructP:
          result.flags.incl pk
          inc n
        of BorrowP:
          result.flags.incl InlineP
          result.flags.incl pk
          inc n
        of HeaderP:
          inc n
          expectStrLit c, n
          result.header = n.litId
          result.flags.incl NodeclP
          inc n
        of DynlibP:
          inc n
          expectStrLit c, n
          result.dynlib = n.litId
          result.flags.incl DynlibP
          result.flags.incl NodeclP
          inc n
        of AlignP:
          inc n
          expectIntLit c, n
          result.align = n.intId
          inc n
        of BitsP:
          inc n
          expectIntLit c, n
          result.bits = n.intId
          inc n
        of RequiresP, EnsuresP, StringP, RaisesP, ErrorP, AssumeP, AssertP, ReportP,
           TagsP, DeprecatedP, SideEffectP, KeepOverflowFlagP, SemanticsP,
           BaseP, FinalP, PragmaP, CursorP, PassiveP, PluginP:
          skip n
          continue
        of BuildP, EmitP, PushP, PopP, PassLP, PassCP:
          bug "unreachable"
        skipParRi c, n
      else:
        error c, "unknown pragma: ", n
  else:
    error c, "(pragmas) or '.' expected, but got: ", n

proc trProcBody(c: var EContext; n: var Cursor) =
  if n.stmtKind == StmtsS:
    c.dest.add n
    inc n
    var prevStmt = NoStmt
    while n.kind != ParRi:
      prevStmt = n.stmtKind
      trStmt c, n, TraverseInner
    if prevStmt == RetS or c.resultSym == SymId(0):
      discard "ok, do not add another return"
    else:
      c.dest.add parLeToken(RetS, n.info)
      c.dest.add symToken(c.resultSym, n.info)
      c.dest.addParRi()
    takeParRi c, n
  else:
    trStmt c, n, TraverseInner

template moveToTopLevel(c: var EContext; mode: TraverseMode; body: typed) =
  if mode in {TraverseAll, TraverseInner}:
    var temp = createTokenBuf()
    swap c.dest, temp
    body
    swap c.dest, temp
    c.pending.add temp
  else:
    body

proc makeLocalDeclName(c: var EContext; s: SymId): string =
  # for proc and type decls
  result = pool.syms[s]
  extractBasename(result)
  result.add "."
  result.addInt c.localDeclCounters
  inc c.localDeclCounters
  result.add "."
  result.add c.main

proc makeLocalSymId(c: var EContext; s: SymId; registerParentScope: bool): SymId =
  let newName = makeLocalDeclName(c, s)
  result = pool.syms.getOrIncl(newName)
  if registerParentScope:
    registerMangleInParent(c, s, newName)
  else:
    registerMangle(c, s, newName)

proc buildProcType(c: var EContext; thisProc: Cursor): SymId =
  var thisProc = asRoutine(thisProc)
  var procTypeBuf = createTokenBuf()
  procTypeBuf.addParLe ProctypeT
  procTypeBuf.addDotToken() # name
  procTypeBuf.addDotToken() # export marker
  procTypeBuf.addDotToken() # pattern
  procTypeBuf.addDotToken() # type vars
  procTypeBuf.addSubtree thisProc.params
  procTypeBuf.addSubtree thisProc.retType
  procTypeBuf.addSubtree thisProc.pragmas
  procTypeBuf.addDotToken() # effects
  procTypeBuf.addDotToken() # body
  procTypeBuf.addParRi() # end of proctype

  var procTypeCursor = beginRead(procTypeBuf)
  var beforeProcPos = c.dest.len
  trAsNamedType c, procTypeCursor
  result = c.dest[c.dest.len - 1].symId
  c.dest.shrink beforeProcPos

proc trProc(c: var EContext; n: var Cursor; mode: TraverseMode) =
  let thisProc = n
  c.openMangleScope()
  var dst = createTokenBuf(50)
  swap c.dest, dst
  #let toPatch = c.dest.len
  let oldResultSym = c.resultSym
  c.resultSym = SymId(0)

  let vinfo = n.info
  c.add "proc", vinfo
  inc n
  let (s, sinfo) = getSymDef(c, n)

  let newSym: SymId

  if mode == TraverseInner:
    # namePos
    newSym = makeLocalSymId(c, s, true)
    c.dest.add symdefToken(newSym, sinfo)
  else:
    # namePos
    newSym = s
    c.dest.add symdefToken(s, sinfo)
  c.offer s

  var isGeneric = false
  if n.kind == ParLe:
    isGeneric = true
  skipExportMarker c, n

  skip n # patterns

  if n.substructureKind == TypevarsU:
    isGeneric = true
    # count each typevar as used:
    inc n
    while n.kind != ParRi:
      assert n.symKind == TypevarY
      inc n
      let (typevar, _) = getSymDef(c, n)
      c.offer typevar
      skipToEnd n
    inc n
  else:
    skip n # generic parameters

  if isGeneric:
    # count each param as used:
    inc n
    while n.kind != ParRi:
      assert n.symKind == ParamY
      inc n
      let (param, _) = getSymDef(c, n)
      c.offer param
      skipToEnd n
    inc n
    skip n # skip return type
  else:
    trParams c, n

  let pinfo = n.info
  let prag = parsePragmas(c, n)

  let oldOwner = setOwner(c, s)

  var genPragmas = openGenPragmas()
  if prag.callConv != NoCallConv:
    let name = $prag.callConv
    c.addKey genPragmas, name, pinfo
  if InlineP in prag.flags:
    c.addKey genPragmas, "inline", pinfo

  if prag.externName.len > 0:
    c.registerMangleInParent(newSym, c.toExtern(s, prag.externName, InlineP in prag.flags))
    c.addKeyVal genPragmas, "was", symToken(s, pinfo), pinfo
  if SelectanyP in prag.flags:
    c.addKey genPragmas, "selectany", pinfo

  closeGenPragmas c, genPragmas

  skip n # miscPos

  # body:
  if isGeneric:
    skip n
  elif mode != TraverseSig or InlineP in prag.flags:
    trProcBody c, n
  else:
    c.dest.addDotToken()
    skip n
  takeParRi c, n
  swap dst, c.dest
  if NodeclP in prag.flags or isGeneric:
    discard "do not add to c.dest"
  elif prag.flags * {ImportcP, ImportcppP} != {} and c.inImpSection == 0:
    c.dest.add tagToken("imp", n.info)
    c.dest.add dst
    c.dest.addParRi()
  else:
    c.dest.add dst
  if prag.header != StrId(0):
    c.headers.incl prag.header

  if prag.dynlib != StrId(0):
    let typeSym = buildProcType(c, thisProc)

    c.dynlibs.mgetOrPut(prag.dynlib, @[]).add (pool.strings.getOrIncl(prag.externName), typeSym)

    var dynlibName = "Dl." & prag.externName & "." & c.main
    c.registerMangleInParent(newSym, dynlibName)

  discard setOwner(c, oldOwner)
  c.closeMangleScope()
  c.resultSym = oldResultSym

proc trTypeDecl(c: var EContext; n: var Cursor; mode: TraverseMode) =
  var dst = createTokenBuf(50)
  swap c.dest, dst
  #let toPatch = c.dest.len
  let decl = asTypeDecl(n)
  let isDistinct = decl.body.typeKind == DistinctT
  let vinfo = n.info
  c.add "type", vinfo
  inc n
  let (s, sinfo) = getSymDef(c, n)
  let oldOwner = setOwner(c, s)

  let newSym: SymId

  if mode == TraverseInner and not isDistinct:
    newSym = makeLocalSymId(c, s, false)
    c.dest.add symdefToken(newSym, sinfo)
  else:
    newSym = s
    c.dest.add symdefToken(s, sinfo)
  c.offer s

  var isGeneric = n.kind == ParLe
  skipExportMarker c, n
  if n.substructureKind == TypevarsU:
    isGeneric = true
    # count each typevar as used:
    inc n
    while n.kind != ParRi:
      assert n.symKind == TypevarY
      inc n
      let (typevar, _) = getSymDef(c, n)
      c.offer typevar
      skipToEnd n
    inc n
  else:
    skip n # generic parameters

  let prag = parsePragmas(c, n)

  if PackedP in prag.flags:
    c.dest.copyIntoKind(PragmasU, NoLineInfo):
      c.dest.copyIntoKind(PackedP, NoLineInfo): discard
  else:
    c.dest.addDotToken() # pragmas

  if prag.externName.len > 0:
    c.registerMangle(newSym, c.toExtern(s, prag.externName))
  if n.typeKind in TypeclassKinds:
    isGeneric = true
  if isGeneric:
    skip n
  else:
    var flags = {IsTypeBody}
    if NodeclP in prag.flags: flags.incl IsNodecl
    if InheritableP in prag.flags and PureP notin prag.flags:
      flags.incl IsInheritable
    if UnionP in prag.flags:
      flags.incl IsUnion
    if {ImportcP, ImportcppP} * prag.flags != {}:
      flags.incl IsImportExternal
    trType c, n, flags
  takeParRi c, n
  swap dst, c.dest
  if NodeclP in prag.flags or isGeneric:
    discard "do not add to c.dest"
  else:
    c.dest.add dst
  if prag.header != StrId(0):
    c.headers.incl prag.header
  discard setOwner(c, oldOwner)

proc genStringLit(c: var EContext; s: string; info: PackedLineInfo) =
  when false:
    # cannot use this logic because C is stupid crap.
    let existing = c.strLits.getOrDefault(s)
    if existing != SymId(0):
      c.dest.add symToken(existing, info)
    else:
      let strName = pool.syms.getOrIncl("str`." & $c.strLits.len)
      c.strLits[s] = strName
      c.pending.add tagToken("const", info)
      c.pending.add symdefToken(strName, info)
      c.offer strName

      c.pending.add tagToken("pragmas", info)
      c.pending.add tagToken("static", info)
      c.pending.addParRi()
      c.pending.addParRi()

      # type:
      c.pending.add symToken(pool.syms.getOrIncl(StringName), info)
      # value:
      c.pending.add tagToken("oconstr", info)
      c.pending.add symToken(pool.syms.getOrIncl(StringName), info)

      c.pending.add parLeToken(KvU, info)
      let strField = pool.syms.getOrIncl(StringAField)
      c.pending.add symToken(strField, info)
      c.pending.addStrLit(s)
      c.pending.addParRi() # "kv"

      c.pending.add parLeToken(KvU, info)
      let lenField = pool.syms.getOrIncl(StringIField)
      c.pending.add symToken(lenField, info)
      # length also contains the "isConst" flag:
      c.pending.addIntLit(s.len * 2, info)
      c.pending.addParRi() # "kv"

      c.pending.addParRi() # "oconstr"
      c.pending.addParRi() # "const"
      c.dest.add symToken(strName, info)
  else:
    c.dest.add tagToken("oconstr", info)
    useStringType c, info

    c.dest.add parLeToken(KvU, info)
    let strField = pool.syms.getOrIncl(StringAField)
    c.dest.add symToken(strField, info)
    c.dest.addStrLit(s)
    c.dest.addParRi() # "kv"

    c.dest.add parLeToken(KvU, info)
    let lenField = pool.syms.getOrIncl(StringIField)
    c.dest.add symToken(lenField, info)
    # length also contains the "isConst" flag:
    c.dest.addIntLit(s.len * 2, info)
    c.dest.addParRi() # "kv"

    c.dest.addParRi() # "oconstr"

proc genStringLit(c: var EContext; n: Cursor) =
  assert n.kind == StringLit
  let info = n.info
  let s {.cursor.} = pool.strings[n.litId]
  genStringLit(c, s, info)

proc trStmtsExpr(c: var EContext; n: var Cursor) =
  let head = n.load()
  inc n
  if isLastSon(n):
    trExpr c, n
    skipParRi c, n
  else:
    c.dest.add head
    while n.kind != ParRi:
      if not isLastSon(n):
        trStmt c, n
      else:
        trExpr c, n
    takeParRi c, n

proc trTupleConstr(c: var EContext; n: var Cursor) =
  c.dest.add tagToken("oconstr", n.info)
  inc n
  var tupleType = n
  c.trType(n, {})

  inc tupleType
  var counter = 0
  while n.kind != ParRi:
    c.dest.add tagToken("kv", n.info)
    let isKvU = tupleType.substructureKind == KvU
    if isKvU:
      inc tupleType # skip "kv"
      skip tupleType # skip key
    c.dest.add symToken(ithTupleField(c, counter, tupleType), n.info)
    skip tupleType
    if isKvU:
      skipParRi tupleType

    inc counter
    if n.substructureKind == KvU:
      inc n # skip "kv"
      skip n # skip key
      trExpr c, n
      skipParRi c, n
    else:
      trExpr c, n
    c.dest.addParRi() # "kv"
  takeParRi c, n

proc trConv(c: var EContext; n: var Cursor) =
  let info = n.info
  let beforeConv = c.dest.len
  c.dest.add tagToken("conv", info)
  inc n
  let destType = n
  trType(c, n)
  let srcType = getType(c.typeCache, n)
  if destType.typeKind == CstringT and isStringType(srcType):
    var isSuffix = false
    if n.exprKind == SufX:
      isSuffix = true
      inc n
    if n.kind == StringLit:
      # evaluate the conversion at compile time:
      c.dest.shrink beforeConv
      c.dest.addStrLit pool.strings[n.litId]
      inc n
      if isSuffix:
        inc n
        skipParRi c, n
      skipParRi c, n
    else:
      let strField = pool.syms.getOrIncl(StringAField)
      c.dest.add tagToken("dot", info)
      trExpr(c, n)
      c.dest.add symToken(strField, info)
      c.dest.addIntLit(0, info)
      c.dest.addParRi()
      takeParRi c, n
  else:
    trExpr(c, n)
    takeParRi c, n

proc isSimpleLiteral(nb: var Cursor): bool =
  case nb.kind
  of IntLit, UIntLit, FloatLit, CharLit, StringLit, DotToken:
    result = true
    inc nb
  else:
    case nb.exprKind
    of FalseX, TrueX, InfX, NegInfX, NanX, NilX:
      result = true
      skip nb
    of SufX:
      inc nb
      result = isSimpleLiteral(nb)
      skip nb # type suffix
      skipParRi nb
    of CastX, ConvX:
      result = true
      inc nb
      skip nb # type
      while nb.kind != ParRi:
        if not isSimpleLiteral(nb): return false
      skipParRi nb
    else:
      result = false

proc getCompilerProc(c: var EContext; name: string; isInline=false): string =
  c.demand pool.syms.getOrIncl(name & ".0." & SystemModuleSuffix)
  result = toExtern(name, if isInline: c.main else: SystemModuleSuffix)

proc trArrAt(c: var EContext; n: var Cursor) =
  c.dest.add parLeToken(AtX, n.info) # NIFC uses the `at` token for array indexing
  inc n
  trExpr(c, n)
  let beforeIndex = c.dest.len
  let info = n.info
  let isUnsigned = getType(c.typeCache, n).typeKind in {UIntT, CharT}
  trExpr(c, n)
  if n.kind != ParRi:
    var indexDest = createTokenBuf(c.dest.len - beforeIndex)
    for i in beforeIndex..<c.dest.len:
      indexDest.add c.dest[i]
    c.dest.shrink beforeIndex
    let indexB = n
    skip n
    if n.kind != ParRi:
      # we have `low(T)`:
      let indexA = n
      skip n
      if BoundCheck in c.activeChecks:
        let abProcName = getCompilerProc(c, if isUnsigned: "nimUcheckAB" else: "nimIcheckAB", true)
        c.dest.copyIntoUnchecked "call", info:
          c.dest.add symToken(pool.syms.getOrIncl(abProcName), info)
          c.dest.add indexDest
          c.dest.addSubtree indexA
          c.dest.addSubtree indexB
      else:
        let indexType = if isUnsigned: c.typeCache.builtins.uintType else: c.typeCache.builtins.intType
        # we need the substraction regardless:
        c.dest.addParLe SubX, info
        c.dest.addSubtree indexType
        c.dest.add indexDest
        c.dest.addSubtree indexA
        c.dest.addParRi()
    else:
      # we only have to care about the upper bound:
      if BoundCheck in c.activeChecks:
        let abProcName = getCompilerProc(c, if isUnsigned: "nimUcheckB" else: "nimIcheckB", true)
        c.dest.copyIntoUnchecked "call", info:
          c.dest.add symToken(pool.syms.getOrIncl(abProcName), info)
          c.dest.add indexDest
          c.dest.addSubtree indexB
      else:
        c.dest.add indexDest
  takeParRi c, n

proc trFieldname(c: var EContext; n: var Cursor) =
  if n.kind == Symbol:
    let ext = maybeMangle(c, n.symId)
    if ext.len != 0:
      c.dest.addSymUse pool.syms.getOrIncl(ext), n.info
    else:
      c.dest.add n
    inc n
  else:
    trExpr c, n

proc trExpr(c: var EContext; n: var Cursor) =
  case n.kind
  of EofToken, ParRi:
    error c, "BUG: unexpected ')' or EofToken"
  of ParLe:
    case n.exprKind
    of EqX, NeqX, LeX, LtX:
      c.dest.add n
      inc n
      let beforeType = c.dest.len
      trType(c, n)
      c.dest.shrink beforeType
      trExpr(c, n)
      trExpr(c, n)
      takeParRi c, n
    of CastX:
      c.dest.add n
      inc n
      trType(c, n)
      trExpr(c, n)
      takeParRi c, n
    of HconvX, ConvX:
      trConv c, n
    of DconvX:
      inc n
      let beforeType = c.dest.len
      trType(c, n)
      c.dest.shrink beforeType
      trExpr(c, n)
      skipParRi(c, n)
    of AconstrX:
      c.dest.add tagToken("aconstr", n.info)
      inc n
      trType(c, n)
      while n.kind != ParRi:
        trExpr(c, n)
      takeParRi c, n
    of OconstrX:
      c.dest.add tagToken("oconstr", n.info)
      inc n
      trType(c, n)
      while n.kind != ParRi:
        if n.substructureKind == KvU:
          c.dest.add n # KvU
          inc n
          takeTree c, n # key
          trExpr c, n # value
          if n.kind != ParRi:
            # optional inheritance
            takeTree c, n
          takeParRi c, n
        else:
          trExpr c, n
      takeParRi c, n
    of TupConstrX:
      trTupleConstr c, n
    of CmdX, CallStrLitX, InfixX, PrefixX, HcallX, CallX:
      c.dest.add tagToken("call", n.info)
      inc n
      while n.kind != ParRi:
        trExpr(c, n)
      takeParRi c, n
    of ExprX:
      trStmtsExpr c, n
    of ArrAtX:
      trArrAt c, n
    of TupatX:
      let fieldType = getType(c.typeCache, n)
      c.dest.add tagToken("dot", n.info)
      inc n # skip tag
      trExpr c, n # tuple
      expectIntLit c, n
      c.dest.add symToken(ithTupleField(c, int pool.integers[n.intId], fieldType), n.info)
      inc n # skip index
      c.dest.addIntLit(0, n.info) # inheritance
      takeParRi c, n
    of DotX:
      c.dest.add tagToken("dot", n.info)
      inc n # skip tag
      trExpr c, n # obj
      trFieldname c, n # field
      if n.kind != ParRi:
        trExpr c, n # inheritance depth
      takeParRi c, n
    of DdotX:
      c.dest.add tagToken("dot", n.info)
      c.dest.add tagToken("deref", n.info)
      inc n # skip tag
      trExpr c, n
      c.dest.addParRi()
      trFieldname c, n
      trExpr c, n
      takeParRi c, n
    of HaddrX, AddrX:
      c.dest.add tagToken("addr", n.info)
      inc n
      trExpr(c, n)
      takeParRi c, n
    of HderefX, DerefX:
      c.dest.add tagToken("deref", n.info)
      inc n
      trExpr(c, n)
      takeParRi c, n
    of SufX:
      var suf = n
      inc suf
      let arg = suf
      skip suf
      assert suf.kind == StringLit
      if arg.kind == StringLit:
        # no suffix for string literal in nifc
        inc n
        if pool.strings[suf.litId] == "C":
          # cstring literal, add string lit directly:
          c.dest.add n
          inc n
        else:
          trExpr c, n
        inc n # suf
        skipParRi c, n
      else:
        c.dest.add n
        inc n
        trExpr c, n
        c.dest.add n
        inc n
        takeParRi c, n
    of AshrX:
      c.dest.add tagToken("shr", n.info)
      inc n
      var bits = -1'i64
      if n.typeKind in {IntT, UIntT}:
        var bitsToken = n
        inc bitsToken
        bits = pool.integers[bitsToken.intId]
      else:
        #error c, "expected int/uint type for ashr, got: ", n
        discard
      trType(c, n)
      c.dest.copyIntoKind CastX, n.info:
        c.dest.add tagToken("i", n.info)
        c.dest.addIntLit(bits, n.info)
        c.dest.addParRi()
        trExpr c, n
      c.dest.copyIntoKind CastX, n.info:
        c.dest.add tagToken("u", n.info)
        c.dest.addIntLit(bits, n.info)
        c.dest.addParRi()
        trExpr c, n
      takeParRi c, n
    of ErrX, NewobjX, NewrefX, SetConstrX, PlusSetX, MinusSetX, MulSetX, XorSetX, EqSetX, LeSetX, LtSetX,
       InSetX, CardX, BracketX, CurlyX, TupX, CompilesX, DeclaredX, DefinedX, AstToStrX, HighX, LowX, TypeofX, UnpackX,
       FieldsX, FieldpairsX, EnumtostrX, IsmainmoduleX, DefaultobjX, DefaulttupX, DefaultdistinctX, DoX, CchoiceX, OchoiceX,
       EmoveX, DestroyX, DupX, CopyX, WasmovedX, SinkhX, TraceX, CurlyatX, PragmaxX, QuotedX, TabconstrX,
       InstanceofX, ProccallX, InternalTypeNameX, InternalFieldPairsX, FailedX, IsX, EnvpX, DelayX:
      error c, "BUG: not eliminated: ", n
      #skip n
    of AtX, PatX, ParX, NilX, InfX, NeginfX, NanX, FalseX, TrueX, AndX, OrX, NotX, NegX,
       AddX, SubX, MulX, DivX, ModX, ShrX, ShlX,
       BitandX, BitorX, BitxorX, BitnotX, BaseobjX, OvfX:
      c.dest.add n
      inc n
      while n.kind != ParRi:
        trExpr c, n
      takeParRi c, n
    of SizeofX, AlignofX, OffsetofX:
      c.dest.add n
      inc n
      trType c, n
      while n.kind != ParRi:
        trExpr c, n
      takeParRi c, n
    of XorX:
      c.dest.add tagToken("neq", n.info)
      inc n
      while n.kind != ParRi:
        trExpr c, n
      takeParRi c, n
    of NoExpr:
      trType c, n
  of SymbolDef:
    c.dest.add n
    c.offer n.symId
    inc n
  of Symbol:
    var inlineValue = getInitValue(c.typeCache, n.symId)
    var inlineValueCopy = inlineValue
    if not cursorIsNil(inlineValue) and inlineValue.kind != DotToken and isSimpleLiteral(inlineValueCopy):
      trExpr(c, inlineValue)
    else:
      let ext = maybeMangle(c, n.symId)
      if ext.len != 0:
        c.dest.addSymUse pool.syms.getOrIncl(ext), n.info
      else:
        c.dest.add n
      c.demand n.symId
    inc n
  of StringLit:
    genStringLit c, n
    inc n
  of UnknownToken, DotToken, Ident, CharLit, IntLit, UIntLit, FloatLit:
    c.dest.add n
    inc n

proc trLocal(c: var EContext; n: var Cursor; tag: SymKind; mode: TraverseMode) =
  var symKind = if tag == ResultY: VarY else: tag
  var localDecl = n
  let toPatch = c.dest.len
  let vinfo = n.info
  c.dest.addParLe symKind, vinfo
  inc n
  let (s, sinfo) = getSymDef(c, n)
  if tag == ResultY:
    c.resultSym = s
  skipExportMarker c, n
  let pinfo = n.info
  let prag = parsePragmas(c, n)

  c.dest.add symdefToken(s, sinfo)
  c.offer s

  var genPragmas = openGenPragmas()

  if prag.externName.len > 0:
    c.registerMangle(s, c.toExtern(s, prag.externName))
    c.addKeyVal genPragmas, "was", symToken(s, pinfo), pinfo

  if ThreadvarP in prag.flags:
    c.dest[toPatch] = tagToken("tvar", vinfo)
    symKind = TvarY
  elif GlobalP in prag.flags:
    c.dest[toPatch] = tagToken("gvar", vinfo)
    symKind = GvarY

  if prag.align != IntId(0):
    c.addKeyVal genPragmas, "align", intToken(prag.align, pinfo), pinfo
  if prag.bits != IntId(0):
    c.addKeyVal genPragmas, "bits", intToken(prag.bits, pinfo), pinfo
  closeGenPragmas c, genPragmas

  var nodecl = prag.flags.contains(NodeclP)
  c.typeCache.registerLocal(s, symKind, n)
  if tag == ParamY and typeKind(n) == VarargsT:
    skip n
    nodecl = true
  else:
    trType c, n

  if mode == TraverseSig:
    if localDecl.substructureKind == ParamU:
      # Parameter decls in NIFC have no dot token for the default value!
      discard
    else:
      # Imported variables don't need initial values.
      c.dest.addDotToken
    skip n
  else:
    trExpr c, n
  takeParRi c, n
  if nodecl:
    c.dest.shrink toPatch
  if prag.header != StrId(0):
    c.headers.incl prag.header

proc trWhile(c: var EContext; n: var Cursor) =
  let info = n.info
  c.nestedIn.add (WhileS, SymId(0))
  c.dest.add n
  inc n
  trExpr c, n
  trStmt c, n
  takeParRi c, n
  let lab = c.nestedIn[^1][1]
  if lab != SymId(0):
    c.dest.add tagToken("lab", info)
    c.dest.add symdefToken(lab, info)
    c.offer lab
    c.dest.addParRi()
  discard c.nestedIn.pop()

proc trBlock(c: var EContext; n: var Cursor) =
  let info = n.info
  inc n
  if n.kind == DotToken:
    c.nestedIn.add (BlockS, SymId(0))
    inc n
  else:
    let (s, _) = getSymDef(c, n)
    c.nestedIn.add (BlockS, s)
  c.dest.add tagToken("scope", info)
  trStmt c, n
  takeParRi c, n
  let lab = c.nestedIn[^1][1]
  if lab != SymId(0):
    c.dest.add tagToken("lab", info)
    c.dest.add symdefToken(lab, info)
    c.offer lab
    c.dest.addParRi()
  discard c.nestedIn.pop()

proc trBreak(c: var EContext; n: var Cursor) =
  let info = n.info
  inc n
  if n.kind == DotToken:
    inc n
    c.dest.add tagToken("break", info)
  else:
    expectSym c, n
    let lab = n.symId
    inc n
    c.dest.add tagToken("jmp", info)
    c.dest.add symToken(lab, info)
  takeParRi c, n

proc trIf(c: var EContext; n: var Cursor) =
  # (if cond (.. then ..) (.. else ..))
  c.dest.add n
  inc n
  while n.kind == ParLe and n.substructureKind == ElifU:
    c.dest.add n
    inc n # skips '(elif'
    trExpr c, n
    trStmt c, n
    takeParRi c, n
  if n.kind == ParLe and n.substructureKind == ElseU:
    c.dest.add n
    inc n
    trStmt c, n
    takeParRi c, n
  takeParRi c, n

include stringcases

proc trStringCase(c: var EContext; n: var Cursor): bool =
  var nb = n
  inc nb
  let selectorType = getType(c.typeCache, nb)
  if isSomeStringType(selectorType):
    transformStringCase(c, n)
    result = true
  else:
    result = false

proc trCase(c: var EContext; n: var Cursor) =
  if trStringCase(c, n):
    return
  c.dest.add n
  inc n
  trExpr c, n
  while n.kind != ParRi:
    case n.substructureKind
    of OfU:
      c.dest.add n
      inc n
      if n.kind == ParLe and n.substructureKind == RangesU:
        inc n
        c.add "ranges", n.info
        while n.kind != ParRi:
          if n.kind == ParLe and n.substructureKind == RangeU:
            inc n
            c.add "range", n.info
            while n.kind != ParRi:
              trExpr c, n
            takeParRi c, n
          else:
            trExpr c, n
        takeParRi c, n
      else:
        trExpr c, n
      trStmt c, n
      takeParRi c, n
    of ElseU:
      c.dest.add n
      inc n
      trStmt c, n
      takeParRi c, n
    else:
      error c, "expected (of) or (else) but got: ", n
  takeParRi c, n

proc trKeepovf(c: var EContext; n: var Cursor) =
  c.dest.add n
  inc n
  trExpr c, n # (add ...)
  trExpr c, n # destination
  takeParRi c, n

proc trRaise(c: var EContext; n: var Cursor) =
  let info = n.info
  inc n
  if c.exceptLabels.len == 0:
    # translate `raise` to `return`:
    c.dest.addParLe RetS, info
    trExpr c, n
  else:
    # translate `raise` to `goto`:
    skip n # raise expression handled in constparams.nim
    let lab = c.exceptLabels[^1]
    c.dest.add tagToken("jmp", info)
    c.dest.add symToken(lab, info)
  takeParRi c, n

proc trTry(c: var EContext; n: var Cursor) =
  # We only deal with the control flow here.
  let info = n.info
  inc n
  var nn = n
  skip nn # stmts
  let oldLen = c.exceptLabels.len
  var hasExcept = false
  if nn.substructureKind == ExceptU:
    let lab = pool.syms.getOrIncl("`lab." & $getTmpId(c))
    c.exceptLabels.add lab
    hasExcept = true
  trStmt c, n

  if hasExcept:
    c.dest.addParLe IfS, n.info

  while n.substructureKind == ExceptU:
    let lab = c.exceptLabels[oldLen]
    c.dest.copyIntoKind ElifU, n.info:
      c.dest.addParPair(FalseX, n.info)
      c.dest.copyIntoKind StmtsS, n.info:
        c.dest.add tagToken("lab", n.info)
        c.dest.add symdefToken(lab, n.info)
        c.dest.addParRi()
        inc n
        if n.stmtKind == LetS:
          trStmt c, n
        else:
          skip n # skip `T`
        trStmt c, n
        skipParRi n
  c.exceptLabels.shrink oldLen

  # Since we duplicated the finally statements before every `raise` statement we
  # know that when control flow reaches here, no error was raised. Hence we do not
  # need to add logic to re-raise an exception here.
  if n.substructureKind == FinU:
    if hasExcept:
      c.dest.addParLe ElseU, n.info
    inc n
    trStmt c, n
    skipParRi n
    if hasExcept:
      c.dest.addParRi()
  skipParRi n
  if hasExcept:
    c.dest.addParRi()

proc trStmt(c: var EContext; n: var Cursor; mode = TraverseInner) =
  case n.kind
  of DotToken:
    c.dest.add n
    inc n
  of ParLe:
    case n.stmtKind
    of NoStmt:
      if n.tagId == TagId(KeepovfTagId):
        trKeepovf c, n
      else:
        error c, "unknown statement: ", n
    of StmtsS:
      if mode == TraverseTopLevel:
        inc n
        while n.kind notin {EofToken, ParRi}:
          trStmt c, n, mode
        skipParRi c, n
      else:
        c.dest.add n
        inc n
        c.loop n:
          trStmt c, n, mode
    of ScopeS:
      c.openMangleScope()
      if mode == TraverseTopLevel:
        inc n
        while n.kind notin {EofToken, ParRi}:
          trStmt c, n, mode
        skipParRi c, n
      else:
        c.dest.add n
        inc n
        c.loop n:
          trStmt c, n, mode
      c.closeMangleScope()
    of VarS, LetS, CursorS:
      trLocal c, n, VarY, mode
    of ResultS:
      trLocal c, n, ResultY, mode
    of GvarS, GletS:
      trLocal c, n, GvarY, mode
    of TvarS, TletS:
      trLocal c, n, TvarY, mode
    of ConstS:
      trLocal c, n, ConstY, mode
    of CallKindsS:
      c.dest.add tagToken("call", n.info)
      inc n
      c.loop n:
        trExpr c, n
    of EmitS, AsmS:
      c.dest.add n
      inc n
      c.loop n:
        if n.kind == StringLit:
          c.dest.add n
          inc n
        elif n.exprkind == SufX:
          inc n
          assert n.kind == StringLit
          c.dest.add n
          skipToEnd n
        else:
          trExpr c, n
    of AsgnS, RetS:
      c.dest.add n
      inc n
      c.loop n:
        trExpr c, n
    of DiscardS:
      let discardToken = n
      inc n
      if n.kind in {StringLit, DotToken}:
        # eliminates discard without side effects
        inc n
        skipParRi c, n
      else:
        c.dest.add discardToken
        trExpr c, n
        takeParRi c, n
    of BreakS: trBreak c, n
    of WhileS: trWhile c, n
    of BlockS: trBlock c, n
    of IfS: trIf c, n
    of CaseS: trCase c, n
    of YldS, ForS, InclS, ExclS, DeferS, UnpackDeclS:
      error c, "BUG: not eliminated: ", n
    of TryS:
      trTry c, n
    of RaiseS:
      trRaise c, n
    of FuncS, ProcS, ConverterS, MethodS:
      moveToTopLevel(c, mode):
        trProc c, n, mode
    of MacroS, TemplateS, IncludeS, ImportS, FromimportS, ImportExceptS, ExportS, CommentS, IteratorS,
       ImportasS, ExportexceptS, BindS, MixinS, UsingS, StaticstmtS:
      # pure compile-time construct, ignore:
      skip n
    of TypeS:
      moveToTopLevel(c, mode):
        trTypeDecl c, n, mode
    of ContinueS, WhenS:
      error c, "unreachable: ", n
    of PragmasS, AssumeS, AssertS:
      skip n
  else:
    assert n.kind != ParRi
    error c, "statement expected, but got: ", n

proc transformInlineRoutines(c: var EContext; n: var Cursor) =
  var swapped = createTokenBuf()
  swap c.dest, swapped

  var toTransform = createTokenBuf()
  toTransform.copyIntoKind StmtsS, n.info:
    takeTree(toTransform, n)
  var t = beginRead(toTransform)
  var dest = transform(c, t, c.main)
  var d = beginRead(dest)

  inc d # skips (stmts

  swap c.dest, swapped

  trStmt c, d, TraverseSig
  let oldInImpSection = c.inImpSection
  c.inImpSection = 0
  while d.kind != ParRi:
    trStmt c, d, TraverseAll
  c.inImpSection = oldInImpSection

proc importSymbol(c: var EContext; s: SymId) =
  let res = tryLoadSym(s)
  if res.status == LacksNothing:
    var n = res.decl
    let kind = n.symKind
    case kind
    of TypeY:
      trTypeDecl c, n, TraverseSig
    of EfldY:
      # import full enum type:
      let typ = asLocal(n).typ
      assert typ.kind == Symbol
      c.demand typ.symId
    else:
      let isR = isRoutine(kind)
      if isR or isLocal(kind):
        var pragmas = if isR:
                        asRoutine(n).pragmas
                      else:
                        asLocal(n).pragmas
        let prag = parsePragmas(c, pragmas)
        if isR:
          if {InlineP, DynlibP} * prag.flags != {}:
            # rewrite the inline routine, it belongs to the current module now that
            # we duplicated it!
            if prag.externName.len > 0:
              c.registerMangle(s, toExtern(prag.externName, c.main))
            else:
              let newName = makeLocalDeclName(c, s)
              c.registerMangle(s, newName)
            transformInlineRoutines(c, n)
            return

        if NodeclP in prag.flags:
          if prag.externName.len > 0:
            c.registerMangle(s, c.toExtern(s, prag.externName))
          if prag.header != StrId(0):
            c.headers.incl prag.header
          return

      # XXX This is a stupid hack to avoid producing (imp (imp ...))
      inc c.inImpSection
      c.dest.add tagToken("imp", n.info)
      if pool.syms[s] == "=wasmoved_SX50ath0pat4k2dls.0.tem6twvye1":
        writeStackTrace()
        echo "IMPORTING ", pool.syms[s]
      trStmt c, n, TraverseSig
      c.dest.addParRi()
      dec c.inImpSection
  else:
    error c, "could not find symbol: " & pool.syms[s]

proc writeOutput(c: var EContext, rootInfo: PackedLineInfo; destfileName: string): TokenBuf =
  # Prepass: patch symbols that need mangling in c.dest
  for i in 0 ..< c.dest.len:
    let tok = c.dest[i]
    if tok.kind in {Symbol, SymbolDef}:
      let mangledName = c.maybeMangle(tok.symId)
      if mangledName.len > 0:
        let mangledSym = pool.syms.getOrIncl(mangledName)
        c.dest[i].setSymId(mangledSym)

  # Build the final output with stmts wrapper and includes
  result = createTokenBuf()
  result.add tagToken("stmts", rootInfo)

  # Add include statements for headers
  for h in c.headers:
    result.add tagToken("incl", rootInfo)
    result.add strToken(h, rootInfo)
    result.addParRi()

  # Add all the generated content
  result.add c.dest

  # Close the stmts wrapper
  result.addParRi()

  # Write with vendor/dialect metadata
  var b = nifbuilder.open(destfileName)
  b.addHeader "hexer", "nifc"
  b.addRaw toString(result)
  b.close()

proc initDynlib(c: var EContext, rootInfo: PackedLineInfo) =
  # dynlib init:
  for key, vals in c.dynlibs:
    let dynlib = pool.strings[key]
    var tmp = pool.syms.getOrIncl "Dl." & dynlib & "." & $getTmpId(c) & "." & c.main

    # nimLoadLibrary
    c.dest.add tagToken("gvar", rootInfo)
    c.dest.add symdefToken(tmp, rootInfo)
    c.offer tmp
    c.dest.addDotToken()
    c.dest.add tagToken("ptr", rootInfo)
    c.dest.add tagToken("void", rootInfo)
    c.dest.addParRi()
    c.dest.addParRi()
    c.dest.add tagToken("call", rootInfo)
    c.dest.add symToken(pool.syms.getOrIncl(getCompilerProc(c, "nimLoadLibrary", false)), rootInfo)
    c.dest.addStrLit dynlib
    c.dest.addParRi()

    c.dest.addParRi()

    # nimGetProcAddr
    for (val, typeSym) in vals:
      let procName = pool.strings[val]
      let varName = pool.syms.getOrIncl "Dl." & pool.strings[val] & "." & c.main
      c.dest.add tagToken("gvar", rootInfo)
      c.dest.add symdefToken(varName, rootInfo)
      c.offer varName
      c.dest.addDotToken()
      c.dest.add symToken(typeSym, rootInfo)

      c.dest.add tagToken("cast", rootInfo)
      c.dest.add symToken(typeSym, rootInfo)
      c.dest.add tagToken("call", rootInfo)
      c.dest.add symToken(pool.syms.getOrIncl(getCompilerProc(c, "nimGetProcAddr", false)), rootInfo)
      c.dest.add symToken(tmp, rootInfo) # library
      c.dest.addStrLit procName # proc name
      c.dest.addParRi()
      c.dest.addParRi()

      c.dest.addParRi()

proc expand*(infile: string; bits: int; flags: set[CheckMode]) =
  let mp = splitModulePath(infile)
  var c = EContext(dir: (if mp.dir.len == 0: getCurrentDir() else: mp.dir), ext: mp.ext, main: mp.name,
    dest: createTokenBuf(),
    nestedIn: @[(StmtsS, SymId(0))],
    typeCache: createTypeCache(),
    pending: createTokenBuf(),
    bits: bits,
    localDeclCounters: 1000,
    activeChecks: flags,
    liftingCtx: createLiftingCtx(mp.name, bits)
    )
  c.openMangleScope()

  var c0 = setupProgram(infile, infile.changeModuleExt ".x.nif", true)
  var dest = transform(c, c0, mp.name)

  var n = beginRead(dest)
  let rootInfo = n.info

  var toplevels = createTokenBuf()
  swap c.dest, toplevels
  if stmtKind(n) == StmtsS:
    inc n
    #genStringType c, n.info
    while n.kind != ParRi:
      trStmt c, n, TraverseTopLevel
  else:
    error c, "expected (stmts) but got: ", n
  swap c.dest, toplevels


  # fix point expansion:
  while true:
    let batch = c.requires.move
    if batch.len == 0: break
    for imp in batch:
      if not c.declared.contains(imp):
        importSymbol(c, imp)

  initDynlib(c, rootInfo)

  if c.dynlibs.len > 0:
    let loadLibrary = pool.syms.getOrIncl("nimLoadLibrary.0." & SystemModuleSuffix)
    let getProcAddr = pool.syms.getOrIncl("nimGetProcAddr.0." & SystemModuleSuffix)
    if not c.declared.contains(loadLibrary):
      importSymbol(c, loadLibrary)
      importSymbol(c, getProcAddr)

  c.dest.add toplevels
  c.dest.add c.pending
  skipParRi c, n
  let destfileName = c.dir / c.main & ".x.nif"

  var outputBuf = writeOutput(c, rootInfo, destfileName)
  c.closeMangleScope()

  # Use the in-memory buffer to avoid re-reading the file we just wrote
  writeDceOutput outputBuf, c.dir / c.main & ".dce.nif"
