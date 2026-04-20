#
#
#           Hexer Compiler
#        (c) Copyright 2024 Andreas Rumpf
#
#    See the file "license.txt", included in this
#    distribution, for details about the copyright.
#

import std / [hashes, os, tables, assertions]

include nifprelude
import symparser
import ".." / models / tags
import ".." / nimony / [nimony_model, programs, typenav, expreval, xints, decls, builtintypes, sizeof,
  typeprops, langmodes, typekeys, nifconfig]
import hexer_context, pipeline, dce1, lifter
import  ".." / lib / [stringtrees]

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

proc add(dest: var TokenBuf; tag: string; info: PackedLineInfo) =
  dest.add tagToken(tag, info)

type
  GenPragmas = object
    opened: bool

proc openGenPragmas(): GenPragmas = GenPragmas(opened: false)

proc maybeOpen(dest: var TokenBuf; g: var GenPragmas; info: PackedLineInfo) {.inline.} =
  if not g.opened:
    g.opened = true
    dest.add tagToken("pragmas", info)

proc addKey(dest: var TokenBuf; g: var GenPragmas; key: string; info: PackedLineInfo) =
  maybeOpen dest, g, info
  dest.add tagToken(key, info)
  dest.addParRi()

proc addKeyVal(dest: var TokenBuf; g: var GenPragmas; key: string; val: PackedToken; info: PackedLineInfo) =
  maybeOpen dest, g, info
  dest.add tagToken(key, info)
  dest.add val
  dest.addParRi()

proc closeGenPragmas(dest: var TokenBuf; g: GenPragmas) =
  if g.opened:
    dest.addParRi()
  else:
    dest.addDotToken()

type
  TraverseMode = enum
    TraverseAll, TraverseInner, TraverseSig, TraverseTopLevel

proc trExpr(c: var EContext; dest: var TokenBuf; n: var Cursor)
proc trStmt(c: var EContext; dest: var TokenBuf; n: var Cursor; mode = TraverseInner)
proc trLocal(c: var EContext; dest: var TokenBuf; n: var Cursor; tag: SymKind; mode: TraverseMode)
proc getCompilerProc(c: var EContext; name: string; isInline=false): string

type
  TypeFlag = enum
    IsTypeBody
    IsPointerOf
    IsNodecl
    IsInheritable
    IsUnion
    IsImportExternal

proc trType(c: var EContext; dest: var TokenBuf; n: var Cursor; flags: set[TypeFlag] = {})

type
  CollectedPragmas = object
    extern: StrId
    flags: set[PragmaKind]
    align, bits: IntId
    header: StrId
    dynlib: StrId
    callConv: CallConv

proc parsePragmas(c: var EContext; dest: var TokenBuf; n: var Cursor): CollectedPragmas

proc externKind(p: CollectedPragmas): string =
  if ImportcP in p.flags:
    result = "importc"
  elif ImportcppP in p.flags:
    result = "importcpp"
  elif ExportcP in p.flags:
    result = "exportc"
  else:
    result = ""

proc externPragmas(c: var EContext; dest: var TokenBuf; genPragmas: var GenPragmas;
                   prag: CollectedPragmas; pinfo: PackedLineInfo) =
  let extKind = externKind(prag)
  if extKind.len != 0:
    if prag.extern != StrId(0):
      dest.addKeyVal genPragmas, extKind, strToken(prag.extern, pinfo), pinfo
    else:
      dest.addKey genPragmas, extKind, pinfo
  if NodeclP in prag.flags:
    dest.addKey genPragmas, "nodecl", pinfo
  if prag.header != StrId(0):
    dest.addKeyVal genPragmas, "header", strToken(prag.header, pinfo), pinfo

proc trField(c: var EContext; dest: var TokenBuf; n: var Cursor; flags: set[TypeFlag] = {}) =
  dest.add n # fld
  inc n

  expectSymdef(c, n)
  let (s, sinfo) = getSymDef(c, n)
  dest.add symdefToken(s, sinfo)

  skipExportMarker c, n

  let pinfo = n.info
  let prag = parsePragmas(c, dest, n)

  var genPragmas = openGenPragmas()
  externPragmas c, dest, genPragmas, prag, pinfo

  if prag.align != IntId(0):
    dest.addKeyVal genPragmas, "align", intToken(prag.align, pinfo), pinfo
  if prag.bits != IntId(0):
    dest.addKeyVal genPragmas, "bits", intToken(prag.bits, pinfo), pinfo
  closeGenPragmas dest, genPragmas

  trType c, dest, n, flags

  skip n # skips value
  takeParRi dest, n

proc ithTupleField(c: var EContext; counter: int, typ: Cursor): SymId {.inline.} =
  #var typ = typ
  pool.syms.getOrIncl("fld." & $counter)
  # & "." & takeMangle(typ, Backend, c.bits))

proc genTupleField(c: var EContext; dest: var TokenBuf; typ: var Cursor; counter: int) =
  dest.add tagToken("fld", typ.info)
  let name = ithTupleField(c, counter, typ)
  dest.add symdefToken(name, typ.info)
  dest.addDotToken() # pragmas
  c.trType(dest, typ, {})
  dest.addParRi() # "fld"

proc trEnumField(c: var EContext; dest: var TokenBuf; n: var Cursor; flags: set[TypeFlag] = {}) =
  dest.add n # efld
  inc n

  expectSymdef(c, n)
  let (s, sinfo) = getSymDef(c, n)
  dest.add symdefToken(s, sinfo)

  skipExportMarker c, n

  skip n # pragmas: must be empty

  skip n # type: must be the enum itself

  inc n # skips TupleConstr
  trExpr c, dest, n
  skip n
  skipParRi c, n

  takeParRi dest, n

proc genStringType(c: var EContext; dest: var TokenBuf; info: PackedLineInfo) {.used.} =
  # now unused
  let s = pool.syms.getOrIncl(StringName)
  dest.add tagToken("type", info)
  dest.add symdefToken(s, info)

  dest.addDotToken()
  dest.add tagToken("object", info)
  dest.addDotToken()

  when sso:
    dest.add tagToken("fld", info)
    let bytesField = pool.syms.getOrIncl(StringBytesField)
    dest.add symdefToken(bytesField, info)
    dest.addDotToken()
    dest.add tagToken("u", info)
    dest.addIntLit(-1, info)
    dest.addParRi() # "u"
    dest.addParRi() # "fld"

    dest.add tagToken("fld", info)
    let moreField = pool.syms.getOrIncl(StringMoreField)
    dest.add symdefToken(moreField, info)
    dest.addDotToken()
    dest.add tagToken("ptr", info)
    dest.add symToken(pool.syms.getOrIncl(LongStringName), info)
    dest.addParRi() # "ptr"
    dest.addParRi() # "fld"
  else:
    dest.add tagToken("fld", info)
    let strField = pool.syms.getOrIncl(StringAField)
    dest.add symdefToken(strField, info)
    dest.addDotToken()
    dest.add tagToken("ptr", info)
    dest.add tagToken("c", info)
    dest.addIntLit(8, info)
    dest.addParRi() # "c"
    dest.addParRi() # "ptr"
    dest.addParRi() # "fld"

    dest.add tagToken("fld", info)
    let lenField = pool.syms.getOrIncl(StringIField)
    dest.add symdefToken(lenField, info)
    dest.addDotToken()
    dest.add tagToken("i", info)
    dest.addIntLit(-1, info)
    dest.addParRi() # "i"
    dest.addParRi() # "fld"

  dest.addParRi() # "object"
  dest.addParRi() # "type"

proc useStringType(c: var EContext; dest: var TokenBuf; info: PackedLineInfo) =
  let s = pool.syms.getOrIncl(StringName)
  dest.add symToken(s, info)

proc trTupleBody(c: var EContext; dest: var TokenBuf; n: var Cursor) =
  let info = n.info
  inc n
  dest.add tagToken("object", info)
  dest.addDotToken()
  var counter = 0
  while n.kind != ParRi:
    if n.substructureKind == KvU:
      inc n # skip tag
      skip n # skip name
      genTupleField(c, dest, n, counter)
      skipParRi c, n
    else:
      genTupleField(c, dest, n, counter)
    inc counter
  takeParRi dest, n

proc trArrayBody(c: var EContext; dest: var TokenBuf; n: var Cursor) =
  dest.add n
  inc n
  trType c, dest, n
  if n.typeKind == RangetypeT:
    inc n
    skip n
    expectIntLit c,  n
    let first = pool.integers[n.intId]
    inc n
    expectIntLit c, n
    let last = pool.integers[n.intId]
    inc n
    skipParRi c, n
    dest.addIntLit(last - first + 1, n.info)
  else:
    # should not be possible, but assume length anyway
    trExpr c, dest, n
  takeParRi dest, n

proc trParams(c: var EContext; dest: var TokenBuf; n: var Cursor)

proc trProcTypeBody(c: var EContext; dest: var TokenBuf; n: var Cursor) =
  dest.add tagToken("proctype", n.info)
  # This is really stupid...
  dest.addDotToken() # name
  inc n # proc
  # name, export marker, pattern, type vars:
  for i in 0..<ParamsPos: skip n
  trParams c, dest, n

  let pinfo = n.info
  let prag = parsePragmas(c, dest, n)
  var genPragmas = openGenPragmas()
  if prag.callConv != NoCallConv:
    let name = $prag.callConv
    addKey dest, genPragmas, name, pinfo
  closeGenPragmas dest, genPragmas

  # ignore, effects and body:
  if n.kind != ParRi:
    skip n
    if n.kind != ParRi:
      skip n
  takeParRi dest, n

proc trRefBody(c: var EContext; dest: var TokenBuf; n: var Cursor; key: string) =
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
  dest.add tagToken("object", info)
  dest.addDotToken()

  dest.add tagToken("fld", info)
  let rcField = pool.syms.getOrIncl(RcField)
  dest.add symdefToken(rcField, info)
  dest.addDotToken() # pragmas
  dest.add tagToken("i", info)
  dest.addIntLit(-1, info)
  dest.addParRi() # "i"
  dest.addParRi() # "fld"

  let dataField = pool.syms.getOrIncl(DataField)
  dest.add tagToken("fld", info)
  dest.add symdefToken(dataField, info)
  dest.addDotToken() # pragmas
  trType c, dest, n, {}
  dest.addParRi() # "fld"

  dest.addParRi() # "object"

proc trAsNamedType(c: var EContext; dest: var TokenBuf; n: var Cursor) =
  let info = n.info
  var body = n
  let k = body.typeKind
  let key: string
  key = takeMangle(n, Backend, c.bits)

  var val = c.newTypes.getOrDefault(key)
  if val == SymId(0):
    val = pool.syms.getOrIncl(genericTypeName(key, c.main))
    c.newTypes[key] = val

    var buf = createTokenBuf(30)
    swap dest, buf

    dest.add tagToken("type", info)
    dest.add symdefToken(val, info)

    dest.addDotToken()
    case k
    of TupleT:
      trTupleBody c, dest, body
    of ArrayT:
      trArrayBody c, dest, body
    of RoutineTypes:
      trProcTypeBody c, dest, body
    of RefT:
      trRefBody c, dest, body, key
    else:
      error c, "expected tuple or array, but got: ", body
    dest.addParRi() # "type"

    swap dest, buf
    c.pending.add buf
    # Convert NifC type decl to Nim-gear2 type decl by
    # inserting empty export marker and type vars
    buf.insert [dotToken(NoLineInfo), dotToken(NoLineInfo)], 1
    programs.publish val, buf
  # regardless of what we had to do, we still need to add the typename:
  if k == RefT:
    dest.add tagToken("ptr", info)
    dest.add symToken(val, info)
    dest.addParRi()
  else:
    dest.add symToken(val, info)

proc addRttiField(c: var EContext; dest: var TokenBuf; info: PackedLineInfo) =
  dest.add tagToken("fld", info)
  dest.add symdefToken(pool.syms.getOrIncl(VTableField), info)
  dest.addEmpty() # pragmas
  dest.addParLe PtrT, info
  let rttiSym = pool.syms.getOrIncl("Rtti.0." & SystemModuleSuffix)
  dest.addSymUse rttiSym, info
  dest.addParRi() # "ptr"
  dest.addParRi() # "fld"

proc trObjFields(c: var EContext; dest: var TokenBuf; n: var Cursor; flags: set[TypeFlag]) =
  while n.kind != ParRi:
    case n.substructureKind
    of FldU:
      trField(c, dest, n, flags)
    of CaseU:
      # XXX for now counts each case object field as separate
      inc n
      trField(c, dest, n, flags)
      dest.add tagToken("union", n.info)
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
            dest.add tagToken("object", n.info)
            dest.addDotToken  # base type
            trObjFields(c, dest, n, flags)
            dest.addParRi # end of object
          skipParRi c, n
          skipParRi c, n
        of ElseU:
          inc n
          assert n.stmtKind == StmtsS
          inc n
          if n.exprKind == NilX:
            skip n
          else:
            dest.add tagToken("object", n.info)
            dest.addDotToken  # base type
            trObjFields(c, dest, n, flags)
            dest.addParRi # end of object
          skipParRi c, n
          skipParRi c, n
        of NilU, NotnilU, KvU, VvU, RangeU, RangesU, ParamU,
            TypevarU, EfldU, FldU, WhenU, ElifU, TypevarsU,
            CaseU, StmtsU, ParamsU, PragmasU, EitherU, JoinU,
            UnpackflatU, UnpacktupU, ExceptU, FinU, UncheckedU,
            NoSub:
          error "expected `of` or `else` inside `case`"
      dest.addParRi # end of union
      skipParRi c, n
    of NilU:
      skip n
    of NotnilU, KvU, VvU, RangeU, RangesU, ParamU, TypevarU,
        EfldU, WhenU, ElifU, ElseU, TypevarsU, OfU, StmtsU,
        ParamsU, PragmasU, EitherU, JoinU, UnpackflatU,
        UnpacktupU, ExceptU, FinU, UncheckedU, NoSub:
      error "illformed AST inside object: ", n

proc trType(c: var EContext; dest: var TokenBuf; n: var Cursor; flags: set[TypeFlag] = {}) =
  case n.kind
  of DotToken:
    dest.add n
    inc n
  of Symbol:
    let s = n.symId
    let res = tryLoadSym(s)
    if res.status == LacksNothing:
      var typeDecl = asTypeDecl(res.decl)
      var body = typeDecl.body
      if body.typeKind == DistinctT: # skips DistinctT
        let prag = parsePragmas(c, dest, typeDecl.pragmas)

        if prag.flags * {ImportcP, ImportcppP} == {}:
          inc body
          trType(c, dest, body, flags)
          inc n
        else:
          dest.add n
          inc n
      else:
        dest.add n
        inc n
    else:
      error c, "could not find symbol: " & pool.syms[s]
  of ParLe:
    case n.typeKind
    of NoType, ErrT, OrT, AndT, NotT, TypedescT, UntypedT, TypedT, TypeKindT, OrdinalT:
      error c, "type expected but got: ", n
    of IntT, UintT, FloatT, CharT, BoolT, AutoT, SymKindT, VarargsT:
      takeTree dest, n
    of MutT, LentT:
      dest.add tagToken("ptr", n.info)
      inc n
      if isViewType(n):
        dest.shrink dest.len-1 # remove the "ptr" again
        trType c, dest, n, {}
        skipParRi n
      else:
        c.loop dest, n:
          trType c, dest, n, {IsPointerOf}
    of PtrT, OutT:
      dest.add tagToken("ptr", n.info)
      inc n
      trType c, dest, n, {IsPointerOf}
      skipNilAnnotation n
      takeParRi dest, n
    of RefT:
      trAsNamedType c, dest, n
    of ArrayT, RoutineTypes:
      if IsNodecl in flags:
        trArrayBody c, dest, n
      else:
        trAsNamedType c, dest, n
    of RangetypeT:
      # skip to base type
      inc n
      trType c, dest, n
      skip n
      skip n
      skipParRi c, n
    of UarrayT:
      if IsPointerOf in flags:
        inc n
        trType c, dest, n
        skipParRi c, n
      else:
        dest.add tagToken("flexarray", n.info)
        inc n
        trType c, dest, n
        takeParRi dest, n
    of PointerT:
      dest.add tagToken("ptr", n.info)
      dest.add tagToken("void", n.info)
      dest.addParRi()
      inc n
      skipNilAnnotation n
      takeParRi dest, n
    of CstringT:
      dest.add tagToken("ptr", n.info)
      dest.add tagToken($CharT, n.info)
      dest.addIntLit(8, n.info)
      dest.addParRi()
      inc n
      skipNilAnnotation n
      takeParRi dest, n
    of StaticT, SinkT, DistinctT:
      inc n
      trType c, dest, n, flags
      skipParRi c, n
    of TupleT:
      trAsNamedType c, dest, n
    of ObjectT:
      if IsUnion in flags:
        dest.add tagToken("union", n.info)
        inc n
        # Union types don't inherit any types.
        assert n.kind == DotToken
        inc n
      else:
        dest.add n
        inc n
        if n.kind == DotToken:
          dest.add n
          inc n
        else:
          # inherited symbol
          let isPtr = n.typeKind in {RefT, PtrT}
          if isPtr: inc n
          let (s, sinfo) = getSym(c, n)
          if isPtr:
            skipNilAnnotation n
            skipParRi c, n
          dest.add symToken(s, sinfo)

        if IsInheritable in flags:
          addRttiField c, dest, n.info

      if n.kind == DotToken:
        dest.add n
        inc n
      else:
        trObjFields(c, dest, n, flags)

      takeParRi dest, n
    of EnumT, HoleyEnumT, AnumT:
      let enumKind = n.typeKind
      dest.add tagToken("enum", n.info)
      inc n
      trType c, dest, n, flags # base type
      if enumKind == AnumT:
        skip n # owner object type sym

      while n.substructureKind == EfldU:
        trEnumField(c, dest, n, flags)

      takeParRi dest, n
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
          dest.add tagToken("u", info)
          dest.addIntLit(size * 8, info)
          dest.addParRi()
        else:
          var arrBuf = createTokenBuf(16)
          arrBuf.add tagToken("array", info)
          arrBuf.add tagToken("u", info)
          arrBuf.addIntLit(8, info)
          arrBuf.addParRi()
          arrBuf.addIntLit(size, info)
          arrBuf.addParRi()
          var arrCursor = cursorAt(arrBuf, 0)
          trAsNamedType(c, dest, arrCursor)
      skip n
      skipParRi c, n
    of VoidT, NiltT, ConceptT, InvokeT:
      error c, "unimplemented type: ", n
  else:
    error c, "type expected but got: ", n

proc maybeByConstRef(c: var EContext; dest: var TokenBuf; n: var Cursor) =
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
    trLocal(c, dest, paramCursor, ParamY, TraverseSig)
    endRead(paramBuf)
    skip n
  else:
    trLocal(c, dest, n, ParamY, TraverseSig)

proc trParams(c: var EContext; dest: var TokenBuf; n: var Cursor) =
  if n.kind == DotToken:
    dest.add n
    inc n
  elif n.kind == ParLe and n.substructureKind == ParamsU:
    dest.add n
    inc n
    loop c, dest, n:
      if n.symKind != ParamY:
        error c, "expected (param) but got: ", n
      maybeByConstRef(c, dest, n)
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
    trType c, dest, retType
  else:
    trType c, dest, retType

proc parsePragmas(c: var EContext; dest: var TokenBuf; n: var Cursor): CollectedPragmas =
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
            if hookKind(n.tagId) != NoHook:
              skip n
            elif isNilAnnotation(n):
              skip n
            else:
              error c, "unknown pragma: ", n
          else:
            result.callConv = cc
            inc n
            skipParRi c, n
        of MagicP:
          inc n
          if n.kind notin {StringLit, Ident}:
            error c, "expected string literal or ident, but got: ", n
          result.flags.incl MagicP
          inc n
          skipParRi c, n
        of ImportcP, ImportcppP:
          inc n
          expectStrLit c, n
          result.extern = n.litId
          result.flags.incl pk
          inc n
          skipParRi c, n
        of ExportcP:
          inc n
          expectStrLit c, n
          result.extern = n.litId
          result.flags.incl pk
          inc n
          skipParRi c, n
        of NodeclP, SelectanyP, ThreadvarP, GlobalP, DiscardableP, NoReturnP,
           VarargsP, NoSideEffectP, NoDestroyP, ByCopyP, ByRefP,
           InlineP, NoinlineP, NoInitP, InjectP, GensymP, DirtyP, UntypedP, ViewP,
           InheritableP, PureP, ClosureP, PackedP, UnionP, IncompleteStructP:
          result.flags.incl pk
          inc n
          skipParRi c, n
        of BorrowP:
          result.flags.incl InlineP
          result.flags.incl pk
          inc n
          skipParRi c, n
        of HeaderP:
          inc n
          expectStrLit c, n
          result.header = n.litId
          inc n
          skipParRi c, n
        of DynlibP:
          inc n
          expectStrLit c, n
          result.dynlib = n.litId
          result.flags.incl DynlibP
          inc n
          skipParRi c, n
        of AlignP:
          inc n
          expectIntLit c, n
          result.align = n.intId
          inc n
          skipParRi c, n
        of BitsP:
          inc n
          expectIntLit c, n
          result.bits = n.intId
          inc n
          skipParRi c, n
        of RequiresP, EnsuresP, StringP, RaisesP, ErrorP, AssumeP, AssertP, ReportP,
           TagsP, DeprecatedP, SideEffectP, KeepOverflowFlagP, SemanticsP,
           BaseP, FinalP, PragmaP, CursorP, PassiveP, PluginP, MethodsP, CastP, SizeP,
           FeatureP:
          skip n
        of BuildP, EmitP, PushP, PopP, PassLP, PassCP, CallConvP:
          bug "unreachable"
      else:
        error c, "unknown pragma: ", n
  else:
    error c, "(pragmas) or '.' expected, but got: ", n

proc trProcBody(c: var EContext; dest: var TokenBuf; n: var Cursor) =
  if n.stmtKind == StmtsS:
    dest.add n
    inc n
    var prevStmt = NoStmt
    while n.kind != ParRi:
      prevStmt = n.stmtKind
      trStmt c, dest, n, TraverseInner
    if prevStmt == RetS or c.resultSym == SymId(0):
      discard "ok, do not add another return"
    else:
      dest.add parLeToken(RetS, n.info)
      dest.add symToken(c.resultSym, n.info)
      dest.addParRi()
    takeParRi dest, n
  else:
    trStmt c, dest, n, TraverseInner

template moveToTopLevel(c: var EContext; dest: var TokenBuf; mode: TraverseMode; body: typed) =
  if mode in {TraverseAll, TraverseInner}:
    var temp = createTokenBuf()
    swap dest, temp
    body
    swap dest, temp
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

proc makeLocalSymId(c: var EContext; s: SymId): SymId =
  let newName = makeLocalDeclName(c, s)
  result = pool.syms.getOrIncl(newName)

proc buildProcType(c: var EContext; dest: var TokenBuf; thisProc: Cursor): SymId =
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
  var beforeProcPos = dest.len
  trAsNamedType c, dest, procTypeCursor
  result = dest[dest.len - 1].symId
  dest.shrink beforeProcPos

proc trProc(c: var EContext; dest: var TokenBuf; n: var Cursor; mode: TraverseMode) =
  let thisProc = n
  c.typeCache.openScope()
  var dst = createTokenBuf(50)
  swap dest, dst
  #let toPatch = c.dest.len
  let oldResultSym = c.resultSym
  c.resultSym = SymId(0)

  let vinfo = n.info
  dest.add tagToken("proc", vinfo)
  inc n
  let (s, sinfo) = getSymDef(c, n)

  let newSym = s
  dest.add symdefToken(s, sinfo)

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
      skipToEnd n
    inc n
    skip n # skip return type
  else:
    trParams c, dest, n

  let pinfo = n.info
  let prag = parsePragmas(c, dest, n)

  var genPragmas = openGenPragmas()

  externPragmas c, dest, genPragmas, prag, pinfo
  if prag.callConv != NoCallConv:
    let name = $prag.callConv
    dest.addKey genPragmas, name, pinfo
  if InlineP in prag.flags:
    dest.addKey genPragmas, "inline", pinfo

  if SelectanyP in prag.flags:
    dest.addKey genPragmas, "selectany", pinfo

  closeGenPragmas dest, genPragmas

  skip n # miscPos

  # body:
  if isGeneric:
    skip n
  elif mode != TraverseSig or InlineP in prag.flags:
    trProcBody c, dest, n
  else:
    dest.addDotToken()
    skip n
  takeParRi dest, n
  swap dst, dest
  if prag.flags * {MagicP, DynlibP} != {} or isGeneric:
    discard "do not add to dest"
  else:
    dest.add dst

  if prag.dynlib != StrId(0):
    let typeSym = buildProcType(c, dest, thisProc)

    c.dynlibs.mgetOrPut(prag.dynlib, @[]).add (newSym, prag.extern, typeSym)

  c.typeCache.closeScope()
  c.resultSym = oldResultSym

proc trTypeDecl(c: var EContext; dest: var TokenBuf; n: var Cursor; mode: TraverseMode) =
  var dst = createTokenBuf(50)
  swap dest, dst
  #let toPatch = c.dest.len
  let decl = asTypeDecl(n)
  let isDistinct = decl.body.typeKind == DistinctT
  let vinfo = n.info
  dest.add tagToken("type", vinfo)
  inc n
  let (s, sinfo) = getSymDef(c, n)

  let newSym = s
  dest.add symdefToken(s, sinfo)

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
      skipToEnd n
    inc n
  else:
    skip n # generic parameters

  let pinfo = n.info
  let prag = parsePragmas(c, dest, n)
  var genPragmas = openGenPragmas()

  externPragmas c, dest, genPragmas, prag, pinfo
  if PackedP in prag.flags:
    dest.addKey genPragmas, "packed", pinfo
  closeGenPragmas dest, genPragmas

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
    trType c, dest, n, flags
  takeParRi dest, n
  swap dst, dest
  if isGeneric:
    discard "do not add to dest"
  else:
    dest.add dst

proc genStringLit(c: var EContext; dest: var TokenBuf; s: string; info: PackedLineInfo) =
  when sso:
    ## Generate an SSO string literal as an oconstr expression.
    ## Short strings (len <= AlwaysAvail) pack all data inline in `bytes`.
    ## Long strings (len > AlwaysAvail) use StaticSlen sentinel in `bytes`
    ## and emit a static LongString const to strLitBuf, referencing it via addr.
    let alwaysAvail = c.bits div 8 - 1 # 7 on 64-bit, 3 on 32-bit
    let staticSlen = 254'u # StaticSlen sentinel

    let bytesField = pool.syms.getOrIncl(StringBytesField)
    let moreField  = pool.syms.getOrIncl(StringMoreField)

    # Pack up to alwaysAvail chars into the `bytes` uint alongside slen.
    # LE layout: slen at bits 0..7 (LSB/byte0), chars at bits 8, 16, ...
    # BE layout: slen at bits (bits-8)..(bits-1) (MSB/byte0), chars at bits (bits-16), ...
    var bytesVal: uint = 0
    if c.bigEndian:
      if s.len <= alwaysAvail:
        bytesVal = uint(s.len) shl uint(alwaysAvail * 8)
        for i in 0 ..< s.len:
          bytesVal = bytesVal or (uint(cast[uint8](s[i])) shl uint((alwaysAvail - 1 - i) * 8))
      else:
        bytesVal = staticSlen shl uint(alwaysAvail * 8)
        for i in 0 ..< alwaysAvail:
          if i < s.len:
            bytesVal = bytesVal or (uint(cast[uint8](s[i])) shl uint((alwaysAvail - 1 - i) * 8))
    else:
      if s.len <= alwaysAvail:
        # Short string: slen in byte 0, chars in bytes 1..slen
        bytesVal = uint(s.len)
        for i in 0 ..< s.len:
          bytesVal = bytesVal or (uint(cast[uint8](s[i])) shl uint((i + 1) * 8))
      else:
        # Long string: StaticSlen in byte 0, first alwaysAvail chars in bytes 1..
        bytesVal = staticSlen
        for i in 0 ..< alwaysAvail:
          if i < s.len:
            bytesVal = bytesVal or (uint(cast[uint8](s[i])) shl uint((i + 1) * 8))

    dest.add tagToken("oconstr", info)
    useStringType c, dest, info

    # (kv bytes <bytesVal>)
    dest.add parLeToken(KvU, info)
    dest.add symToken(bytesField, info)
    dest.addUIntLit(bytesVal, info)
    dest.addParRi() # "kv"

    # (kv more nil-or-addr)
    dest.add parLeToken(KvU, info)
    dest.add symToken(moreField, info)
    if s.len <= alwaysAvail:
      dest.addParPair(NilX, info)
    else:
      # Emit a static LongString const to strLitBuf
      let litName = pool.syms.getOrIncl("strlit." & $c.strLitCounter & "." & c.main)
      inc c.strLitCounter

      c.strLitBuf.add tagToken("const", info)
      c.strLitBuf.add symdefToken(litName, info)
      c.strLitBuf.addDotToken() # no pragmas
      # type: LongString
      c.strLitBuf.add symToken(pool.syms.getOrIncl(LongStringName), info)
      # value: (oconstr LongStringName (kv fullLen len) (kv rc 0) (kv capImpl 0) (kv data "s"))
      c.strLitBuf.add tagToken("oconstr", info)
      c.strLitBuf.add symToken(pool.syms.getOrIncl(LongStringName), info)

      c.strLitBuf.add parLeToken(KvU, info)
      c.strLitBuf.add symToken(pool.syms.getOrIncl(LongStringFullLenField), info)
      c.strLitBuf.addIntLit(s.len, info)
      c.strLitBuf.addParRi() # "kv"

      c.strLitBuf.add parLeToken(KvU, info)
      c.strLitBuf.add symToken(pool.syms.getOrIncl(LongStringRcField), info)
      c.strLitBuf.addIntLit(0, info)
      c.strLitBuf.addParRi() # "kv"

      c.strLitBuf.add parLeToken(KvU, info)
      c.strLitBuf.add symToken(pool.syms.getOrIncl(LongStringCapImplField), info)
      c.strLitBuf.addIntLit(0, info)
      c.strLitBuf.addParRi() # "kv"

      c.strLitBuf.add parLeToken(KvU, info)
      c.strLitBuf.add symToken(pool.syms.getOrIncl(LongStringDataField), info)
      c.strLitBuf.addStrLit(s)
      c.strLitBuf.addParRi() # "kv"

      c.strLitBuf.addParRi() # "oconstr"
      c.strLitBuf.addParRi() # "const"

      # Reference the LongString via addr
      dest.add tagToken("addr", info)
      dest.add symToken(litName, info)
      dest.addParRi() # "addr"
    dest.addParRi() # "kv" (more)

    dest.addParRi() # "oconstr"
  else:
    dest.add tagToken("oconstr", info)
    useStringType c, dest, info

    dest.add parLeToken(KvU, info)
    let strField = pool.syms.getOrIncl(StringAField)
    dest.add symToken(strField, info)
    dest.addStrLit(s)
    dest.addParRi() # "kv"

    dest.add parLeToken(KvU, info)
    let lenField = pool.syms.getOrIncl(StringIField)
    dest.add symToken(lenField, info)
    # length also contains the "isConst" flag:
    dest.addIntLit(s.len * 2, info)
    dest.addParRi() # "kv"

    dest.addParRi() # "oconstr"

proc genStringLit(c: var EContext; dest: var TokenBuf; n: Cursor) =
  assert n.kind == StringLit
  let info = n.info
  let s {.cursor.} = pool.strings[n.litId]
  genStringLit(c, dest, s, info)

proc trStmtsExpr(c: var EContext; dest: var TokenBuf; n: var Cursor) =
  let head = n.load()
  inc n
  if isLastSon(n):
    trExpr c, dest, n
    skipParRi c, n
  else:
    dest.add head
    while n.kind != ParRi:
      if not isLastSon(n):
        trStmt c, dest, n
      else:
        trExpr c, dest, n
    takeParRi dest, n

proc trTupleConstr(c: var EContext; dest: var TokenBuf; n: var Cursor) =
  dest.add tagToken("oconstr", n.info)
  inc n
  var tupleType = n
  c.trType(dest, n, {})

  inc tupleType
  var counter = 0
  while n.kind != ParRi:
    dest.add tagToken("kv", n.info)
    let isKvU = tupleType.substructureKind == KvU
    if isKvU:
      inc tupleType # skip "kv"
      skip tupleType # skip key
    dest.add symToken(ithTupleField(c, counter, tupleType), n.info)
    skip tupleType
    if isKvU:
      skipParRi tupleType

    inc counter
    if n.substructureKind == KvU:
      inc n # skip "kv"
      skip n # skip key
      trExpr c, dest, n
      skipParRi c, n
    else:
      trExpr c, dest, n
    dest.addParRi() # "kv"
  takeParRi dest, n

proc trConv(c: var EContext; dest: var TokenBuf; n: var Cursor) =
  let info = n.info
  let beforeConv = dest.len
  dest.add tagToken("conv", info)
  inc n
  let destType = n
  trType(c, dest, n)
  let srcType = getType(c.typeCache, n)
  if destType.typeKind == CstringT and isStringType(srcType):
    var isSuffix = false
    if n.exprKind == SufX:
      isSuffix = true
      inc n
    if n.kind == StringLit:
      # evaluate the conversion at compile time:
      dest.shrink beforeConv
      dest.addStrLit pool.strings[n.litId]
      inc n
      if isSuffix:
        inc n
        skipParRi c, n
      skipParRi c, n
    else:
      when sso:
        bug "cannot convert a string to cstring at runtime"
      else:
        let strField = pool.syms.getOrIncl(StringAField)
        dest.add tagToken("dot", info)
        trExpr(c, dest, n)
        dest.add symToken(strField, info)
        dest.addIntLit(0, info)
        dest.addParRi()
        takeParRi dest, n
  else:
    trExpr(c, dest, n)
    takeParRi dest, n

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
    of ErrX, AtX, DerefX, DotX, PatX, ParX, AddrX, AndX, OrX,
        XorX, NotX, NegX, SizeofX, AlignofX, OffsetofX,
        OconstrX, AconstrX, BracketX, CurlyX, CurlyatX, OvfX,
        AddX, SubX, MulX, DivX, ModX, ShrX, ShlX, BitandX,
        BitorX, BitxorX, BitnotX, EqX, NeqX, LeX, LtX, CallX,
        CmdX, CchoiceX, OchoiceX, PragmaxX, QuotedX, HderefX,
        DdotX, HaddrX, NewrefX, NewobjX, TupX, TupconstrX,
        SetconstrX, TabconstrX, AshrX, BaseobjX, HconvX,
        DconvX, CallstrlitX, InfixX, PrefixX, HcallX,
        CompilesX, DeclaredX, DefinedX, AstToStrX,
        InstanceofX, ProccallX, HighX, LowX, TypeofX, UnpackX,
        FieldsX, FieldpairsX, EnumtostrX, IsmainmoduleX,
        DefaultobjX, DefaulttupX, DefaultdistinctX, DelayX,
        Delay0X, SuspendX, ExprX, DoX, ArratX, TupatX,
        PlussetX, MinussetX, MulsetX, XorsetX, EqsetX, LesetX,
        LtsetX, InsetX, CardX, EmoveX, DestroyX, DupX, CopyX,
        WasmovedX, SinkhX, TraceX, InternalTypeNameX,
        InternalFieldPairsX, FailedX, IsX, EnvpX, KvX, NoExpr:
      result = false

proc getCompilerProc(c: var EContext; name: string; isInline=false): string =
  result = name & ".0." & SystemModuleSuffix

proc trArrAt(c: var EContext; dest: var TokenBuf; n: var Cursor) =
  dest.add parLeToken(AtX, n.info) # NIFC uses the `at` token for array indexing
  inc n
  trExpr(c, dest, n)
  let beforeIndex = dest.len
  let info = n.info
  let isUnsigned = getType(c.typeCache, n).typeKind in {UIntT, CharT}
  trExpr(c, dest, n)
  if n.kind != ParRi:
    var indexDest = createTokenBuf(dest.len - beforeIndex)
    for i in beforeIndex..<dest.len:
      indexDest.add dest[i]
    dest.shrink beforeIndex
    let indexB = n
    skip n
    if n.kind != ParRi:
      # we have `low(T)`:
      let indexA = n
      skip n
      if BoundCheck in c.activeChecks:
        let abProcName = getCompilerProc(c, if isUnsigned: "nimUcheckAB" else: "nimIcheckAB", true)
        dest.copyIntoUnchecked "call", info:
          dest.add symToken(pool.syms.getOrIncl(abProcName), info)
          dest.add indexDest
          dest.addSubtree indexA
          dest.addSubtree indexB
      else:
        let indexType = if isUnsigned: c.typeCache.builtins.uintType else: c.typeCache.builtins.intType
        # we need the substraction regardless:
        dest.addParLe SubX, info
        dest.addSubtree indexType
        dest.add indexDest
        dest.addSubtree indexA
        dest.addParRi()
    else:
      # we only have to care about the upper bound:
      if BoundCheck in c.activeChecks:
        let abProcName = getCompilerProc(c, if isUnsigned: "nimUcheckB" else: "nimIcheckB", true)
        dest.copyIntoUnchecked "call", info:
          dest.add symToken(pool.syms.getOrIncl(abProcName), info)
          dest.add indexDest
          dest.addSubtree indexB
      else:
        dest.add indexDest
  takeParRi dest, n

proc trFieldname(c: var EContext; dest: var TokenBuf; n: var Cursor) =
  if n.kind == Symbol:
    dest.add n
    inc n
  else:
    trExpr c, dest, n

proc trExpr(c: var EContext; dest: var TokenBuf; n: var Cursor) =
  case n.kind
  of EofToken, ParRi:
    error c, "BUG: unexpected ')' or EofToken"
  of ParLe:
    case n.exprKind
    of EqX, NeqX, LeX, LtX:
      # `(eq T X X)` in Nimony carries `T`, but NIFC comparisons are `(eq X X)` — see
      # `cmpOp` in llvmgenexprs.nim. Walk `T` with `trType` for side effects, omit from dest.
      dest.add n
      inc n
      let beforeType = dest.len
      trType(c, dest, n)
      dest.shrink beforeType
      trExpr(c, dest, n)
      trExpr(c, dest, n)
      takeParRi dest, n
    of AddX, SubX, MulX, DivX, ModX, ShrX, ShlX, BitandX, BitorX, BitxorX:
      # `(op T X X)` — NIFC typed binops need the type (`signedBinOp` / `unsignedBinOp`).
      dest.add n
      inc n
      trType(c, dest, n)
      trExpr(c, dest, n)
      trExpr(c, dest, n)
      takeParRi dest, n
    of BitnotX:
      # `(bitnot T X)` — NIFC expects type + expr (see BitnotC in llvmgenexprs.nim).
      dest.add n
      inc n
      trType(c, dest, n)
      trExpr(c, dest, n)
      takeParRi dest, n
    of BaseobjX:
      # `(baseobj T INTLIT X)` — keep `T` and depth for NIFC (BaseobjC).
      dest.add n
      inc n
      trType(c, dest, n)
      expectIntLit c, n
      dest.add n
      inc n
      trExpr(c, dest, n)
      takeParRi dest, n
    of CastX:
      dest.add n
      inc n
      trType(c, dest, n)
      trExpr(c, dest, n)
      takeParRi dest, n
    of HconvX, ConvX:
      trConv c, dest, n
    of DconvX:
      inc n
      let beforeType = dest.len
      trType(c, dest, n)
      dest.shrink beforeType
      trExpr(c, dest, n)
      skipParRi(c, n)
    of AconstrX:
      dest.add tagToken("aconstr", n.info)
      inc n
      trType(c, dest, n)
      while n.kind != ParRi:
        trExpr(c, dest, n)
      takeParRi dest, n
    of OconstrX:
      dest.add tagToken("oconstr", n.info)
      inc n
      trType(c, dest, n)
      while n.kind != ParRi:
        if n.substructureKind == KvU:
          dest.add n # KvU
          inc n
          takeTree dest, n # key
          trExpr c, dest, n # value
          if n.kind != ParRi:
            # optional inheritance
            takeTree dest, n
          takeParRi dest, n
        else:
          trExpr c, dest, n
      takeParRi dest, n
    of TupConstrX:
      trTupleConstr c, dest, n
    of CmdX, CallStrLitX, InfixX, PrefixX, HcallX, CallX:
      dest.add tagToken("call", n.info)
      inc n
      while n.kind != ParRi:
        trExpr(c, dest, n)
      takeParRi dest, n
    of ExprX:
      trStmtsExpr c, dest, n
    of ArrAtX:
      trArrAt c, dest, n
    of TupatX:
      let fieldType = getType(c.typeCache, n)
      dest.add tagToken("dot", n.info)
      inc n # skip tag
      trExpr c, dest, n # tuple
      expectIntLit c, n
      dest.add symToken(ithTupleField(c, int pool.integers[n.intId], fieldType), n.info)
      inc n # skip index
      dest.addIntLit(0, n.info) # inheritance
      takeParRi dest, n
    of DotX:
      dest.add tagToken("dot", n.info)
      inc n # skip tag
      trExpr c, dest, n # obj
      trFieldname c, dest, n # field
      if n.kind != ParRi:
        trExpr c, dest, n # inheritance depth
      takeParRi dest, n
    of DdotX:
      dest.add tagToken("dot", n.info)
      dest.add tagToken("deref", n.info)
      inc n # skip tag
      trExpr c, dest, n
      dest.addParRi()
      trFieldname c, dest, n
      trExpr c, dest, n
      takeParRi dest, n
    of HaddrX, AddrX:
      dest.add tagToken("addr", n.info)
      inc n
      trExpr(c, dest, n)
      takeParRi dest, n
    of HderefX, DerefX:
      dest.add tagToken("deref", n.info)
      inc n
      trExpr(c, dest, n)
      takeParRi dest, n
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
          dest.add n
          inc n
        else:
          trExpr c, dest, n
        inc n # suf
        skipParRi c, n
      else:
        dest.add n
        inc n
        trExpr c, dest, n
        dest.add n
        inc n
        takeParRi dest, n
    of AshrX:
      dest.add tagToken("shr", n.info)
      inc n
      var bits = -1'i64
      if n.typeKind in {IntT, UIntT}:
        var bitsToken = n
        inc bitsToken
        bits = pool.integers[bitsToken.intId]
      else:
        #error c, "expected int/uint type for ashr, got: ", n
        discard
      trType(c, dest, n)
      dest.copyIntoKind CastX, n.info:
        dest.add tagToken("i", n.info)
        dest.addIntLit(bits, n.info)
        dest.addParRi()
        trExpr c, dest, n
      dest.copyIntoKind CastX, n.info:
        dest.add tagToken("u", n.info)
        dest.addIntLit(bits, n.info)
        dest.addParRi()
        trExpr c, dest, n
      takeParRi dest, n
    of ErrX, NewobjX, NewrefX, SetConstrX, PlusSetX, MinusSetX, MulSetX, XorSetX, EqSetX, LeSetX, LtSetX,
       InSetX, CardX, BracketX, CurlyX, TupX, CompilesX, DeclaredX, DefinedX, AstToStrX, HighX, LowX, TypeofX, UnpackX,
       FieldsX, FieldpairsX, EnumtostrX, IsmainmoduleX, DefaultobjX, DefaulttupX, DefaultdistinctX, DoX, CchoiceX, OchoiceX,
       EmoveX, DestroyX, DupX, CopyX, WasmovedX, SinkhX, TraceX, CurlyatX, PragmaxX, QuotedX, TabconstrX,
       InstanceofX, ProccallX, InternalTypeNameX, InternalFieldPairsX, FailedX, IsX, EnvpX, DelayX, Delay0X, SuspendX:
      error c, "BUG: not eliminated: ", n
      #skip n
    of AtX, PatX, ParX, NilX, InfX, NeginfX, NanX, FalseX, TrueX, AndX, OrX, NotX, NegX, OvfX:
      dest.add n
      inc n
      while n.kind != ParRi:
        trExpr c, dest, n
      takeParRi dest, n
    of SizeofX, AlignofX, OffsetofX:
      dest.add n
      inc n
      trType c, dest, n
      while n.kind != ParRi:
        trExpr c, dest, n
      takeParRi dest, n
    of XorX:
      dest.add tagToken("neq", n.info)
      inc n
      while n.kind != ParRi:
        trExpr c, dest, n
      takeParRi dest, n
    of KvX:
      dest.add n
      inc n
      takeTree dest, n
      trExpr c, dest, n
      if n.kind != ParRi:
        takeTree dest, n
      takeParRi dest, n
    of NoExpr:
      trType c, dest, n
  of SymbolDef:
    dest.add n
    inc n
  of Symbol:
    var inlineValue = getInitValue(c.typeCache, n.symId)
    var inlineValueCopy = inlineValue
    if not cursorIsNil(inlineValue) and inlineValue.kind != DotToken and isSimpleLiteral(inlineValueCopy):
      trExpr(c, dest, inlineValue)
    else:
      dest.add n
    inc n
  of StringLit:
    genStringLit c, dest, n
    inc n
  of UnknownToken, DotToken, Ident, CharLit, IntLit, UIntLit, FloatLit:
    dest.add n
    inc n

proc trLocal(c: var EContext; dest: var TokenBuf; n: var Cursor; tag: SymKind; mode: TraverseMode) =
  var symKind = if tag == ResultY: VarY else: tag
  var localDecl = n
  let toPatch = dest.len
  let vinfo = n.info
  dest.addParLe symKind, vinfo
  inc n
  let (s, sinfo) = getSymDef(c, n)
  if tag == ResultY:
    c.resultSym = s
  skipExportMarker c, n
  let pinfo = n.info
  let prag = parsePragmas(c, dest, n)

  dest.add symdefToken(s, sinfo)

  var genPragmas = openGenPragmas()
  if tag != ParamY:
    externPragmas c, dest, genPragmas, prag, pinfo

  if ThreadvarP in prag.flags:
    dest[toPatch] = tagToken("tvar", vinfo)
    symKind = TvarY
  elif GlobalP in prag.flags:
    dest[toPatch] = tagToken("gvar", vinfo)
    symKind = GvarY

  if prag.align != IntId(0):
    dest.addKeyVal genPragmas, "align", intToken(prag.align, pinfo), pinfo
  if prag.bits != IntId(0):
    dest.addKeyVal genPragmas, "bits", intToken(prag.bits, pinfo), pinfo
  closeGenPragmas dest, genPragmas

  c.typeCache.registerLocal(s, symKind, n)
  trType c, dest, n

  if mode == TraverseSig:
    if localDecl.substructureKind == ParamU:
      # Parameter decls in NIFC have no dot token for the default value!
      discard
    else:
      # Imported variables don't need initial values.
      dest.addDotToken
    skip n
  else:
    trExpr c, dest, n
  takeParRi dest, n

proc trWhile(c: var EContext; dest: var TokenBuf; n: var Cursor) =
  let info = n.info
  c.nestedIn.add (WhileS, SymId(0))
  dest.add n
  inc n
  trExpr c, dest, n
  trStmt c, dest, n
  takeParRi dest, n
  let lab = c.nestedIn[^1][1]
  if lab != SymId(0):
    dest.add tagToken("lab", info)
    dest.add symdefToken(lab, info)
    dest.addParRi()
  discard c.nestedIn.pop()

proc trBlock(c: var EContext; dest: var TokenBuf; n: var Cursor) =
  let info = n.info
  inc n
  if n.kind == DotToken:
    c.nestedIn.add (BlockS, SymId(0))
    inc n
  else:
    let (s, _) = getSymDef(c, n)
    c.nestedIn.add (BlockS, s)
  dest.add tagToken("scope", info)
  trStmt c, dest, n
  takeParRi dest, n
  let lab = c.nestedIn[^1][1]
  if lab != SymId(0):
    dest.add tagToken("lab", info)
    dest.add symdefToken(lab, info)
    dest.addParRi()
  discard c.nestedIn.pop()

proc trBreak(c: var EContext; dest: var TokenBuf; n: var Cursor) =
  let info = n.info
  inc n
  if n.kind == DotToken:
    inc n
    dest.add tagToken("break", info)
  else:
    expectSym c, n
    let lab = n.symId
    inc n
    dest.add tagToken("jmp", info)
    dest.add symToken(lab, info)
  takeParRi dest, n

proc trIf(c: var EContext; dest: var TokenBuf; n: var Cursor) =
  # (if cond (.. then ..) (.. else ..))
  dest.add n
  inc n
  while n.kind == ParLe and n.substructureKind == ElifU:
    dest.add n
    inc n # skips '(elif'
    trExpr c, dest, n
    trStmt c, dest, n
    takeParRi dest, n
  if n.kind == ParLe and n.substructureKind == ElseU:
    dest.add n
    inc n
    trStmt c, dest, n
    takeParRi dest, n
  takeParRi dest, n

include stringcases

proc trStringCase(c: var EContext; dest: var TokenBuf; n: var Cursor): bool =
  var nb = n
  inc nb
  let selectorType = getType(c.typeCache, nb)
  if isSomeStringType(selectorType):
    transformStringCase(c, dest, n)
    result = true
  else:
    result = false

proc trCase(c: var EContext; dest: var TokenBuf; n: var Cursor) =
  if trStringCase(c, dest, n):
    return
  dest.add n
  inc n
  trExpr c, dest, n
  while n.kind != ParRi:
    case n.substructureKind
    of OfU:
      dest.add n
      inc n
      if n.kind == ParLe and n.substructureKind == RangesU:
        inc n
        dest.add "ranges", n.info
        while n.kind != ParRi:
          if n.kind == ParLe and n.substructureKind == RangeU:
            inc n
            dest.add "range", n.info
            while n.kind != ParRi:
              trExpr c, dest, n
            takeParRi dest, n
          else:
            trExpr c, dest, n
        takeParRi dest, n
      else:
        trExpr c, dest, n
      trStmt c, dest, n
      takeParRi dest, n
    of ElseU:
      dest.add n
      inc n
      trStmt c, dest, n
      takeParRi dest, n
    of NilU, NotnilU, KvU, VvU, RangeU, RangesU, ParamU,
        TypevarU, EfldU, FldU, WhenU, ElifU, TypevarsU, CaseU,
        StmtsU, ParamsU, PragmasU, EitherU, JoinU,
        UnpackflatU, UnpacktupU, ExceptU, FinU, UncheckedU,
        NoSub:
      error c, "expected (of) or (else) but got: ", n
  takeParRi dest, n

proc trKeepovf(c: var EContext; dest: var TokenBuf; n: var Cursor) =
  dest.add n
  inc n
  trExpr c, dest, n # (add ...)
  trExpr c, dest, n # destination
  takeParRi dest, n

proc trRaise(c: var EContext; dest: var TokenBuf; n: var Cursor) =
  let info = n.info
  inc n
  if c.exceptLabels.len == 0:
    # translate `raise` to `return`:
    dest.addParLe RetS, info
    trExpr c, dest, n
  else:
    # translate `raise` to `goto`:
    skip n # raise expression handled in constparams.nim
    let lab = c.exceptLabels[^1]
    dest.add tagToken("jmp", info)
    dest.add symToken(lab, info)
  takeParRi dest, n

proc trTry(c: var EContext; dest: var TokenBuf; n: var Cursor) =
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
  trStmt c, dest, n

  if hasExcept:
    dest.addParLe IfS, n.info

  while n.substructureKind == ExceptU:
    let lab = c.exceptLabels[oldLen]
    dest.copyIntoKind ElifU, n.info:
      dest.addParPair(FalseX, n.info)
      dest.copyIntoKind StmtsS, n.info:
        dest.add tagToken("lab", n.info)
        dest.add symdefToken(lab, n.info)
        dest.addParRi()
        inc n
        if n.stmtKind == LetS:
          trStmt c, dest, n
        else:
          skip n # skip `T`
        trStmt c, dest, n
        skipParRi n
  c.exceptLabels.shrink oldLen

  # Since we duplicated the finally statements before every `raise` statement we
  # know that when control flow reaches here, no error was raised. Hence we do not
  # need to add logic to re-raise an exception here.
  if n.substructureKind == FinU:
    if hasExcept:
      dest.addParLe ElseU, n.info
    inc n
    trStmt c, dest, n
    skipParRi n
    if hasExcept:
      dest.addParRi()
  skipParRi n
  if hasExcept:
    dest.addParRi()

proc trStmt(c: var EContext; dest: var TokenBuf; n: var Cursor; mode = TraverseInner) =
  case n.kind
  of DotToken:
    dest.add n
    inc n
  of ParLe:
    case n.stmtKind
    of NoStmt:
      if n.tagId == TagId(KeepovfTagId):
        trKeepovf c, dest, n
      else:
        error c, "unknown statement: ", n
    of PragmaxS:
      inc n
      skip n
      trStmt c, dest, n, mode
      skipParRi n
    of StmtsS:
      if mode == TraverseTopLevel:
        inc n
        while n.kind notin {EofToken, ParRi}:
          trStmt c, dest, n, mode
        skipParRi c, n
      else:
        dest.add n
        inc n
        c.loop dest, n:
          trStmt c, dest, n, mode
    of ScopeS:
      c.typeCache.openScope()
      if mode == TraverseTopLevel:
        inc n
        while n.kind notin {EofToken, ParRi}:
          trStmt c, dest, n, mode
        skipParRi c, n
      else:
        dest.add n
        inc n
        c.loop dest, n:
          trStmt c, dest, n, mode
      c.typeCache.closeScope()
    of VarS, LetS, CursorS, PatternvarS:
      trLocal c, dest, n, VarY, mode
    of ResultS:
      trLocal c, dest, n, ResultY, mode
    of GvarS, GletS:
      trLocal c, dest, n, GvarY, mode
    of TvarS, TletS:
      trLocal c, dest, n, TvarY, mode
    of ConstS:
      trLocal c, dest, n, ConstY, mode
    of CallKindsS:
      dest.add tagToken("call", n.info)
      inc n
      c.loop dest, n:
        trExpr c, dest, n
    of EmitS, AsmS:
      dest.add n
      inc n
      c.loop dest, n:
        if n.kind == StringLit:
          dest.add n
          inc n
        elif n.exprkind == SufX:
          inc n
          assert n.kind == StringLit
          dest.add n
          skipToEnd n
        else:
          trExpr c, dest, n
    of AsgnS, RetS:
      dest.add n
      inc n
      c.loop dest, n:
        trExpr c, dest, n
    of DiscardS:
      let discardToken = n
      inc n
      if n.kind in {StringLit, DotToken}:
        # eliminates discard without side effects
        inc n
        skipParRi c, n
      else:
        dest.add discardToken
        trExpr c, dest, n
        takeParRi dest, n
    of BreakS: trBreak c, dest, n
    of WhileS: trWhile c, dest, n
    of BlockS: trBlock c, dest, n
    of IfS: trIf c, dest, n
    of CaseS: trCase c, dest, n
    of YldS, ForS, InclS, ExclS, DeferS, UnpackDeclS:
      error c, "BUG: not eliminated: ", n
    of TryS:
      trTry c, dest, n
    of RaiseS:
      trRaise c, dest, n
    of FuncS, ProcS, ConverterS, MethodS:
      moveToTopLevel(c, dest, mode):
        trProc c, dest, n, mode
    of ImportS:
      # Collect module suffixes for init proc generation:
      inc n
      while n.kind != ParRi:
        if n.kind == Ident:
          c.importedModuleSuffixes.add pool.strings[n.litId]
        skip n
      inc n # skip ParRi
    of MacroS, TemplateS, IncludeS, FromimportS, ImportExceptS, ExportS, CommentS, IteratorS,
       ImportasS, ExportexceptS, BindS, MixinS, UsingS, StaticstmtS:
      # pure compile-time construct, ignore:
      skip n
    of TypeS:
      moveToTopLevel(c, dest, mode):
        trTypeDecl c, dest, n, mode
    of ContinueS, WhenS:
      error c, "unreachable: ", n
    of PragmasS, AssumeS, AssertS:
      skip n
  else:
    assert n.kind != ParRi
    error c, "statement expected, but got: ", n

proc transformInlineRoutines(c: var EContext; dest: var TokenBuf; n: var Cursor) =
  var swapped = createTokenBuf()
  swap dest, swapped

  var toTransform = createTokenBuf()
  toTransform.copyIntoKind StmtsS, n.info:
    takeTree(toTransform, n)
  var t = beginRead(toTransform)
  var dest = transform(c, t, c.main, c.bits)
  var d = beginRead(dest)

  inc d # skips (stmts

  swap dest, swapped

  trStmt c, dest, d, TraverseSig
  while d.kind != ParRi:
    trStmt c, dest, d, TraverseAll

proc writeOutput(c: var EContext; dest: var TokenBuf; rootInfo: PackedLineInfo; destfileName: string): TokenBuf =
  # Build the final output with stmts wrapper and includes
  result = createTokenBuf()
  result.add tagToken("stmts", rootInfo)

  # Add all the generated content
  result.add dest

  # Close the stmts wrapper
  result.addParRi()

  writeFile result, destfileName, OnlyIfChanged

proc initDynlib(c: var EContext; dest: var TokenBuf; rootInfo: PackedLineInfo) =
  # dynlib init:
  for key, vals in c.dynlibs:
    let dynlib = pool.strings[key]
    var tmp = pool.syms.getOrIncl "Dl." & dynlib & "." & $getTmpId(c) & "." & c.main

    # nimLoadLibrary
    dest.add tagToken("gvar", rootInfo)
    dest.add symdefToken(tmp, rootInfo)
    dest.addDotToken()
    dest.add tagToken("ptr", rootInfo)
    dest.add tagToken("void", rootInfo)
    dest.addParRi()
    dest.addParRi()
    dest.add tagToken("call", rootInfo)
    dest.add symToken(pool.syms.getOrIncl(getCompilerProc(c, "nimLoadLibrary", false)), rootInfo)
    dest.addStrLit dynlib
    dest.addParRi()

    dest.addParRi()

    # nimGetProcAddr
    for (varName, val, typeSym) in vals:
      let procName = pool.strings[val]
      dest.add tagToken("gvar", rootInfo)
      dest.add symdefToken(varName, rootInfo)
      dest.addDotToken()
      dest.add symToken(typeSym, rootInfo)

      dest.add tagToken("cast", rootInfo)
      dest.add symToken(typeSym, rootInfo)
      dest.add tagToken("call", rootInfo)
      dest.add symToken(pool.syms.getOrIncl(getCompilerProc(c, "nimGetProcAddr", false)), rootInfo)
      dest.add symToken(tmp, rootInfo) # library
      dest.addStrLit procName # proc name
      dest.addParRi()
      dest.addParRi()

      dest.addParRi()

proc initProcName(moduleSuffix: string): string =
  "`ini.0." & moduleSuffix

proc genInitProc(c: var EContext; dest: var TokenBuf; rootInfo: PackedLineInfo; importedSuffixes: seq[string]) =
  ## Generate an explicit init proc for this module that:
  ## 1. Guards against double-initialization
  ## 2. Calls imported modules' init procs in order
  ## 3. Contains this module's top-level executable code (via a call from NIFC's init section)
  let initSym = pool.syms.getOrIncl(initProcName(c.main))
  let guardSym = pool.syms.getOrIncl("`iniGuard.0." & c.main)

  # Emit the guard variable: (gvar :InitGuard.suffix . (bool) .)
  dest.add tagToken("gvar", rootInfo)
  dest.add symdefToken(guardSym, rootInfo)
  dest.addDotToken()
  dest.add tagToken("bool", rootInfo)
  dest.addParRi()
  dest.addDotToken()
  dest.addParRi()

  # Emit the init proc declaration: (proc NAME (params) RETTYPE PRAGMAS BODY)
  dest.add tagToken("proc", rootInfo)
  dest.add symdefToken(initSym, rootInfo)
  # params: empty
  dest.add tagToken("params", rootInfo)
  dest.addParRi()
  # return type: void
  dest.addDotToken()
  # pragmas:
  dest.addDotToken()
  # body:
  dest.add tagToken("stmts", rootInfo)

  # Guard: if InitGuard.suffix: return
  dest.add tagToken("if", rootInfo)
  dest.add tagToken("elif", rootInfo)
  dest.add symToken(guardSym, rootInfo)
  dest.add tagToken("stmts", rootInfo)
  dest.add tagToken("ret", rootInfo)
  dest.addDotToken()
  dest.addParRi() # ret
  dest.addParRi() # stmts
  dest.addParRi() # elif
  dest.addParRi() # if

  # Set guard: (asgn InitGuard.suffix (true))
  dest.add tagToken("asgn", rootInfo)
  dest.add symToken(guardSym, rootInfo)
  dest.add tagToken("true", rootInfo)
  dest.addParRi() # true
  dest.addParRi() # asgn

  # Call each imported module's init proc:
  for suffix in importedSuffixes:
    let importInitSym = pool.syms.getOrIncl(initProcName(suffix))
    dest.add tagToken("call", rootInfo)
    dest.add symToken(importInitSym, rootInfo)
    dest.addParRi()

proc genInitProcEnd(c: var EContext; dest: var TokenBuf; rootInfo: PackedLineInfo) =
  # Close: stmts, proc
  dest.addParRi() # stmts (body)
  dest.addParRi() # proc

proc genMainProc(c: var EContext; dest: var TokenBuf; rootInfo: PackedLineInfo) =
  ## Generate cmdCount/cmdLine globals and a C main() wrapper for the main module.
  ## The gvars get exportc pragmas so NIFC defines them with the expected C names.
  ## Symbol names must contain dots to be recognized as Symbol tokens (not Ident) in NIF.
  let initSym = pool.syms.getOrIncl(initProcName(c.main))

  # Declare a nodecl importc "char" type alias so argv/cmdLine use plain C `char`
  # instead of NC8 (unsigned char). The C standard requires char** for main's argv.
  let ccharSym = pool.syms.getOrIncl("`cchar.0." & c.main)
  dest.add tagToken("type", rootInfo)
  dest.add symdefToken(ccharSym, rootInfo)
  dest.add tagToken("pragmas", rootInfo)
  dest.add tagToken("importc", rootInfo)
  dest.addStrLit("char", rootInfo)
  dest.addParRi() # importc
  dest.add tagToken("nodecl", rootInfo)
  dest.addParRi() # nodecl
  dest.addParRi() # pragmas
  dest.add tagToken("i", rootInfo) # body: (i 8)
  dest.addIntLit(8, rootInfo)
  dest.addParRi() # i
  dest.addParRi() # type

  # (gvar :cmdCount (pragmas (exportc "cmdCount")) (i 32) .)
  let cmdCountSym = pool.syms.getOrIncl("`cmdCount.0." & c.main)
  dest.add tagToken("gvar", rootInfo)
  dest.add symdefToken(cmdCountSym, rootInfo)
  dest.add tagToken("pragmas", rootInfo)
  dest.add tagToken("exportc", rootInfo)
  dest.addStrLit("cmdCount", rootInfo)
  dest.addParRi() # exportc
  dest.addParRi() # pragmas
  dest.add tagToken("i", rootInfo)
  dest.addIntLit(32, rootInfo)
  dest.addParRi() # i
  dest.addDotToken() # no init value
  dest.addParRi() # gvar

  # (gvar :cmdLine (pragmas (exportc "cmdLine")) (ptr (ptr cchar)) .)
  let cmdLineSym = pool.syms.getOrIncl("`cmdLine.0." & c.main)
  dest.add tagToken("gvar", rootInfo)
  dest.add symdefToken(cmdLineSym, rootInfo)
  dest.add tagToken("pragmas", rootInfo)
  dest.add tagToken("exportc", rootInfo)
  dest.addStrLit("cmdLine", rootInfo)
  dest.addParRi() # exportc
  dest.addParRi() # pragmas
  dest.add tagToken("ptr", rootInfo)
  dest.add tagToken("ptr", rootInfo)

  dest.add tagToken("c", rootInfo)
  dest.addIntLit(8, rootInfo)
  dest.addParRi() # c 8

  dest.addParRi() # inner ptr
  dest.addParRi() # outer ptr
  dest.addDotToken() # no init value
  dest.addParRi() # gvar

  # Generate: (proc :main (params (param :argc . (i 32)) (param :argv . (ptr (ptr cchar)))) (i 32) (pragmas (exportc "main")) (stmts ...))
  let mainSym = pool.syms.getOrIncl("`main.0." & c.main)
  let argcSym = pool.syms.getOrIncl("`argc.0." & c.main)
  let argvSym = pool.syms.getOrIncl("`argv.0." & c.main)
  dest.add tagToken("proc", rootInfo)
  dest.add symdefToken(mainSym, rootInfo)
  # params
  dest.add tagToken("params", rootInfo)
  # (param :argc . (i 32))
  dest.add tagToken("param", rootInfo)
  dest.add symdefToken(argcSym, rootInfo)
  dest.addDotToken()
  dest.add tagToken("i", rootInfo)
  dest.addIntLit(32, rootInfo)
  dest.addParRi() # i
  dest.addParRi() # param
  # (param :argv . (ptr (ptr cchar)))
  dest.add tagToken("param", rootInfo)
  dest.add symdefToken(argvSym, rootInfo)
  dest.addDotToken()
  dest.add tagToken("ptr", rootInfo)
  dest.add tagToken("ptr", rootInfo)
  dest.add symToken(ccharSym, rootInfo)
  dest.addParRi() # inner ptr
  dest.addParRi() # outer ptr
  dest.addParRi() # param
  dest.addParRi() # params
  # return type: (i 32)
  dest.add tagToken("i", rootInfo)
  dest.addIntLit(32, rootInfo)
  dest.addParRi() # i
  # pragmas: (pragmas (exportc "main"))
  dest.add tagToken("pragmas", rootInfo)
  dest.add tagToken("exportc", rootInfo)
  dest.addStrLit("main", rootInfo)
  dest.addParRi() # exportc
  dest.addParRi() # pragmas
  # body
  dest.add tagToken("stmts", rootInfo)
  # (asgn cmdCount argc)
  dest.add tagToken("asgn", rootInfo)
  dest.add symToken(cmdCountSym, rootInfo)
  dest.add symToken(argcSym, rootInfo)
  dest.addParRi() # asgn
  # (asgn cmdLine argv)
  dest.add tagToken("asgn", rootInfo)
  dest.add symToken(cmdLineSym, rootInfo)

  dest.add tagToken("cast", rootInfo)
  dest.add tagToken("ptr", rootInfo)
  dest.add tagToken("ptr", rootInfo)
  dest.add tagToken("c", rootInfo)
  dest.addIntLit(8, rootInfo)
  dest.addParRi() # c 8
  dest.addParRi() # inner ptr
  dest.addParRi() # outer ptr

  dest.add symToken(argvSym, rootInfo)
  dest.addParRi() # asgn

  dest.addParRi() # asgn
  # (call ini.0.modname)
  dest.add tagToken("call", rootInfo)
  dest.add symToken(initSym, rootInfo)
  dest.addParRi() # call
  # (ret 0)
  dest.add tagToken("ret", rootInfo)
  dest.addIntLit(0, rootInfo)
  dest.addParRi() # ret
  dest.addParRi() # stmts
  dest.addParRi() # proc

proc isTopLevelDecl(n: Cursor): bool {.inline.} =
  ## Returns true for declarations that should stay at the top level
  ## (outside the init proc). Everything else is executable code or
  ## local state that belongs inside the init proc.
  n.stmtKind in {ProcS, FuncS, ConverterS, MethodS, TypeS,
    IncludeS, ImportS, FromimportS, ImportExceptS, ExportS,
    ImportasS, ExportexceptS, CommentS, IteratorS,
    BindS, MixinS, UsingS, StaticstmtS,
    ConstS, PragmasS, EmitS}

const RuntimeVarKinds = {VarY, LetY, ResultY, CursorY, PatternvarY, GvarY, GletY, TvarY, TletY}

proc initHasCall(c: var EContext; n: Cursor): bool =
  ## Returns true if the init expression of a global var/let decl contains any
  ## function call or runtime variable reference. Such inits cannot be emitted
  ## inline by NIFC at C file scope (only literals and compile-time constants
  ## are valid C file-scope initializers).
  var n = n
  inc n    # skip the gvar/glet/tvar/tlet tag
  skip n   # skip SymbolDef
  skip n   # skip export marker
  skip n   # skip pragmas
  skip n   # skip type
  # Now at the init value; scan its subtree
  var nested = 0
  while true:
    case n.kind
    of ParRi:
      if nested == 0: break
      dec nested
      inc n
    of EofToken: break
    of ParLe:
      if n.stmtKind in CallKindsS:
        return true
      inc nested
      inc n
    of Symbol:
      if c.typeCache.fetchSymKind(n.symId) in RuntimeVarKinds:
        return true  # runtime variable → value not available at C file scope
      inc n
    else:
      inc n
  result = false

proc trToplevel(c: var EContext; dest: var TokenBuf; n: var Cursor) =
  inc n
  while n.kind != ParRi:
    let sk = n.stmtKind
    if sk in {GvarS, GletS, TvarS, TletS}:
      let tag = if sk in {TvarS, TletS}: TvarY else: GvarY
      if not initHasCall(c, n):
        # Simple init (literal, nil, etc.): keep at top level.
        # NIFC can emit "Type var = value;" at C file scope directly.
        trLocal c, dest, n, tag, TraverseAll
      else:
        # Complex init with function calls: emit a no-init declaration at top
        # level and place the actual init as an assignment inside the Init proc
        # body so that any temp variables created by to_stmts remain in scope.
        let savedN = n
        trLocal c, dest, n, tag, TraverseSig
        var initN = savedN
        inc initN  # past gvar/glet tag -> at SymbolDef
        let (initSym, initInfo) = getSymDef(c, initN)
        skipExportMarker c, initN
        skip initN  # past pragmas -> at type
        skip initN  # past type -> at init value
        swap dest, c.initBody
        dest.addParLe AsgnS, initInfo
        dest.add symToken(initSym, initInfo)
        trExpr c, dest, initN
        dest.addParRi()
        swap dest, c.initBody
    elif sk == StmtsS:
      # Nested stmts block: recurse to handle mixed decls and executable code
      trToplevel c, dest, n
      skipParRi c, n
    elif isTopLevelDecl(n):
      # Pure declarations and compile-time constructs stay at top level:
      trStmt c, dest, n, TraverseTopLevel
    else:
      # Executable code and local vars go into the init proc body:
      swap dest, c.initBody
      trStmt c, dest, n, TraverseAll
      swap dest, c.initBody

proc expand*(infile: string; bits: int; bigEndian: bool; flags: set[CheckMode]; isMain: bool; outdir: string; appType = appConsole) =
  let mp = splitModulePath(infile)
  let dir = if outdir.len > 0: outdir elif mp.dir.len == 0: getCurrentDir() else: mp.dir
  var c = EContext(dir: dir, ext: mp.ext, main: mp.name,
    nestedIn: @[(StmtsS, SymId(0))],
    typeCache: createTypeCache(),
    pending: createTokenBuf(),
    strLitBuf: createTokenBuf(),
    bits: bits,
    bigEndian: bigEndian,
    localDeclCounters: 1000,
    activeChecks: flags,
    liftingCtx: createLiftingCtx(mp.name, bits)
  )
  c.typeCache.openScope()

  var owningBuf = createTokenBuf(300)

  var c0 = setupProgram(infile, infile.changeModuleExt ".x.nif", owningBuf, true)
  var dest = transform(c, c0, mp.name, c.bits)

  var n = beginRead(dest)
  let rootInfo = n.info

  var toplevels = createTokenBuf()
  c.initBody = createTokenBuf()
  var cdest = createTokenBuf(300)
  swap cdest, toplevels
  if stmtKind(n) == StmtsS:
    trToplevel c, cdest, n
  else:
    error c, "expected (stmts) but got: ", n
  swap cdest, toplevels

  initDynlib(c, cdest, rootInfo)

  when sso:
    cdest.add c.strLitBuf

  cdest.add toplevels
  cdest.add c.pending

  # Generate the init proc after all other code so NIFC places it last
  # in the C file, after all function definitions it may call.
  genInitProc(c, cdest, rootInfo, c.importedModuleSuffixes)
  cdest.add c.initBody
  genInitProcEnd(c, cdest, rootInfo)

  if isMain and appType in {appConsole, appGui}:
    genMainProc(c, cdest, rootInfo)

  skipParRi c, n
  let destfileName = c.dir / c.main & ".x.nif"

  var outputBuf = writeOutput(c, cdest, rootInfo, destfileName)
  c.typeCache.closeScope()

  # Use the in-memory buffer to avoid re-reading the file we just wrote
  writeDceOutput outputBuf, c.dir / c.main & ".dce.nif", "." & c.main
