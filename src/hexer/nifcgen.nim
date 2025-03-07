#
#
#           Hexer Compiler
#        (c) Copyright 2024 Andreas Rumpf
#
#    See the file "copying.txt", included in this
#    distribution, for details about the copyright.
#

import std / [hashes, os, tables, sets, assertions]

include nifprelude
import typekeys
import ".." / nimony / [nimony_model, programs, typenav, expreval, xints, decls, builtintypes, sizeof, typeprops]
from ".." / nimony / sigmatch import isSomeStringType, isStringType
import basics, pipeline
import  ".." / lib / stringtrees


proc setOwner(ctx: var EContext; newOwner: SymId): SymId =
  result = ctx.currentOwner
  ctx.currentOwner = newOwner

proc demand(ctx: var EContext; s: SymId) =
  if not ctx.declared.contains(s):
    ctx.requires.add s

proc offer(ctx: var EContext; s: SymId) =
  ctx.declared.incl s


proc skipExportMarker(ctx: var EContext; n: var Cursor) =
  if n.kind == DotToken:
    inc n
  elif n.kind == Ident and pool.strings[n.litId] == "x":
    inc n
  elif n.kind == ParLe:
    # can now also be `(tag)` or `(tag <bits>)`:
    skip n
  else:
    error ctx, "expected '.' or 'x' for an export marker: ", n

proc expectSymdef(ctx: var EContext; n: var Cursor) =
  if n.kind != SymbolDef:
    error ctx, "expected symbol definition, but got: ", n

proc getSymDef(ctx: var EContext; n: var Cursor): (SymId, PackedLineInfo) =
  expectSymdef(ctx, n)
  result = (n.symId, n.info)
  inc n

proc expectSym(ctx: var EContext; n: var Cursor) =
  if n.kind != Symbol:
    error ctx, "expected symbol, but got: ", n

proc getSym(ctx: var EContext; n: var Cursor): (SymId, PackedLineInfo) =
  expectSym(ctx, n)
  result = (n.symId, n.info)
  inc n

proc expectStrLit(ctx: var EContext; n: var Cursor) =
  if n.kind != StringLit:
    error ctx, "expected string literal, but got: ", n

proc expectIntLit(ctx: var EContext; n: var Cursor) =
  if n.kind != IntLit:
    error ctx, "expected int literal, but got: ", n



proc add(ctx: var EContext; tag: string; info: PackedLineInfo) =
  ctx.dest.add tagToken(tag, info)

type
  TraverseMode = enum
    TraverseAll, TraverseSig, TraverseTopLevel

proc traverseExpr(ctx: var EContext; n: var Cursor)
proc traverseStmt(ctx: var EContext; n: var Cursor; mode = TraverseAll)
proc traverseLocal(ctx: var EContext; n: var Cursor; tag: SymKind; mode: TraverseMode)

type
  TypeFlag = enum
    IsTypeBody
    IsPointerOf
    IsNodecl

proc traverseType(ctx: var EContext; n: var Cursor; flags: set[TypeFlag] = {})

type
  CollectedPragmas = object
    externName: string
    flags: set[PragmaKind]
    align, bits: IntId
    header: StrId
    callConv: CallConv

proc parsePragmas(ctx: var EContext; n: var Cursor): CollectedPragmas

proc traverseField(ctx: var EContext; n: var Cursor; flags: set[TypeFlag] = {}) =
  ctx.dest.add n # fld
  inc n

  expectSymdef(ctx, n)
  let (s, sinfo) = getSymDef(ctx, n)
  ctx.dest.add symdefToken(s, sinfo)
  ctx.offer s

  skipExportMarker ctx, n

  let prag = parsePragmas(ctx, n)

  ctx.dest.addDotToken() # adds pragmas

  if prag.externName.len > 0:
    ctx.registerMangle(s, prag.externName & ".c")

  traverseType ctx, n, flags

  skip n # skips value
  takeParRi ctx, n

proc ithTupleField(counter: int): SymId {.inline.} =
  pool.syms.getOrIncl("fld." & $counter)

proc genTupleField(ctx: var EContext; typ: var Cursor; counter: int) =
  ctx.dest.add tagToken("fld", typ.info)
  let name = ithTupleField(counter)
  ctx.dest.add symdefToken(name, typ.info)
  ctx.offer name
  ctx.dest.addDotToken() # pragmas
  ctx.traverseType(typ, {})
  ctx.dest.addParRi() # "fld"

proc traverseEnumField(ctx: var EContext; n: var Cursor; flags: set[TypeFlag] = {}) =
  ctx.dest.add n # efld
  inc n

  expectSymdef(ctx, n)
  let (s, sinfo) = getSymDef(ctx, n)
  ctx.dest.add symdefToken(s, sinfo)
  ctx.offer s

  skipExportMarker ctx, n

  skip n # pragmas: must be empty

  skip n # type: must be the enum itself

  inc n # skips TupleConstr
  traverseExpr ctx, n
  skip n
  skipParRi ctx, n

  takeParRi ctx, n

proc genStringType(ctx: var EContext; info: PackedLineInfo) =
  let s = pool.syms.getOrIncl(StringName)
  ctx.dest.add tagToken("type", info)
  ctx.dest.add symdefToken(s, info)
  ctx.offer s

  ctx.dest.addDotToken()
  ctx.dest.add tagToken("object", info)
  ctx.dest.addDotToken()

  ctx.dest.add tagToken("fld", info)
  let strField = pool.syms.getOrIncl(StringAField)
  ctx.dest.add symdefToken(strField, info)
  ctx.offer strField
  ctx.dest.addDotToken()
  ctx.dest.add tagToken("ptr", info)
  ctx.dest.add tagToken("c", info)
  ctx.dest.addIntLit(8, info)
  ctx.dest.addParRi() # "c"
  ctx.dest.addParRi() # "ptr"
  ctx.dest.addParRi() # "fld"

  ctx.dest.add tagToken("fld", info)
  let lenField = pool.syms.getOrIncl(StringIField)
  ctx.dest.add symdefToken(lenField, info)
  ctx.offer lenField
  ctx.dest.addDotToken()
  ctx.dest.add tagToken("i", info)
  ctx.dest.addIntLit(-1, info)
  ctx.dest.addParRi() # "i"
  ctx.dest.addParRi() # "fld"

  ctx.dest.addParRi() # "object"
  ctx.dest.addParRi() # "type"

proc useStringType(ctx: var EContext; info: PackedLineInfo) =
  let s = pool.syms.getOrIncl(StringName)
  ctx.dest.add symToken(s, info)
  ctx.demand s

proc traverseTupleBody(ctx: var EContext; n: var Cursor) =
  let info = n.info
  inc n
  ctx.dest.add tagToken("object", info)
  ctx.dest.addDotToken()
  var counter = 0
  while n.kind != ParRi:
    if n.substructureKind == KvU:
      inc n # skip tag
      skip n # skip name
      genTupleField(ctx, n, counter)
      skipParRi ctx, n
    else:
      genTupleField(ctx, n, counter)
    inc counter
  takeParRi ctx, n

proc traverseArrayBody(ctx: var EContext; n: var Cursor) =
  ctx.dest.add n
  inc n
  traverseType ctx, n
  if n.typeKind == RangetypeT:
    inc n
    skip n
    expectIntLit ctx, n
    let first = pool.integers[n.intId]
    inc n
    expectIntLit ctx, n
    let last = pool.integers[n.intId]
    inc n
    skipParRi ctx, n
    ctx.dest.addIntLit(last - first + 1, n.info)
  else:
    # should not be possible, but assume length anyway
    traverseExpr ctx, n
  takeParRi ctx, n

type
  GenPragmas = object
    opened: bool

proc openGenPragmas(): GenPragmas = GenPragmas(opened: false)

proc maybeOpen(ctx: var EContext; g: var GenPragmas; info: PackedLineInfo) {.inline.} =
  if not g.opened:
    g.opened = true
    ctx.dest.add tagToken("pragmas", info)

proc addKey(ctx: var EContext; g: var GenPragmas; key: string; info: PackedLineInfo) =
  maybeOpen ctx, g, info
  ctx.dest.add tagToken(key, info)
  ctx.dest.addParRi()

proc addKeyVal(ctx: var EContext; g: var GenPragmas; key: string; val: PackedToken; info: PackedLineInfo) =
  maybeOpen ctx, g, info
  ctx.dest.add tagToken(key, info)
  ctx.dest.add val
  ctx.dest.addParRi()

proc closeGenPragmas(ctx: var EContext; g: GenPragmas) =
  if g.opened:
    ctx.dest.addParRi()
  else:
    ctx.dest.addDotToken()

proc traverseParams(ctx: var EContext; n: var Cursor)

proc traverseProcTypeBody(ctx: var EContext; n: var Cursor) =
  ctx.dest.add tagToken("proctype", n.info)
  # This is really stupid...
  ctx.dest.addDotToken() # name
  inc n # proc
  # name, export marker, pattern, type vars:
  for i in 0..<4: skip n
  traverseParams ctx, n

  let pinfo = n.info
  let prag = parsePragmas(ctx, n)
  var genPragmas = openGenPragmas()
  if prag.callConv != NoCallConv:
    let name = $prag.callConv
    ctx.addKey genPragmas, name, pinfo
  closeGenPragmas ctx, genPragmas

  # ignore, effects and body:
  skip n
  skip n
  takeParRi ctx, n

proc traverseRefBody(ctx: var EContext; n: var Cursor; key: string) =
  # We translate `ref T` to:
  # ptr OuterT;
  # OuterT = object
  #  r: int
  #  d: T
  # This means `deref x` becomes `x->d` and `x.field` becomes `x->d.field`
  # `cast` must also be adjusted by the offset of `d` within `OuterT` but this seems
  # to be optional.

  #let dataStructName = pool.syms.getOrIncl(key & ".1.t")

  let info = n.info
  inc n
  ctx.dest.add tagToken("object", info)
  ctx.dest.addDotToken()

  ctx.dest.add tagToken("fld", info)
  let rcField = pool.syms.getOrIncl(RcField)
  ctx.dest.add symdefToken(rcField, info)
  ctx.offer rcField
  ctx.dest.addDotToken() # pragmas
  ctx.dest.add tagToken("i", info)
  ctx.dest.addIntLit(-1, info)
  ctx.dest.addParRi() # "i"
  ctx.dest.addParRi() # "fld"

  let dataField = pool.syms.getOrIncl(DataField)
  ctx.dest.add tagToken("fld", info)
  ctx.dest.add symdefToken(dataField, info)
  ctx.offer dataField
  ctx.dest.addDotToken() # pragmas
  ctx.traverseType(n, {})
  ctx.dest.addParRi() # "fld"

  ctx.dest.addParRi() # "object"

proc traverseAsNamedType(ctx: var EContext; n: var Cursor) =
  let info = n.info
  var body = n
  let k = body.typeKind
  let key = takeMangle n

  var val = ctx.newTypes.getOrDefault(key)
  if val == SymId(0):
    val = pool.syms.getOrIncl(key & GeneratedTypeSuffix)
    ctx.newTypes[key] = val

    var buf = createTokenBuf(30)
    swap ctx.dest, buf

    ctx.dest.add tagToken("type", info)
    ctx.dest.add symdefToken(val, info)
    ctx.offer val

    ctx.dest.addDotToken()
    case k
    of TupleT:
      traverseTupleBody ctx, body
    of ArrayT:
      traverseArrayBody ctx, body
    of ProctypeT:
      traverseProcTypeBody ctx, body
    of RefT:
      traverseRefBody ctx, body, key
    else:
      error ctx, "expected tuple or array, but got: ", body
    ctx.dest.addParRi() # "type"

    swap ctx.dest, buf
    ctx.pending.add buf
  # regardless of what we had to do, we still need to add the typename:
  if k == RefT:
    ctx.dest.add tagToken("ptr", info)
    ctx.dest.add symToken(val, info)
    ctx.dest.addParRi()
  else:
    ctx.dest.add symToken(val, info)

proc traverseType(ctx: var EContext; n: var Cursor; flags: set[TypeFlag] = {}) =
  case n.kind
  of DotToken:
    ctx.dest.add n
    inc n
  of Symbol:
    let s = n.symId
    let ext = maybeMangle(ctx, s)
    if ext.len != 0:
      ctx.dest.addSymUse pool.syms.getOrIncl(ext), n.info
      inc n
      return
    let res = tryLoadSym(s)
    if res.status == LacksNothing:
      var body = asTypeDecl(res.decl).body
      if body.typeKind == DistinctT: # skips DistinctT
        inc body
        traverseType(ctx, body, flags)
        inc n
      else:
        ctx.demand s
        ctx.dest.add n
        inc n
    else:
      error ctx, "could not find symbol: " & pool.syms[s]
  of ParLe:
    case n.typeKind
    of NoType, ErrT, OrT, AndT, NotT, TypedescT, UntypedT, TypedT, TypeKindT, OrdinalT:
      error ctx, "type expected but got: ", n
    of IntT, UIntT:
      let start = ctx.dest.len
      ctx.dest.add n
      inc n
      ctx.dest.add n
      inc n
      if n.kind != ParRi and n.pragmaKind in {ImportcP, ImportcppP}:
        ctx.dest.shrink start
        inc n
        ctx.dest.addSymUse pool.syms.getOrIncl(pool.strings[n.litId] & ".c"), n.info
        inc n
        skipParRi ctx, n
        skipParRi ctx, n
      else:
        takeParRi ctx, n
    of FloatT, CharT, BoolT, AutoT, SymKindT:
      ctx.loop n:
        ctx.dest.add n
        inc n
    of MutT, LentT:
      ctx.dest.add tagToken("ptr", n.info)
      inc n
      if isViewType(n):
        ctx.dest.shrink ctx.dest.len-1 # remove the "ptr" again
        traverseType ctx, n, {}
        skipParRi n
      else:
        ctx.loop n:
          traverseType ctx, n, {IsPointerOf}
    of PtrT, OutT:
      ctx.dest.add tagToken("ptr", n.info)
      inc n
      ctx.loop n:
        traverseType ctx, n, {IsPointerOf}
    of RefT:
      traverseAsNamedType ctx, n
    of ArrayT, ProctypeT:
      if IsNodecl in flags:
        traverseArrayBody ctx, n
      else:
        traverseAsNamedType ctx, n
    of RangetypeT:
      # skip to base type
      inc n
      traverseType ctx, n
      skip n
      skip n
      skipParRi ctx, n
    of UarrayT:
      if IsPointerOf in flags:
        inc n
        traverseType ctx, n
        skipParRi ctx, n
      else:
        ctx.dest.add tagToken("flexarray", n.info)
        inc n
        traverseType ctx, n
        takeParRi ctx, n
    of PointerT:
      ctx.dest.add tagToken("ptr", n.info)
      ctx.dest.add tagToken("void", n.info)
      ctx.dest.addParRi()
      inc n
      takeParRi ctx, n
    of CstringT:
      ctx.dest.add tagToken("ptr", n.info)
      ctx.dest.add tagToken($CharT, n.info)
      ctx.dest.addIntLit(8, n.info)
      ctx.dest.addParRi()
      inc n
      takeParRi ctx, n
    of StaticT, SinkT, DistinctT:
      inc n
      traverseType ctx, n, flags
      skipParRi ctx, n
    of TupleT:
      traverseAsNamedType ctx, n
    of ObjectT:
      ctx.dest.add n
      inc n
      if n.kind == DotToken:
        ctx.dest.add n
        inc n
      else:
        # inherited symbol
        let (s, sinfo) = getSym(ctx, n)
        ctx.dest.add symToken(s, sinfo)
        ctx.demand s

      if n.kind == DotToken:
        ctx.dest.add n
        inc n
      else:
        while n.substructureKind == FldU:
          traverseField(ctx, n, flags)

      takeParRi ctx, n
    of EnumT, HoleyEnumT:
      ctx.dest.add tagToken("enum", n.info)
      inc n
      traverseType ctx, n, flags # base type

      while n.substructureKind == EfldU:
        traverseEnumField(ctx, n, flags)

      takeParRi ctx, n
    of SetT:
      let info = n.info
      inc n
      let sizeOrig = bitsetSizeInBytes(n)
      var err = false
      let size = asSigned(sizeOrig, err)
      if err:
        error ctx, "invalid set element type: ", n
      else:
        case size
        of 1, 2, 4, 8:
          ctx.dest.add tagToken("u", info)
          ctx.dest.addIntLit(size * 8, info)
          ctx.dest.addParRi()
        else:
          var arrBuf = createTokenBuf(16)
          arrBuf.add tagToken("array", info)
          arrBuf.add tagToken("u", info)
          arrBuf.addIntLit(8, info)
          arrBuf.addParRi()
          arrBuf.addIntLit(size, info)
          arrBuf.addParRi()
          var arrCursor = cursorAt(arrBuf, 0)
          traverseAsNamedType(ctx, arrCursor)
      skip n
      skipParRi ctx, n
    of VoidT, VarargsT, NiltT, ConceptT,
       IteratorT, InvokeT, ParamsT, ItertypeT:
      error ctx, "unimplemented type: ", n
  else:
    error ctx, "type expected but got: ", n

proc maybeByConstRef(ctx: var EContext; n: var Cursor) =
  let param = asLocal(n)
  if param.typ.typeKind in {TypedescT, StaticT}:
    # do not produce any code for this as it's a compile-time parameter
    skip n
  elif passByConstRef(param.typ, param.pragmas, ctx.bits div 8):
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
    traverseLocal(ctx, paramCursor, ParamY, TraverseSig)
    endRead(paramBuf)
    skip n
  else:
    traverseLocal(ctx, n, ParamY, TraverseSig)

proc traverseParams(ctx: var EContext; n: var Cursor) =
  if n.kind == DotToken:
    ctx.dest.add n
    inc n
  elif n.kind == ParLe and n.typeKind == ParamsT:
    ctx.dest.add n
    inc n
    loop ctx, n:
      if n.symKind != ParamY:
        error ctx, "expected (param) but got: ", n
      maybeByConstRef(ctx, n)
  else:
    error ctx, "expected (params) but got: ", n
  # the result type
  traverseType ctx, n

proc parsePragmas(ctx: var EContext; n: var Cursor): CollectedPragmas =
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
        error ctx, "expected ')', but EOF reached"
      else: discard
      if n.kind == ParLe:
        let pk = n.pragmaKind
        case pk
        of NoPragma:
          let cc = n.callConvKind
          if cc == NoCallConv:
            error ctx, "unknown pragma: ", n
          else:
            result.callConv = cc
          inc n
        of MagicP:
          inc n
          if n.kind notin {StringLit, Ident}:
            error ctx, "expected string literal or ident, but got: ", n
          result.flags.incl NodeclP
          inc n
        of ImportcP, ImportcppP:
          inc n
          expectStrLit ctx, n
          result.externName = pool.strings[n.litId]
          result.flags.incl pk
          inc n
        of ExportcP, PluginP:
          inc n
          expectStrLit ctx, n
          result.externName = pool.strings[n.litId]
          inc n
        of NodeclP, SelectanyP, ThreadvarP, GlobalP, DiscardableP, NoReturnP,
           VarargsP, BorrowP, NoSideEffectP, NoDestroyP, ByCopyP, ByRefP,
           InlineP, NoinlineP, NoInitP, InjectP, GensymP, UntypedP, ViewP:
          result.flags.incl pk
          inc n
        of HeaderP:
          inc n
          expectStrLit ctx, n
          result.header = n.litId
          result.flags.incl NodeclP
          inc n
        of AlignP:
          inc n
          expectIntLit ctx, n
          result.align = n.intId
          inc n
        of BitsP:
          inc n
          expectIntLit ctx, n
          result.bits = n.intId
          inc n
        of RequiresP, EnsuresP, StringP, RaisesP, ErrorP:
          skip n
          continue
        of BuildP, EmitP:
          raiseAssert "unreachable"
        skipParRi ctx, n
      else:
        error ctx, "unknown pragma: ", n
  else:
    error ctx, "(pragmas) or '.' expected, but got: ", n

proc traverseProcBody(ctx: var EContext; n: var Cursor) =
  if n.stmtKind == StmtsS:
    ctx.dest.add n
    inc n
    var prevStmt = NoStmt
    while n.kind != ParRi:
      prevStmt = n.stmtKind
      traverseStmt ctx, n, TraverseAll
    if prevStmt == RetS or ctx.resultSym == SymId(0):
      discard "ok, do not add another return"
    else:
      ctx.dest.add parLeToken(RetS, n.info)
      ctx.dest.add symToken(ctx.resultSym, n.info)
      ctx.dest.addParRi()
    takeParRi ctx, n
  else:
    traverseStmt ctx, n, TraverseAll

proc traverseProc(ctx: var EContext; n: var Cursor; mode: TraverseMode) =
  ctx.openMangleScope()
  var dst = createTokenBuf(50)
  swap ctx.dest, dst
  #let toPatch = ctx.dest.len
  let oldResultSym = ctx.resultSym
  ctx.resultSym = SymId(0)

  let vinfo = n.info
  ctx.add "proc", vinfo
  inc n
  let (s, sinfo) = getSymDef(ctx, n)

  # namePos
  ctx.dest.add symdefToken(s, sinfo)
  ctx.offer s

  var isGeneric = false
  if n.kind == ParLe:
    isGeneric = true
  skipExportMarker ctx, n

  skip n # patterns

  if n.substructureKind == TypevarsU:
    isGeneric = true
    # count each typevar as used:
    inc n
    while n.kind != ParRi:
      assert n.symKind == TypevarY
      inc n
      let (typevar, _) = getSymDef(ctx, n)
      ctx.offer typevar
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
      let (param, _) = getSymDef(ctx, n)
      ctx.offer param
      skipToEnd n
    inc n
    skip n # skip return type
  else:
    traverseParams ctx, n

  let pinfo = n.info
  let prag = parsePragmas(ctx, n)

  let oldOwner = setOwner(ctx, s)

  var genPragmas = openGenPragmas()
  if prag.callConv != NoCallConv:
    let name = $prag.callConv
    ctx.addKey genPragmas, name, pinfo
  if InlineP in prag.flags:
    ctx.addKey genPragmas, "inline", pinfo

  if prag.externName.len > 0:
    ctx.registerMangleInParent(s, prag.externName & ".c")
    ctx.addKeyVal genPragmas, "was", symToken(s, pinfo), pinfo
  if SelectanyP in prag.flags:
    ctx.addKey genPragmas, "selectany", pinfo

  if BorrowP in prag.flags:
    ctx.addKey genPragmas, $InlineP, pinfo
  closeGenPragmas ctx, genPragmas

  skip n # miscPos

  # body:
  if isGeneric:
    skip n
  elif mode != TraverseSig or InlineP in prag.flags:
    traverseProcBody ctx, n
  else:
    ctx.dest.addDotToken()
    skip n
  takeParRi ctx, n
  swap dst, ctx.dest
  if NodeclP in prag.flags or isGeneric:
    discard "do not add to ctx.dest"
  elif prag.flags * {ImportcP, ImportcppP} != {} and ctx.inImpSection == 0:
    ctx.dest.add tagToken("imp", n.info)
    ctx.dest.add dst
    ctx.dest.addParRi()
  else:
    ctx.dest.add dst
  if prag.header != StrId(0):
    ctx.headers.incl prag.header
  discard setOwner(ctx, oldOwner)
  ctx.closeMangleScope()
  ctx.resultSym = oldResultSym

proc traverseTypeDecl(ctx: var EContext; n: var Cursor) =
  var dst = createTokenBuf(50)
  swap ctx.dest, dst
  #let toPatch = ctx.dest.len
  let vinfo = n.info
  ctx.add "type", vinfo
  inc n
  let (s, sinfo) = getSymDef(ctx, n)
  let oldOwner = setOwner(ctx, s)

  ctx.dest.add symdefToken(s, sinfo)
  ctx.offer s

  var isGeneric = n.kind == ParLe
  skipExportMarker ctx, n
  if n.substructureKind == TypevarsU:
    isGeneric = true
    # count each typevar as used:
    inc n
    while n.kind != ParRi:
      assert n.symKind == TypevarY
      inc n
      let (typevar, _) = getSymDef(ctx, n)
      ctx.offer typevar
      skipToEnd n
    inc n
  else:
    skip n # generic parameters

  let prag = parsePragmas(ctx, n)

  ctx.dest.addDotToken() # adds pragmas

  if prag.externName.len > 0:
    ctx.registerMangle(s, prag.externName & ".c")
  if n.typeKind in TypeclassKinds:
    isGeneric = true
  if isGeneric:
    skip n
  else:
    traverseType ctx, n, {IsTypeBody} + (if NodeclP in prag.flags: {IsNodecl} else: {})
  takeParRi ctx, n
  swap dst, ctx.dest
  if NodeclP in prag.flags or isGeneric:
    discard "do not add to ctx.dest"
  else:
    ctx.dest.add dst
  if prag.header != StrId(0):
    ctx.headers.incl prag.header
  discard setOwner(ctx, oldOwner)

proc genStringLit(ctx: var EContext; s: string; info: PackedLineInfo) =
  when false:
    # cannot use this logic because C is stupid crap.
    let existing = ctx.strLits.getOrDefault(s)
    if existing != SymId(0):
      ctx.dest.add symToken(existing, info)
    else:
      let strName = pool.syms.getOrIncl("str`." & $ctx.strLits.len)
      ctx.strLits[s] = strName
      ctx.pending.add tagToken("const", info)
      ctx.pending.add symdefToken(strName, info)
      ctx.offer strName

      ctx.pending.add tagToken("pragmas", info)
      ctx.pending.add tagToken("static", info)
      ctx.pending.addParRi()
      ctx.pending.addParRi()

      # type:
      ctx.pending.add symToken(pool.syms.getOrIncl(StringName), info)
      # value:
      ctx.pending.add tagToken("oconstr", info)
      ctx.pending.add symToken(pool.syms.getOrIncl(StringName), info)

      ctx.pending.add parLeToken(KvU, info)
      let strField = pool.syms.getOrIncl(StringAField)
      ctx.pending.add symToken(strField, info)
      ctx.pending.addStrLit(s)
      ctx.pending.addParRi() # "kv"

      ctx.pending.add parLeToken(KvU, info)
      let lenField = pool.syms.getOrIncl(StringIField)
      ctx.pending.add symToken(lenField, info)
      # length also contains the "isConst" flag:
      ctx.pending.addIntLit(s.len * 2, info)
      ctx.pending.addParRi() # "kv"

      ctx.pending.addParRi() # "oconstr"
      ctx.pending.addParRi() # "const"
      ctx.dest.add symToken(strName, info)
  else:
    ctx.dest.add tagToken("oconstr", info)
    useStringType ctx, info

    ctx.dest.add parLeToken(KvU, info)
    let strField = pool.syms.getOrIncl(StringAField)
    ctx.dest.add symToken(strField, info)
    ctx.dest.addStrLit(s)
    ctx.dest.addParRi() # "kv"

    ctx.dest.add parLeToken(KvU, info)
    let lenField = pool.syms.getOrIncl(StringIField)
    ctx.dest.add symToken(lenField, info)
    # length also contains the "isConst" flag:
    ctx.dest.addIntLit(s.len * 2, info)
    ctx.dest.addParRi() # "kv"

    ctx.dest.addParRi() # "oconstr"

proc genStringLit(ctx: var EContext; n: Cursor) =
  assert n.kind == StringLit
  let info = n.info
  let s {.cursor.} = pool.strings[n.litId]
  genStringLit(ctx, s, info)

proc traverseStmtsExpr(ctx: var EContext; n: var Cursor) =
  let head = n.load()
  inc n
  if isLastSon(n):
    traverseExpr ctx, n
    skipParRi ctx, n
  else:
    ctx.dest.add head
    while n.kind != ParRi:
      if not isLastSon(n):
        traverseStmt ctx, n
      else:
        traverseExpr ctx, n
    takeParRi ctx, n

proc traverseTupleConstr(ctx: var EContext; n: var Cursor) =
  ctx.dest.add tagToken("oconstr", n.info)
  var tupleType = ctx.typeCache.getType(n)
  ctx.traverseType(tupleType, {})
  inc n
  var counter = 0
  while n.kind != ParRi:
    ctx.dest.add tagToken("kv", n.info)
    ctx.dest.add symToken(ithTupleField(counter), n.info)
    inc counter
    if n.substructureKind == KvU:
      inc n # skip "kv"
      skip n # skip key
      traverseExpr ctx, n
      skipParRi ctx, n
    else:
      traverseExpr ctx, n
    ctx.dest.addParRi() # "kv"
  takeParRi ctx, n

proc traverseConv(ctx: var EContext; n: var Cursor) =
  let info = n.info
  let beforeConv = ctx.dest.len
  ctx.dest.add tagToken("conv", info)
  inc n
  let destType = n
  traverseType(ctx, n)
  let srcType = getType(ctx.typeCache, n)
  if destType.typeKind == CstringT and isStringType(srcType):
    var isSuffix = false
    if n.exprKind == SufX:
      isSuffix = true
      inc n
    if n.kind == StringLit:
      # evaluate the conversion at compile time:
      ctx.dest.shrink beforeConv
      ctx.dest.addStrLit pool.strings[n.litId]
      inc n
      if isSuffix:
        inc n
        skipParRi ctx, n
      skipParRi ctx, n
    else:
      let strField = pool.syms.getOrIncl(StringAField)
      ctx.dest.add tagToken("dot", info)
      traverseExpr(ctx, n)
      ctx.dest.add symToken(strField, info)
      ctx.dest.addIntLit(0, info)
      ctx.dest.addParRi()
      takeParRi ctx, n
  else:
    traverseExpr(ctx, n)
    takeParRi ctx, n

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

proc traverseExpr(ctx: var EContext; n: var Cursor) =
  case n.kind
  of EofToken, ParRi:
    error ctx, "BUG: unexpected ')' or EofToken"
  of ParLe:
    case n.exprKind
    of EqX, NeqX, LeX, LtX:
      ctx.dest.add n
      inc n
      let beforeType = ctx.dest.len
      traverseType(ctx, n)
      ctx.dest.shrink beforeType
      traverseExpr(ctx, n)
      traverseExpr(ctx, n)
      takeParRi ctx, n
    of CastX:
      ctx.dest.add n
      inc n
      traverseType(ctx, n)
      traverseExpr(ctx, n)
      takeParRi ctx, n
    of HconvX, ConvX:
      traverseConv ctx, n
    of DconvX:
      inc n
      let beforeType = ctx.dest.len
      traverseType(ctx, n)
      ctx.dest.shrink beforeType
      traverseExpr(ctx, n)
      skipParRi(ctx, n)
    of AconstrX:
      ctx.dest.add tagToken("aconstr", n.info)
      inc n
      traverseType(ctx, n)
      while n.kind != ParRi:
        traverseExpr(ctx, n)
      takeParRi ctx, n
    of OconstrX:
      ctx.dest.add tagToken("oconstr", n.info)
      inc n
      traverseType(ctx, n)
      while n.kind != ParRi:
        if n.substructureKind == KvU:
          ctx.dest.add n # KvU
          inc n
          takeTree ctx, n # key
          traverseExpr ctx, n # value
          takeParRi ctx, n
        else:
          traverseExpr ctx, n
      takeParRi ctx, n
    of TupX:
      traverseTupleConstr ctx, n
    of CmdX, CallStrLitX, InfixX, PrefixX, HcallX, CallX:
      ctx.dest.add tagToken("call", n.info)
      inc n
      while n.kind != ParRi:
        traverseExpr(ctx, n)
      takeParRi ctx, n
    of ExprX:
      traverseStmtsExpr ctx, n
    of ArrAtX:
      # XXX does not handle index type with offset low(I), maybe should be done in sem
      ctx.dest.add tagToken("at", n.info)
      inc n
      traverseExpr(ctx, n)
      traverseExpr(ctx, n)
      takeParRi ctx, n
    of TupAtX:
      ctx.dest.add tagToken("dot", n.info)
      inc n # skip tag
      traverseExpr ctx, n # tuple
      expectIntLit ctx, n
      ctx.dest.add symToken(ithTupleField(pool.integers[n.intId]), n.info)
      inc n # skip index
      ctx.dest.addIntLit(0, n.info) # inheritance
      takeParRi ctx, n
    of DotX:
      ctx.dest.add tagToken("dot", n.info)
      inc n # skip tag
      traverseExpr ctx, n # obj
      traverseExpr ctx, n # field
      traverseExpr ctx, n # inheritance depth
      takeParRi ctx, n
    of DdotX:
      ctx.dest.add tagToken("dot", n.info)
      ctx.dest.add tagToken("deref", n.info)
      inc n # skip tag
      traverseExpr ctx, n
      ctx.dest.addParRi()
      traverseExpr ctx, n
      traverseExpr ctx, n
      takeParRi ctx, n
    of HaddrX, AddrX:
      ctx.dest.add tagToken("addr", n.info)
      inc n
      traverseExpr(ctx, n)
      takeParRi ctx, n
    of HderefX, DerefX:
      ctx.dest.add tagToken("deref", n.info)
      inc n
      traverseExpr(ctx, n)
      takeParRi ctx, n
    of SufX:
      var suf = n
      inc suf
      let arg = suf
      skip suf
      assert suf.kind == StringLit
      if arg.kind == StringLit and pool.strings[suf.litId] == "R":
        # cstring conversion
        inc n
        ctx.dest.add n # add string lit directly
        inc n # arg
        inc n # suf
      else:
        ctx.dest.add n
        inc n
        traverseExpr ctx, n
        ctx.dest.add n
        inc n
      takeParRi ctx, n
    of AshrX:
      ctx.dest.add tagToken("shr", n.info)
      inc n
      var bits = -1'i64
      if n.typeKind in {IntT, UIntT}:
        var bitsToken = n
        inc bitsToken
        bits = pool.integers[bitsToken.intId]
      else:
        #error ctx, "expected int/uint type for ashr, got: ", n
        discard
      traverseType(ctx, n)
      ctx.dest.copyIntoKind CastX, n.info:
        ctx.dest.add tagToken("i", n.info)
        ctx.dest.addIntLit(bits, n.info)
        ctx.dest.addParRi()
        traverseExpr ctx, n
      ctx.dest.copyIntoKind CastX, n.info:
        ctx.dest.add tagToken("u", n.info)
        ctx.dest.addIntLit(bits, n.info)
        ctx.dest.addParRi()
        traverseExpr ctx, n
      takeParRi ctx, n
    of ErrX, NewobjX, SetConstrX, PlusSetX, MinusSetX, MulSetX, XorSetX, EqSetX, LeSetX, LtSetX,
       InSetX, CardX, BracketX, CurlyX, CompilesX, DeclaredX, DefinedX, HighX, LowX, TypeofX, UnpackX,
       EnumtostrX, IsmainmoduleX, DefaultobjX, DefaulttupX, DoX, CchoiceX, OchoiceX,
       EmoveX, DestroyX, DupX, CopyX, WasmovedX, SinkhX, TraceX, CurlyatX, PragmaxX, QuotedX, TabconstrX:
      error ctx, "BUG: not eliminated: ", n
      #skip n
    of AtX, PatX, ParX, NilX, InfX, NeginfX, NanX, FalseX, TrueX, AndX, OrX, NotX, NegX,
       SizeofX, AlignofX, OffsetofX, AddX, SubX, MulX, DivX, ModX, ShrX, ShlX,
       BitandX, BitorX, BitxorX, BitnotX, OconvX:
      ctx.dest.add n
      inc n
      while n.kind != ParRi:
        traverseExpr ctx, n
      takeParRi ctx, n
    of NoExpr:
      traverseType ctx, n
  of SymbolDef:
    ctx.dest.add n
    ctx.offer n.symId
    inc n
  of Symbol:
    let inlineValue = getInitValue(ctx.typeCache, n.symId)
    var inlineValueCopy = inlineValue
    if not cursorIsNil(inlineValue) and isSimpleLiteral(inlineValueCopy):
      ctx.dest.addSubtree inlineValue
    else:
      let ext = maybeMangle(ctx, n.symId)
      if ext.len != 0:
        ctx.dest.addSymUse pool.syms.getOrIncl(ext), n.info
      else:
        ctx.dest.add n
      ctx.demand n.symId
    inc n
  of StringLit:
    genStringLit ctx, n
    inc n
  of UnknownToken, DotToken, Ident, CharLit, IntLit, UIntLit, FloatLit:
    ctx.dest.add n
    inc n

proc traverseLocal(ctx: var EContext; n: var Cursor; tag: SymKind; mode: TraverseMode) =
  var symKind = if tag == ResultY: VarY else: tag
  var localDecl = n
  let toPatch = ctx.dest.len
  let vinfo = n.info
  ctx.dest.addParLe symKind, vinfo
  inc n
  let (s, sinfo) = getSymDef(ctx, n)
  if tag == ResultY:
    ctx.resultSym = s
  skipExportMarker ctx, n
  let pinfo = n.info
  let prag = parsePragmas(ctx, n)

  ctx.dest.add symdefToken(s, sinfo)
  ctx.offer s

  var genPragmas = openGenPragmas()

  if prag.externName.len > 0:
    ctx.registerMangle(s, prag.externName & ".c")
    ctx.addKeyVal genPragmas, "was", symToken(s, pinfo), pinfo

  if ThreadvarP in prag.flags:
    ctx.dest[toPatch] = tagToken("tvar", vinfo)
    symKind = TvarY
  elif GlobalP in prag.flags:
    ctx.dest[toPatch] = tagToken("gvar", vinfo)
    symKind = GvarY

  if prag.align != IntId(0):
    ctx.addKeyVal genPragmas, "align", intToken(prag.align, pinfo), pinfo
  if prag.bits != IntId(0):
    ctx.addKeyVal genPragmas, "bits", intToken(prag.bits, pinfo), pinfo
  closeGenPragmas ctx, genPragmas

  var nodecl = prag.flags.contains(NodeclP)
  ctx.typeCache.registerLocal(s, symKind, n)
  if tag == ParamY and typeKind(n) == VarargsT:
    skip n
    nodecl = true
  else:
    traverseType ctx, n

  if mode == TraverseSig and localDecl.substructureKind == ParamU:
    # Parameter decls in NIFC have no dot token for the default value!
    skip n
  else:
    traverseExpr ctx, n
  takeParRi ctx, n
  if nodecl:
    ctx.dest.shrink toPatch
  if prag.header != StrId(0):
    ctx.headers.incl prag.header

proc traverseWhile(ctx: var EContext; n: var Cursor) =
  let info = n.info
  ctx.nestedIn.add (WhileS, SymId(0))
  ctx.dest.add n
  inc n
  traverseExpr ctx, n
  traverseStmt ctx, n
  takeParRi ctx, n
  let lab = ctx.nestedIn[^1][1]
  if lab != SymId(0):
    ctx.dest.add tagToken("lab", info)
    ctx.dest.add symdefToken(lab, info)
    ctx.offer lab
    ctx.dest.addParRi()
  discard ctx.nestedIn.pop()

proc traverseBlock(ctx: var EContext; n: var Cursor) =
  let info = n.info
  inc n
  if n.kind == DotToken:
    ctx.nestedIn.add (BlockS, SymId(0))
    inc n
  else:
    let (s, _) = getSymDef(ctx, n)
    ctx.nestedIn.add (BlockS, s)
  ctx.dest.add tagToken("scope", info)
  traverseStmt ctx, n
  takeParRi ctx, n
  let lab = ctx.nestedIn[^1][1]
  if lab != SymId(0):
    ctx.dest.add tagToken("lab", info)
    ctx.dest.add symdefToken(lab, info)
    ctx.offer lab
    ctx.dest.addParRi()
  discard ctx.nestedIn.pop()

proc traverseBreak(ctx: var EContext; n: var Cursor) =
  let info = n.info
  inc n
  if n.kind == DotToken:
    inc n
    ctx.dest.add tagToken("break", info)
  else:
    expectSym ctx, n
    let lab = n.symId
    inc n
    ctx.dest.add tagToken("jmp", info)
    ctx.dest.add symToken(lab, info)
  takeParRi ctx, n

proc traverseIf(ctx: var EContext; n: var Cursor) =
  # (if cond (.. then ..) (.. else ..))
  ctx.dest.add n
  inc n
  while n.kind == ParLe and n.substructureKind == ElifU:
    ctx.dest.add n
    inc n # skips '(elif'
    traverseExpr ctx, n
    traverseStmt ctx, n
    takeParRi ctx, n
  if n.kind == ParLe and n.substructureKind == ElseU:
    ctx.dest.add n
    inc n
    traverseStmt ctx, n
    takeParRi ctx, n
  takeParRi ctx, n

include stringcases

proc traverseStringCase(ctx: var EContext; n: var Cursor): bool =
  var nb = n
  inc nb
  let selectorType = getType(ctx.typeCache, nb)
  if isSomeStringType(selectorType):
    transformStringCase(ctx, n)
    result = true
  else:
    result = false

proc traverseCase(ctx: var EContext; n: var Cursor) =
  if traverseStringCase(ctx, n):
    return
  ctx.dest.add n
  inc n
  traverseExpr ctx, n
  while n.kind != ParRi:
    case n.substructureKind
    of OfU:
      ctx.dest.add n
      inc n
      if n.kind == ParLe and n.substructureKind == RangesU:
        inc n
        ctx.add "ranges", n.info
        while n.kind != ParRi:
          traverseExpr ctx, n
        takeParRi ctx, n
      else:
        traverseExpr ctx, n
      traverseStmt ctx, n
      takeParRi ctx, n
    of ElseU:
      ctx.dest.add n
      inc n
      traverseStmt ctx, n
      takeParRi ctx, n
    else:
      error ctx, "expected (of) or (else) but got: ", n
  takeParRi ctx, n

proc traverseStmt(ctx: var EContext; n: var Cursor; mode = TraverseAll) =
  case n.kind
  of DotToken:
    ctx.dest.add n
    inc n
  of ParLe:
    case n.stmtKind
    of NoStmt:
      error ctx, "unknown statement: ", n
    of StmtsS:
      if mode == TraverseTopLevel:
        inc n
        while n.kind notin {EofToken, ParRi}:
          traverseStmt ctx, n, mode
        skipParRi ctx, n
      else:
        ctx.dest.add n
        inc n
        ctx.loop n:
          traverseStmt ctx, n, mode
    of ScopeS:
      ctx.openMangleScope()
      if mode == TraverseTopLevel:
        inc n
        while n.kind notin {EofToken, ParRi}:
          traverseStmt ctx, n, mode
        skipParRi ctx, n
      else:
        ctx.dest.add n
        inc n
        ctx.loop n:
          traverseStmt ctx, n, mode
      ctx.closeMangleScope()
    of VarS, LetS, CursorS:
      traverseLocal ctx, n, VarY, mode
    of ResultS:
      traverseLocal ctx, n, ResultY, mode
    of GvarS, GletS:
      traverseLocal ctx, n, GvarY, mode
    of TvarS, TletS:
      traverseLocal ctx, n, TvarY, mode
    of ConstS:
      traverseLocal ctx, n, ConstY, mode
    of CmdS, CallS:
      ctx.dest.add tagToken("call", n.info)
      inc n
      ctx.loop n:
        traverseExpr ctx, n
    of EmitS, AsmS:
      ctx.dest.add n
      inc n
      ctx.loop n:
        if n.kind == StringLit:
          ctx.dest.add n
          inc n
        else:
          traverseExpr ctx, n
    of AsgnS, RetS:
      ctx.dest.add n
      inc n
      ctx.loop n:
        traverseExpr ctx, n
    of DiscardS:
      let discardToken = n
      inc n
      if n.kind == DotToken:
        # eliminates discard without side effects
        inc n
        skipParRi ctx, n
      else:
        ctx.dest.add discardToken
        traverseExpr ctx, n
        takeParRi ctx, n
    of BreakS: traverseBreak ctx, n
    of WhileS: traverseWhile ctx, n
    of BlockS: traverseBlock ctx, n
    of IfS: traverseIf ctx, n
    of CaseS: traverseCase ctx, n
    of YldS, ForS, InclS, ExclS, DeferS, UnpackDeclS:
      error ctx, "BUG: not eliminated: ", n
    of TryS, RaiseS:
      error ctx, "BUG: not implemented: ", n
    of FuncS, ProcS, ConverterS, MethodS:
      traverseProc ctx, n, mode
    of MacroS, TemplateS, IncludeS, ImportS, FromS, ImportExceptS, ExportS, CommentS, IteratorS,
       ImportasS, ExportexceptS, BindS, MixinS, UsingS, StaticstmtS:
      # pure compile-time construct, ignore:
      skip n
    of TypeS:
      traverseTypeDecl ctx, n
    of ContinueS, WhenS:
      error ctx, "unreachable: ", n
    of PragmasS:
      skip n
  else:
    error ctx, "statement expected, but got: ", n

proc transformInlineRoutines(ctx: var EContext; n: var Cursor) =
  var swapped = createTokenBuf()
  swap ctx.dest, swapped

  var toTransform = createTokenBuf()
  toTransform.copyIntoKind StmtsS, n.info:
    takeTree(toTransform, n)
  var c0 = beginRead(toTransform)
  var dest = transform(ctx, c0, ctx.main)
  var c1 = beginRead(dest)
  inc c1 # skips (stmts

  swap ctx.dest, swapped

  traverseStmt ctx, c1, TraverseSig

proc importSymbol(ctx: var EContext; s: SymId) =
  let res = tryLoadSym(s)
  if res.status == LacksNothing:
    var n = res.decl
    let kind = n.symKind
    case kind
    of TypeY:
      traverseTypeDecl ctx, n
    of EfldY:
      # import full enum type:
      let typ = asLocal(n).typ
      assert typ.kind == Symbol
      ctx.demand typ.symId
    else:
      let isR = isRoutine(kind)
      if isR or isLocal(kind):
        var pragmas = if isR:
                        asRoutine(n).pragmas
                      else:
                        asLocal(n).pragmas
        let prag = parsePragmas(ctx, pragmas)
        if isR and InlineP in prag.flags:
          transformInlineRoutines(ctx, n)
          return
        if NodeclP in prag.flags:
          if prag.externName.len > 0:
            ctx.registerMangle(s, prag.externName & ".c")
          if prag.header != StrId(0):
            ctx.headers.incl prag.header
          return

      # XXX This is a stupid hack to avoid producing (imp (imp ...))
      inc ctx.inImpSection
      ctx.dest.add tagToken("imp", n.info)
      traverseStmt ctx, n, TraverseSig
      ctx.dest.addParRi()
      dec ctx.inImpSection
  else:
    error ctx, "could not find symbol: " & pool.syms[s]

proc writeOutput(ctx: var EContext, rootInfo: PackedLineInfo) =
  var b = nifbuilder.open(ctx.dir / ctx.main & ".c.nif")
  b.addHeader "hexer", "nifc"
  var stack: seq[PackedLineInfo] = @[]
  if rootInfo.isValid:
    stack.add rootInfo
    let rawInfo = unpack(pool.man, rootInfo)
    b.addLineInfo(rawInfo.col, rawInfo.line, pool.files[rawInfo.file])
  b.addTree "stmts"
  for h in ctx.headers:
    b.withTree "incl":
      b.addStrLit pool.strings[h]

  var n = beginRead(ctx.dest)
  var ownerStack = @[(SymId(0), -1)]

  var nested = 0
  var nextIsOwner = -1
  for nb in 0 ..< ctx.dest.len:
    let info = n.info
    if info.isValid:
      let rawInfo = unpack(pool.man, info)
      let file = rawInfo.file
      var line = rawInfo.line
      var col = rawInfo.col
      if file.isValid:
        var fileAsStr = ""
        if stack.len > 0:
          let pRawInfo = unpack(pool.man, stack[^1])
          if file != pRawInfo.file: fileAsStr = pool.files[file]
          if fileAsStr.len == 0:
            line = line - pRawInfo.line
            col = col - pRawInfo.col
        else:
          fileAsStr = pool.files[file]
        b.addLineInfo(col, line, fileAsStr)

    case n.kind
    of DotToken:
      b.addEmpty()
    of Ident:
      b.addIdent(pool.strings[n.litId])
    of Symbol:
      let val = ctx.maybeMangle(n.symId)
      if val.len > 0:
        b.addSymbol(val)
      else:
        b.addSymbol(pool.syms[n.symId])
    of SymbolDef:
      let val = ctx.maybeMangle(n.symId)
      if val.len > 0:
        b.addSymbolDef(val)
      else:
        b.addSymbolDef(pool.syms[n.symId])
      if nextIsOwner >= 0:
        ownerStack.add (n.symId, nextIsOwner)
        nextIsOwner = -1
    of IntLit:
      b.addIntLit(pool.integers[n.intId])
    of UIntLit:
      b.addUIntLit(pool.uintegers[n.uintId])
    of FloatLit:
      b.addFloatLit(pool.floats[n.floatId])
    of CharLit:
      b.addCharLit char(n.uoperand)
    of StringLit:
      b.addStrLit(pool.strings[n.litId])
    of UnknownToken:
      b.addIdent "<unknown token>"
    of EofToken:
      b.addIntLit n.soperand
    of ParRi:
      if stack.len > 0:
        discard stack.pop()
      b.endTree()
      if nested > 0: dec nested
      if ownerStack[^1][1] == nested:
        discard ownerStack.pop()
    of ParLe:
      let tag = pool.tags[n.tagId]
      if tag == "proc" or tag == "type":
        nextIsOwner = nested
      b.addTree(tag)
      stack.add info
      inc nested
    inc n

  b.endTree()
  b.close()


proc expand*(infile: string, bits: int) =
  let (dir, file, ext) = splitModulePath(infile)
  var ctx = EContext(dir: (if dir.len == 0: getCurrentDir() else: dir), ext: ext, main: file,
    dest: createTokenBuf(),
    nestedIn: @[(StmtsS, SymId(0))],
    typeCache: createTypeCache(),
    bits: bits
    )
  ctx.openMangleScope()

  var c0 = setupProgram(infile, infile.changeFileExt ".c.nif", true)
  var dest = transform(ctx, c0, file)

  var n = beginRead(dest)
  let rootInfo = n.info

  if stmtKind(n) == StmtsS:
    inc n
    #genStringType ctx, n.info
    while n.kind != ParRi:
      traverseStmt ctx, n, TraverseTopLevel
  else:
    error ctx, "expected (stmts) but got: ", n

  # fix point expansion:
  var i = 0
  while i < ctx.requires.len:
    let imp = ctx.requires[i]
    if not ctx.declared.contains(imp):
      importSymbol(ctx, imp)
    inc i
  ctx.dest.add ctx.pending
  skipParRi ctx, n
  writeOutput ctx, rootInfo
  ctx.closeMangleScope()

when isMainModule:
  echo splitModulePath("/abc/def/name.4.nif")
