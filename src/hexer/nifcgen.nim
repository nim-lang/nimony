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


proc skipExportMarker(ctx: var EContext; c: var Cursor) =
  if c.kind == DotToken:
    inc c
  elif c.kind == Ident and pool.strings[c.litId] == "x":
    inc c
  elif c.kind == ParLe:
    # can now also be `(tag)` or `(tag <bits>)`:
    skip c
  else:
    error ctx, "expected '.' or 'x' for an export marker: ", c

proc expectSymdef(ctx: var EContext; c: var Cursor) =
  if c.kind != SymbolDef:
    error ctx, "expected symbol definition, but got: ", c

proc getSymDef(ctx: var EContext; c: var Cursor): (SymId, PackedLineInfo) =
  expectSymdef(ctx, c)
  result = (c.symId, c.info)
  inc c

proc expectSym(ctx: var EContext; c: var Cursor) =
  if c.kind != Symbol:
    error ctx, "expected symbol, but got: ", c

proc getSym(ctx: var EContext; c: var Cursor): (SymId, PackedLineInfo) =
  expectSym(ctx, c)
  result = (c.symId, c.info)
  inc c

proc expectStrLit(ctx: var EContext; c: var Cursor) =
  if c.kind != StringLit:
    error ctx, "expected string literal, but got: ", c

proc expectIntLit(ctx: var EContext; c: var Cursor) =
  if c.kind != IntLit:
    error ctx, "expected int literal, but got: ", c



proc add(ctx: var EContext; tag: string; info: PackedLineInfo) =
  ctx.dest.add tagToken(tag, info)

type
  TraverseMode = enum
    TraverseAll, TraverseSig, TraverseTopLevel

proc traverseExpr(ctx: var EContext; c: var Cursor)
proc traverseStmt(ctx: var EContext; c: var Cursor; mode = TraverseAll)
proc traverseLocal(ctx: var EContext; c: var Cursor; tag: SymKind; mode: TraverseMode)

type
  TypeFlag = enum
    IsTypeBody
    IsPointerOf
    IsNodecl

proc traverseType(ctx: var EContext; c: var Cursor; flags: set[TypeFlag] = {})

type
  CollectedPragmas = object
    externName: string
    flags: set[PragmaKind]
    align, bits: IntId
    header: StrId
    callConv: CallConv

proc parsePragmas(ctx: var EContext; c: var Cursor): CollectedPragmas

proc traverseField(ctx: var EContext; c: var Cursor; flags: set[TypeFlag] = {}) =
  ctx.dest.add c # fld
  inc c

  expectSymdef(ctx, c)
  let (s, sinfo) = getSymDef(ctx, c)
  ctx.dest.add symdefToken(s, sinfo)
  ctx.offer s

  skipExportMarker ctx, c

  let prag = parsePragmas(ctx, c)

  ctx.dest.addDotToken() # adds pragmas

  if prag.externName.len > 0:
    ctx.registerMangle(s, prag.externName & ".c")

  traverseType ctx, c, flags

  skip c # skips value
  takeParRi ctx, c

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

proc traverseEnumField(ctx: var EContext; c: var Cursor; flags: set[TypeFlag] = {}) =
  ctx.dest.add c # efld
  inc c

  expectSymdef(ctx, c)
  let (s, sinfo) = getSymDef(ctx, c)
  ctx.dest.add symdefToken(s, sinfo)
  ctx.offer s

  skipExportMarker ctx, c

  skip c # pragmas: must be empty

  skip c # type: must be the enum itself

  inc c # skips TupleConstr
  traverseExpr ctx, c
  skip c
  skipParRi ctx, c

  takeParRi ctx, c

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

proc traverseTupleBody(ctx: var EContext; c: var Cursor) =
  let info = c.info
  inc c
  ctx.dest.add tagToken("object", info)
  ctx.dest.addDotToken()
  var counter = 0
  while c.kind != ParRi:
    if c.substructureKind == KvU:
      inc c # skip tag
      skip c # skip name
      genTupleField(ctx, c, counter)
      skipParRi ctx, c
    else:
      genTupleField(ctx, c, counter)
    inc counter
  takeParRi ctx, c

proc traverseArrayBody(ctx: var EContext; c: var Cursor) =
  ctx.dest.add c
  inc c
  traverseType ctx, c
  if c.typeKind == RangetypeT:
    inc c
    skip c
    expectIntLit ctx, c
    let first = pool.integers[c.intId]
    inc c
    expectIntLit ctx, c
    let last = pool.integers[c.intId]
    inc c
    skipParRi ctx, c
    ctx.dest.addIntLit(last - first + 1, c.info)
  else:
    # should not be possible, but assume length anyway
    traverseExpr ctx, c
  takeParRi ctx, c

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

proc traverseParams(ctx: var EContext; c: var Cursor)

proc traverseProcTypeBody(ctx: var EContext; c: var Cursor) =
  ctx.dest.add tagToken("proctype", c.info)
  # This is really stupid...
  ctx.dest.addDotToken() # name
  inc c # proc
  # name, export marker, pattern, type vars:
  for i in 0..<4: skip c
  traverseParams ctx, c

  let pinfo = c.info
  let prag = parsePragmas(ctx, c)
  var genPragmas = openGenPragmas()
  if prag.callConv != NoCallConv:
    let name = $prag.callConv
    ctx.addKey genPragmas, name, pinfo
  closeGenPragmas ctx, genPragmas

  # ignore, effects and body:
  skip c
  skip c
  takeParRi ctx, c

proc traverseRefBody(ctx: var EContext; c: var Cursor; key: string) =
  # We translate `ref T` to:
  # ptr OuterT;
  # OuterT = object
  #  r: int
  #  d: T
  # This means `deref x` becomes `x->d` and `x.field` becomes `x->d.field`
  # `cast` must also be adjusted by the offset of `d` within `OuterT` but this seems
  # to be optional.

  #let dataStructName = pool.syms.getOrIncl(key & ".1.t")

  let info = c.info
  inc c
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
  ctx.traverseType(c, {})
  ctx.dest.addParRi() # "fld"

  ctx.dest.addParRi() # "object"

proc traverseAsNamedType(ctx: var EContext; c: var Cursor) =
  let info = c.info
  var body = c
  let k = body.typeKind
  let key = takeMangle c

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

proc traverseType(ctx: var EContext; c: var Cursor; flags: set[TypeFlag] = {}) =
  case c.kind
  of DotToken:
    ctx.dest.add c
    inc c
  of Symbol:
    let s = c.symId
    let ext = maybeMangle(ctx, s)
    if ext.len != 0:
      ctx.dest.addSymUse pool.syms.getOrIncl(ext), c.info
      inc c
      return
    let res = tryLoadSym(s)
    if res.status == LacksNothing:
      var body = asTypeDecl(res.decl).body
      if body.typeKind == DistinctT: # skips DistinctT
        inc body
        traverseType(ctx, body, flags)
        inc c
      else:
        ctx.demand s
        ctx.dest.add c
        inc c
    else:
      error ctx, "could not find symbol: " & pool.syms[s]
  of ParLe:
    case c.typeKind
    of NoType, ErrT, OrT, AndT, NotT, TypedescT, UntypedT, TypedT, TypeKindT, OrdinalT:
      error ctx, "type expected but got: ", c
    of IntT, UIntT:
      let start = ctx.dest.len
      ctx.dest.add c
      inc c
      ctx.dest.add c
      inc c
      if c.kind != ParRi and c.pragmaKind in {ImportcP, ImportcppP}:
        ctx.dest.shrink start
        inc c
        ctx.dest.addSymUse pool.syms.getOrIncl(pool.strings[c.litId] & ".c"), c.info
        inc c
        skipParRi ctx, c
        skipParRi ctx, c
      else:
        takeParRi ctx, c
    of FloatT, CharT, BoolT, AutoT, SymKindT:
      ctx.loop c:
        ctx.dest.add c
        inc c
    of MutT, LentT:
      ctx.dest.add tagToken("ptr", c.info)
      inc c
      if isViewType(c):
        ctx.dest.shrink ctx.dest.len-1 # remove the "ptr" again
        traverseType ctx, c, {}
        skipParRi c
      else:
        ctx.loop c:
          traverseType ctx, c, {IsPointerOf}
    of PtrT, OutT:
      ctx.dest.add tagToken("ptr", c.info)
      inc c
      ctx.loop c:
        traverseType ctx, c, {IsPointerOf}
    of RefT:
      traverseAsNamedType ctx, c
    of ArrayT, ProctypeT:
      if IsNodecl in flags:
        traverseArrayBody ctx, c
      else:
        traverseAsNamedType ctx, c
    of RangetypeT:
      # skip to base type
      inc c
      traverseType ctx, c
      skip c
      skip c
      skipParRi ctx, c
    of UarrayT:
      if IsPointerOf in flags:
        inc c
        traverseType ctx, c
        skipParRi ctx, c
      else:
        ctx.dest.add tagToken("flexarray", c.info)
        inc c
        traverseType ctx, c
        takeParRi ctx, c
    of PointerT:
      ctx.dest.add tagToken("ptr", c.info)
      ctx.dest.add tagToken("void", c.info)
      ctx.dest.addParRi()
      inc c
      takeParRi ctx, c
    of CstringT:
      ctx.dest.add tagToken("ptr", c.info)
      ctx.dest.add tagToken($CharT, c.info)
      ctx.dest.addIntLit(8, c.info)
      ctx.dest.addParRi()
      inc c
      takeParRi ctx, c
    of StaticT, SinkT, DistinctT:
      inc c
      traverseType ctx, c, flags
      skipParRi ctx, c
    of TupleT:
      traverseAsNamedType ctx, c
    of ObjectT:
      ctx.dest.add c
      inc c
      if c.kind == DotToken:
        ctx.dest.add c
        inc c
      else:
        # inherited symbol
        let (s, sinfo) = getSym(ctx, c)
        ctx.dest.add symToken(s, sinfo)
        ctx.demand s

      if c.kind == DotToken:
        ctx.dest.add c
        inc c
      else:
        while c.substructureKind == FldU:
          traverseField(ctx, c, flags)

      takeParRi ctx, c
    of EnumT, HoleyEnumT:
      ctx.dest.add tagToken("enum", c.info)
      inc c
      traverseType ctx, c, flags # base type

      while c.substructureKind == EfldU:
        traverseEnumField(ctx, c, flags)

      takeParRi ctx, c
    of SetT:
      let info = c.info
      inc c
      let sizeOrig = bitsetSizeInBytes(c)
      var err = false
      let size = asSigned(sizeOrig, err)
      if err:
        error ctx, "invalid set element type: ", c
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
      skip c
      skipParRi ctx, c
    of VoidT, VarargsT, NiltT, ConceptT,
       IteratorT, InvokeT, ParamsT, ItertypeT:
      error ctx, "unimplemented type: ", c
  else:
    error ctx, "type expected but got: ", c

proc maybeByConstRef(ctx: var EContext; c: var Cursor) =
  let param = asLocal(c)
  if param.typ.typeKind in {TypedescT, StaticT}:
    # do not produce any code for this as it's a compile-time parameter
    skip c
  elif passByConstRef(param.typ, param.pragmas, ctx.bits div 8):
    var paramBuf = createTokenBuf()
    paramBuf.add tagToken("param", c.info)
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
    skip c
  else:
    traverseLocal(ctx, c, ParamY, TraverseSig)

proc traverseParams(ctx: var EContext; c: var Cursor) =
  if c.kind == DotToken:
    ctx.dest.add c
    inc c
  elif c.kind == ParLe and c.typeKind == ParamsT:
    ctx.dest.add c
    inc c
    loop ctx, c:
      if c.symKind != ParamY:
        error ctx, "expected (param) but got: ", c
      maybeByConstRef(ctx, c)
  else:
    error ctx, "expected (params) but got: ", c
  # the result type
  traverseType ctx, c

proc parsePragmas(ctx: var EContext; c: var Cursor): CollectedPragmas =
  result = default(CollectedPragmas)
  if c.kind == DotToken:
    inc c
  elif c.kind == ParLe and pool.tags[c.tag] == $PragmasS:
    inc c
    while true:
      case c.kind
      of ParRi:
        inc c
        break
      of EofToken:
        error ctx, "expected ')', but EOF reached"
      else: discard
      if c.kind == ParLe:
        let pk = c.pragmaKind
        case pk
        of NoPragma:
          let cc = c.callConvKind
          if cc == NoCallConv:
            error ctx, "unknown pragma: ", c
          else:
            result.callConv = cc
          inc c
        of MagicP:
          inc c
          if c.kind notin {StringLit, Ident}:
            error ctx, "expected string literal or ident, but got: ", c
          result.flags.incl NodeclP
          inc c
        of ImportcP, ImportcppP:
          inc c
          expectStrLit ctx, c
          result.externName = pool.strings[c.litId]
          result.flags.incl pk
          inc c
        of ExportcP, PluginP:
          inc c
          expectStrLit ctx, c
          result.externName = pool.strings[c.litId]
          inc c
        of NodeclP, SelectanyP, ThreadvarP, GlobalP, DiscardableP, NoReturnP,
           VarargsP, BorrowP, NoSideEffectP, NoDestroyP, ByCopyP, ByRefP,
           InlineP, NoinlineP, NoInitP, InjectP, GensymP, UntypedP, ViewP:
          result.flags.incl pk
          inc c
        of HeaderP:
          inc c
          expectStrLit ctx, c
          result.header = c.litId
          result.flags.incl NodeclP
          inc c
        of AlignP:
          inc c
          expectIntLit ctx, c
          result.align = c.intId
          inc c
        of BitsP:
          inc c
          expectIntLit ctx, c
          result.bits = c.intId
          inc c
        of RequiresP, EnsuresP, StringP, RaisesP, ErrorP:
          skip c
          continue
        of BuildP, EmitP:
          raiseAssert "unreachable"
        skipParRi ctx, c
      else:
        error ctx, "unknown pragma: ", c
  else:
    error ctx, "(pragmas) or '.' expected, but got: ", c

proc traverseProcBody(ctx: var EContext; c: var Cursor) =
  if c.stmtKind == StmtsS:
    ctx.dest.add c
    inc c
    var prevStmt = NoStmt
    while c.kind != ParRi:
      prevStmt = c.stmtKind
      traverseStmt ctx, c, TraverseAll
    if prevStmt == RetS or ctx.resultSym == SymId(0):
      discard "ok, do not add another return"
    else:
      ctx.dest.add parLeToken(RetS, c.info)
      ctx.dest.add symToken(ctx.resultSym, c.info)
      ctx.dest.addParRi()
    takeParRi ctx, c
  else:
    traverseStmt ctx, c, TraverseAll

proc traverseProc(ctx: var EContext; c: var Cursor; mode: TraverseMode) =
  ctx.openMangleScope()
  var dst = createTokenBuf(50)
  swap ctx.dest, dst
  #let toPatch = ctx.dest.len
  let oldResultSym = ctx.resultSym
  ctx.resultSym = SymId(0)

  let vinfo = c.info
  ctx.add "proc", vinfo
  inc c
  let (s, sinfo) = getSymDef(ctx, c)

  # namePos
  ctx.dest.add symdefToken(s, sinfo)
  ctx.offer s

  var isGeneric = false
  if c.kind == ParLe:
    isGeneric = true
  skipExportMarker ctx, c

  skip c # patterns

  if c.substructureKind == TypevarsU:
    isGeneric = true
    # count each typevar as used:
    inc c
    while c.kind != ParRi:
      assert c.symKind == TypevarY
      inc c
      let (typevar, _) = getSymDef(ctx, c)
      ctx.offer typevar
      skipToEnd c
    inc c
  else:
    skip c # generic parameters

  if isGeneric:
    # count each param as used:
    inc c
    while c.kind != ParRi:
      assert c.symKind == ParamY
      inc c
      let (param, _) = getSymDef(ctx, c)
      ctx.offer param
      skipToEnd c
    inc c
    skip c # skip return type
  else:
    traverseParams ctx, c

  let pinfo = c.info
  let prag = parsePragmas(ctx, c)

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

  skip c # miscPos

  # body:
  if isGeneric:
    skip c
  elif mode != TraverseSig or InlineP in prag.flags:
    traverseProcBody ctx, c
  else:
    ctx.dest.addDotToken()
    skip c
  takeParRi ctx, c
  swap dst, ctx.dest
  if NodeclP in prag.flags or isGeneric:
    discard "do not add to ctx.dest"
  elif prag.flags * {ImportcP, ImportcppP} != {} and ctx.inImpSection == 0:
    ctx.dest.add tagToken("imp", c.info)
    ctx.dest.add dst
    ctx.dest.addParRi()
  else:
    ctx.dest.add dst
  if prag.header != StrId(0):
    ctx.headers.incl prag.header
  discard setOwner(ctx, oldOwner)
  ctx.closeMangleScope()
  ctx.resultSym = oldResultSym

proc traverseTypeDecl(ctx: var EContext; c: var Cursor) =
  var dst = createTokenBuf(50)
  swap ctx.dest, dst
  #let toPatch = ctx.dest.len
  let vinfo = c.info
  ctx.add "type", vinfo
  inc c
  let (s, sinfo) = getSymDef(ctx, c)
  let oldOwner = setOwner(ctx, s)

  ctx.dest.add symdefToken(s, sinfo)
  ctx.offer s

  var isGeneric = c.kind == ParLe
  skipExportMarker ctx, c
  if c.substructureKind == TypevarsU:
    isGeneric = true
    # count each typevar as used:
    inc c
    while c.kind != ParRi:
      assert c.symKind == TypevarY
      inc c
      let (typevar, _) = getSymDef(ctx, c)
      ctx.offer typevar
      skipToEnd c
    inc c
  else:
    skip c # generic parameters

  let prag = parsePragmas(ctx, c)

  ctx.dest.addDotToken() # adds pragmas

  if prag.externName.len > 0:
    ctx.registerMangle(s, prag.externName & ".c")
  if c.typeKind in TypeclassKinds:
    isGeneric = true
  if isGeneric:
    skip c
  else:
    traverseType ctx, c, {IsTypeBody} + (if NodeclP in prag.flags: {IsNodecl} else: {})
  takeParRi ctx, c
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

proc genStringLit(ctx: var EContext; c: Cursor) =
  assert c.kind == StringLit
  let info = c.info
  let s {.cursor.} = pool.strings[c.litId]
  genStringLit(ctx, s, info)

proc traverseStmtsExpr(ctx: var EContext; c: var Cursor) =
  let head = c.load()
  inc c
  if isLastSon(c):
    traverseExpr ctx, c
    skipParRi ctx, c
  else:
    ctx.dest.add head
    while c.kind != ParRi:
      if not isLastSon(c):
        traverseStmt ctx, c
      else:
        traverseExpr ctx, c
    takeParRi ctx, c

proc traverseTupleConstr(ctx: var EContext; c: var Cursor) =
  ctx.dest.add tagToken("oconstr", c.info)
  var tupleType = ctx.typeCache.getType(c)
  ctx.traverseType(tupleType, {})
  inc c
  var counter = 0
  while c.kind != ParRi:
    ctx.dest.add tagToken("kv", c.info)
    ctx.dest.add symToken(ithTupleField(counter), c.info)
    inc counter
    if c.substructureKind == KvU:
      inc c # skip "kv"
      skip c # skip key
      traverseExpr ctx, c
      skipParRi ctx, c
    else:
      traverseExpr ctx, c
    ctx.dest.addParRi() # "kv"
  takeParRi ctx, c

proc traverseConv(ctx: var EContext; c: var Cursor) =
  let info = c.info
  let beforeConv = ctx.dest.len
  ctx.dest.add tagToken("conv", info)
  inc c
  let destType = c
  traverseType(ctx, c)
  let srcType = getType(ctx.typeCache, c)
  if destType.typeKind == CstringT and isStringType(srcType):
    var isSuffix = false
    if c.exprKind == SufX:
      isSuffix = true
      inc c
    if c.kind == StringLit:
      # evaluate the conversion at compile time:
      ctx.dest.shrink beforeConv
      ctx.dest.addStrLit pool.strings[c.litId]
      inc c
      if isSuffix:
        inc c
        skipParRi ctx, c
      skipParRi ctx, c
    else:
      let strField = pool.syms.getOrIncl(StringAField)
      ctx.dest.add tagToken("dot", info)
      traverseExpr(ctx, c)
      ctx.dest.add symToken(strField, info)
      ctx.dest.addIntLit(0, info)
      ctx.dest.addParRi()
      takeParRi ctx, c
  else:
    traverseExpr(ctx, c)
    takeParRi ctx, c

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

proc traverseExpr(ctx: var EContext; c: var Cursor) =
  case c.kind
  of EofToken, ParRi:
    error ctx, "BUG: unexpected ')' or EofToken"
  of ParLe:
    case c.exprKind
    of EqX, NeqX, LeX, LtX:
      ctx.dest.add c
      inc c
      let beforeType = ctx.dest.len
      traverseType(ctx, c)
      ctx.dest.shrink beforeType
      traverseExpr(ctx, c)
      traverseExpr(ctx, c)
      takeParRi ctx, c
    of CastX:
      ctx.dest.add c
      inc c
      traverseType(ctx, c)
      traverseExpr(ctx, c)
      takeParRi ctx, c
    of HconvX, ConvX:
      traverseConv ctx, c
    of DconvX:
      inc c
      let beforeType = ctx.dest.len
      traverseType(ctx, c)
      ctx.dest.shrink beforeType
      traverseExpr(ctx, c)
      skipParRi(ctx, c)
    of AconstrX:
      ctx.dest.add tagToken("aconstr", c.info)
      inc c
      traverseType(ctx, c)
      while c.kind != ParRi:
        traverseExpr(ctx, c)
      takeParRi ctx, c
    of OconstrX:
      ctx.dest.add tagToken("oconstr", c.info)
      inc c
      traverseType(ctx, c)
      while c.kind != ParRi:
        if c.substructureKind == KvU:
          ctx.dest.add c # KvU
          inc c
          takeTree ctx, c # key
          traverseExpr ctx, c # value
          takeParRi ctx, c
        else:
          traverseExpr ctx, c
      takeParRi ctx, c
    of TupX:
      traverseTupleConstr ctx, c
    of CmdX, CallStrLitX, InfixX, PrefixX, HcallX, CallX:
      ctx.dest.add tagToken("call", c.info)
      inc c
      while c.kind != ParRi:
        traverseExpr(ctx, c)
      takeParRi ctx, c
    of ExprX:
      traverseStmtsExpr ctx, c
    of ArrAtX:
      # XXX does not handle index type with offset low(I), maybe should be done in sem
      ctx.dest.add tagToken("at", c.info)
      inc c
      traverseExpr(ctx, c)
      traverseExpr(ctx, c)
      takeParRi ctx, c
    of TupAtX:
      ctx.dest.add tagToken("dot", c.info)
      inc c # skip tag
      traverseExpr ctx, c # tuple
      expectIntLit ctx, c
      ctx.dest.add symToken(ithTupleField(pool.integers[c.intId]), c.info)
      inc c # skip index
      ctx.dest.addIntLit(0, c.info) # inheritance
      takeParRi ctx, c
    of DotX:
      ctx.dest.add tagToken("dot", c.info)
      inc c # skip tag
      traverseExpr ctx, c # obj
      traverseExpr ctx, c # field
      traverseExpr ctx, c # inheritance depth
      takeParRi ctx, c
    of DdotX:
      ctx.dest.add tagToken("dot", c.info)
      ctx.dest.add tagToken("deref", c.info)
      inc c # skip tag
      traverseExpr ctx, c
      ctx.dest.addParRi()
      traverseExpr ctx, c
      traverseExpr ctx, c
      takeParRi ctx, c
    of HaddrX, AddrX:
      ctx.dest.add tagToken("addr", c.info)
      inc c
      traverseExpr(ctx, c)
      takeParRi ctx, c
    of HderefX, DerefX:
      ctx.dest.add tagToken("deref", c.info)
      inc c
      traverseExpr(ctx, c)
      takeParRi ctx, c
    of SufX:
      var suf = c
      inc suf
      let arg = suf
      skip suf
      assert suf.kind == StringLit
      if arg.kind == StringLit and pool.strings[suf.litId] == "R":
        # cstring conversion
        inc c
        ctx.dest.add c # add string lit directly
        inc c # arg
        inc c # suf
      else:
        ctx.dest.add c
        inc c
        traverseExpr ctx, c
        ctx.dest.add c
        inc c
      takeParRi ctx, c
    of AshrX:
      ctx.dest.add tagToken("shr", c.info)
      inc c
      var bits = -1'i64
      if c.typeKind in {IntT, UIntT}:
        var bitsToken = c
        inc bitsToken
        bits = pool.integers[bitsToken.intId]
      else:
        #error ctx, "expected int/uint type for ashr, got: ", c
        discard
      traverseType(ctx, c)
      ctx.dest.copyIntoKind CastX, c.info:
        ctx.dest.add tagToken("i", c.info)
        ctx.dest.addIntLit(bits, c.info)
        ctx.dest.addParRi()
        traverseExpr ctx, c
      ctx.dest.copyIntoKind CastX, c.info:
        ctx.dest.add tagToken("u", c.info)
        ctx.dest.addIntLit(bits, c.info)
        ctx.dest.addParRi()
        traverseExpr ctx, c
      takeParRi ctx, c
    of ErrX, NewobjX, SetConstrX, PlusSetX, MinusSetX, MulSetX, XorSetX, EqSetX, LeSetX, LtSetX,
       InSetX, CardX, BracketX, CurlyX, CompilesX, DeclaredX, DefinedX, HighX, LowX, TypeofX, UnpackX,
       EnumtostrX, IsmainmoduleX, DefaultobjX, DefaulttupX, DoX, CchoiceX, OchoiceX,
       EmoveX, DestroyX, DupX, CopyX, WasmovedX, SinkhX, TraceX, CurlyatX, PragmaxX, QuotedX, TabconstrX:
      error ctx, "BUG: not eliminated: ", c
      #skip c
    of AtX, PatX, ParX, NilX, InfX, NeginfX, NanX, FalseX, TrueX, AndX, OrX, NotX, NegX,
       SizeofX, AlignofX, OffsetofX, AddX, SubX, MulX, DivX, ModX, ShrX, ShlX,
       BitandX, BitorX, BitxorX, BitnotX, OconvX:
      ctx.dest.add c
      inc c
      while c.kind != ParRi:
        traverseExpr ctx, c
      takeParRi ctx, c
    of NoExpr:
      traverseType ctx, c
  of SymbolDef:
    ctx.dest.add c
    ctx.offer c.symId
    inc c
  of Symbol:
    let inlineValue = getInitValue(ctx.typeCache, c.symId)
    var inlineValueCopy = inlineValue
    if not cursorIsNil(inlineValue) and isSimpleLiteral(inlineValueCopy):
      ctx.dest.addSubtree inlineValue
    else:
      let ext = maybeMangle(ctx, c.symId)
      if ext.len != 0:
        ctx.dest.addSymUse pool.syms.getOrIncl(ext), c.info
      else:
        ctx.dest.add c
      ctx.demand c.symId
    inc c
  of StringLit:
    genStringLit ctx, c
    inc c
  of UnknownToken, DotToken, Ident, CharLit, IntLit, UIntLit, FloatLit:
    ctx.dest.add c
    inc c

proc traverseLocal(ctx: var EContext; c: var Cursor; tag: SymKind; mode: TraverseMode) =
  var symKind = if tag == ResultY: VarY else: tag
  var localDecl = c
  let toPatch = ctx.dest.len
  let vinfo = c.info
  ctx.dest.addParLe symKind, vinfo
  inc c
  let (s, sinfo) = getSymDef(ctx, c)
  if tag == ResultY:
    ctx.resultSym = s
  skipExportMarker ctx, c
  let pinfo = c.info
  let prag = parsePragmas(ctx, c)

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
  ctx.typeCache.registerLocal(s, symKind, c)
  if tag == ParamY and typeKind(c) == VarargsT:
    skip c
    nodecl = true
  else:
    traverseType ctx, c

  if mode == TraverseSig and localDecl.substructureKind == ParamU:
    # Parameter decls in NIFC have no dot token for the default value!
    skip c
  else:
    traverseExpr ctx, c
  takeParRi ctx, c
  if nodecl:
    ctx.dest.shrink toPatch
  if prag.header != StrId(0):
    ctx.headers.incl prag.header

proc traverseWhile(ctx: var EContext; c: var Cursor) =
  let info = c.info
  ctx.nestedIn.add (WhileS, SymId(0))
  ctx.dest.add c
  inc c
  traverseExpr ctx, c
  traverseStmt ctx, c
  takeParRi ctx, c
  let lab = ctx.nestedIn[^1][1]
  if lab != SymId(0):
    ctx.dest.add tagToken("lab", info)
    ctx.dest.add symdefToken(lab, info)
    ctx.offer lab
    ctx.dest.addParRi()
  discard ctx.nestedIn.pop()

proc traverseBlock(ctx: var EContext; c: var Cursor) =
  let info = c.info
  inc c
  if c.kind == DotToken:
    ctx.nestedIn.add (BlockS, SymId(0))
    inc c
  else:
    let (s, _) = getSymDef(ctx, c)
    ctx.nestedIn.add (BlockS, s)
  ctx.dest.add tagToken("scope", info)
  traverseStmt ctx, c
  takeParRi ctx, c
  let lab = ctx.nestedIn[^1][1]
  if lab != SymId(0):
    ctx.dest.add tagToken("lab", info)
    ctx.dest.add symdefToken(lab, info)
    ctx.offer lab
    ctx.dest.addParRi()
  discard ctx.nestedIn.pop()

proc traverseBreak(ctx: var EContext; c: var Cursor) =
  let info = c.info
  inc c
  if c.kind == DotToken:
    inc c
    ctx.dest.add tagToken("break", info)
  else:
    expectSym ctx, c
    let lab = c.symId
    inc c
    ctx.dest.add tagToken("jmp", info)
    ctx.dest.add symToken(lab, info)
  takeParRi ctx, c

proc traverseIf(ctx: var EContext; c: var Cursor) =
  # (if cond (.. then ..) (.. else ..))
  ctx.dest.add c
  inc c
  while c.kind == ParLe and c.substructureKind == ElifU:
    ctx.dest.add c
    inc c # skips '(elif'
    traverseExpr ctx, c
    traverseStmt ctx, c
    takeParRi ctx, c
  if c.kind == ParLe and c.substructureKind == ElseU:
    ctx.dest.add c
    inc c
    traverseStmt ctx, c
    takeParRi ctx, c
  takeParRi ctx, c

include stringcases

proc traverseStringCase(ctx: var EContext; c: var Cursor): bool =
  var nb = c
  inc nb
  let selectorType = getType(ctx.typeCache, nb)
  if isSomeStringType(selectorType):
    transformStringCase(ctx, c)
    result = true
  else:
    result = false

proc traverseCase(ctx: var EContext; c: var Cursor) =
  if traverseStringCase(ctx, c):
    return
  ctx.dest.add c
  inc c
  traverseExpr ctx, c
  while c.kind != ParRi:
    case c.substructureKind
    of OfU:
      ctx.dest.add c
      inc c
      if c.kind == ParLe and c.substructureKind == RangesU:
        inc c
        ctx.add "ranges", c.info
        while c.kind != ParRi:
          traverseExpr ctx, c
        takeParRi ctx, c
      else:
        traverseExpr ctx, c
      traverseStmt ctx, c
      takeParRi ctx, c
    of ElseU:
      ctx.dest.add c
      inc c
      traverseStmt ctx, c
      takeParRi ctx, c
    else:
      error ctx, "expected (of) or (else) but got: ", c
  takeParRi ctx, c

proc traverseStmt(ctx: var EContext; c: var Cursor; mode = TraverseAll) =
  case c.kind
  of DotToken:
    ctx.dest.add c
    inc c
  of ParLe:
    case c.stmtKind
    of NoStmt:
      error ctx, "unknown statement: ", c
    of StmtsS:
      if mode == TraverseTopLevel:
        inc c
        while c.kind notin {EofToken, ParRi}:
          traverseStmt ctx, c, mode
        skipParRi ctx, c
      else:
        ctx.dest.add c
        inc c
        ctx.loop c:
          traverseStmt ctx, c, mode
    of ScopeS:
      ctx.openMangleScope()
      if mode == TraverseTopLevel:
        inc c
        while c.kind notin {EofToken, ParRi}:
          traverseStmt ctx, c, mode
        skipParRi ctx, c
      else:
        ctx.dest.add c
        inc c
        ctx.loop c:
          traverseStmt ctx, c, mode
      ctx.closeMangleScope()
    of VarS, LetS, CursorS:
      traverseLocal ctx, c, VarY, mode
    of ResultS:
      traverseLocal ctx, c, ResultY, mode
    of GvarS, GletS:
      traverseLocal ctx, c, GvarY, mode
    of TvarS, TletS:
      traverseLocal ctx, c, TvarY, mode
    of ConstS:
      traverseLocal ctx, c, ConstY, mode
    of CmdS, CallS:
      ctx.dest.add tagToken("call", c.info)
      inc c
      ctx.loop c:
        traverseExpr ctx, c
    of EmitS, AsmS:
      ctx.dest.add c
      inc c
      ctx.loop c:
        if c.kind == StringLit:
          ctx.dest.add c
          inc c
        else:
          traverseExpr ctx, c
    of AsgnS, RetS:
      ctx.dest.add c
      inc c
      ctx.loop c:
        traverseExpr ctx, c
    of DiscardS:
      let discardToken = c
      inc c
      if c.kind == DotToken:
        # eliminates discard without side effects
        inc c
        skipParRi ctx, c
      else:
        ctx.dest.add discardToken
        traverseExpr ctx, c
        takeParRi ctx, c
    of BreakS: traverseBreak ctx, c
    of WhileS: traverseWhile ctx, c
    of BlockS: traverseBlock ctx, c
    of IfS: traverseIf ctx, c
    of CaseS: traverseCase ctx, c
    of YldS, ForS, InclS, ExclS, DeferS, UnpackDeclS:
      error ctx, "BUG: not eliminated: ", c
    of TryS, RaiseS:
      error ctx, "BUG: not implemented: ", c
    of FuncS, ProcS, ConverterS, MethodS:
      traverseProc ctx, c, mode
    of MacroS, TemplateS, IncludeS, ImportS, FromS, ImportExceptS, ExportS, CommentS, IteratorS,
       ImportasS, ExportexceptS, BindS, MixinS, UsingS, StaticstmtS:
      # pure compile-time construct, ignore:
      skip c
    of TypeS:
      traverseTypeDecl ctx, c
    of ContinueS, WhenS:
      error ctx, "unreachable: ", c
    of PragmasS:
      skip c
  else:
    error ctx, "statement expected, but got: ", c

proc transformInlineRoutines(ctx: var EContext; c: var Cursor) =
  var swapped = createTokenBuf()
  swap ctx.dest, swapped

  var toTransform = createTokenBuf()
  toTransform.copyIntoKind StmtsS, c.info:
    takeTree(toTransform, c)
  var c0 = beginRead(toTransform)
  var dest = transform(ctx, c0, ctx.main)
  var c1 = beginRead(dest)
  inc c1 # skips (stmts

  swap ctx.dest, swapped

  traverseStmt ctx, c1, TraverseSig

proc importSymbol(ctx: var EContext; s: SymId) =
  let res = tryLoadSym(s)
  if res.status == LacksNothing:
    var c = res.decl
    let kind = c.symKind
    case kind
    of TypeY:
      traverseTypeDecl ctx, c
    of EfldY:
      # import full enum type:
      let typ = asLocal(c).typ
      assert typ.kind == Symbol
      ctx.demand typ.symId
    else:
      let isR = isRoutine(kind)
      if isR or isLocal(kind):
        var pragmas = if isR:
                        asRoutine(c).pragmas
                      else:
                        asLocal(c).pragmas
        let prag = parsePragmas(ctx, pragmas)
        if isR and InlineP in prag.flags:
          transformInlineRoutines(ctx, c)
          return
        if NodeclP in prag.flags:
          if prag.externName.len > 0:
            ctx.registerMangle(s, prag.externName & ".c")
          if prag.header != StrId(0):
            ctx.headers.incl prag.header
          return

      # XXX This is a stupid hack to avoid producing (imp (imp ...))
      inc ctx.inImpSection
      ctx.dest.add tagToken("imp", c.info)
      traverseStmt ctx, c, TraverseSig
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

  var c = beginRead(ctx.dest)
  var ownerStack = @[(SymId(0), -1)]

  var nested = 0
  var nextIsOwner = -1
  for nb in 0 ..< ctx.dest.len:
    let info = c.info
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

    case c.kind
    of DotToken:
      b.addEmpty()
    of Ident:
      b.addIdent(pool.strings[c.litId])
    of Symbol:
      let val = ctx.maybeMangle(c.symId)
      if val.len > 0:
        b.addSymbol(val)
      else:
        b.addSymbol(pool.syms[c.symId])
    of SymbolDef:
      let val = ctx.maybeMangle(c.symId)
      if val.len > 0:
        b.addSymbolDef(val)
      else:
        b.addSymbolDef(pool.syms[c.symId])
      if nextIsOwner >= 0:
        ownerStack.add (c.symId, nextIsOwner)
        nextIsOwner = -1
    of IntLit:
      b.addIntLit(pool.integers[c.intId])
    of UIntLit:
      b.addUIntLit(pool.uintegers[c.uintId])
    of FloatLit:
      b.addFloatLit(pool.floats[c.floatId])
    of CharLit:
      b.addCharLit char(c.uoperand)
    of StringLit:
      b.addStrLit(pool.strings[c.litId])
    of UnknownToken:
      b.addIdent "<unknown token>"
    of EofToken:
      b.addIntLit c.soperand
    of ParRi:
      if stack.len > 0:
        discard stack.pop()
      b.endTree()
      if nested > 0: dec nested
      if ownerStack[^1][1] == nested:
        discard ownerStack.pop()
    of ParLe:
      let tag = pool.tags[c.tagId]
      if tag == "proc" or tag == "type":
        nextIsOwner = nested
      b.addTree(tag)
      stack.add info
      inc nested
    inc c

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

  var c = beginRead(dest)
  let rootInfo = c.info

  if stmtKind(c) == StmtsS:
    inc c
    #genStringType ctx, c.info
    while c.kind != ParRi:
      traverseStmt ctx, c, TraverseTopLevel
  else:
    error ctx, "expected (stmts) but got: ", c

  # fix point expansion:
  var i = 0
  while i < ctx.requires.len:
    let imp = ctx.requires[i]
    if not ctx.declared.contains(imp):
      importSymbol(ctx, imp)
    inc i
  ctx.dest.add ctx.pending
  skipParRi ctx, c
  writeOutput ctx, rootInfo
  ctx.closeMangleScope()

when isMainModule:
  echo splitModulePath("/abc/def/name.4.nif")
