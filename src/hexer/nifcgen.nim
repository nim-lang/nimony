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


proc setOwner(e: var EContext; newOwner: SymId): SymId =
  result = e.currentOwner
  e.currentOwner = newOwner

proc demand(e: var EContext; s: SymId) =
  if not e.declared.contains(s):
    e.requires.add s

proc offer(e: var EContext; s: SymId) =
  e.declared.incl s


proc skipExportMarker(e: var EContext; c: var Cursor) =
  if c.kind == DotToken:
    inc c
  elif c.kind == Ident and pool.strings[c.litId] == "x":
    inc c
  elif c.kind == ParLe:
    # can now also be `(tag)` or `(tag <bits>)`:
    skip c
  else:
    error e, "expected '.' or 'x' for an export marker: ", c

proc expectSymdef(e: var EContext; c: var Cursor) =
  if c.kind != SymbolDef:
    error e, "expected symbol definition, but got: ", c

proc getSymDef(e: var EContext; c: var Cursor): (SymId, PackedLineInfo) =
  expectSymdef(e, c)
  result = (c.symId, c.info)
  inc c

proc expectSym(e: var EContext; c: var Cursor) =
  if c.kind != Symbol:
    error e, "expected symbol, but got: ", c

proc getSym(e: var EContext; c: var Cursor): (SymId, PackedLineInfo) =
  expectSym(e, c)
  result = (c.symId, c.info)
  inc c

proc expectStrLit(e: var EContext; c: var Cursor) =
  if c.kind != StringLit:
    error e, "expected string literal, but got: ", c

proc expectIntLit(e: var EContext; c: var Cursor) =
  if c.kind != IntLit:
    error e, "expected int literal, but got: ", c



proc add(e: var EContext; tag: string; info: PackedLineInfo) =
  e.dest.add tagToken(tag, info)

type
  TraverseMode = enum
    TraverseAll, TraverseSig, TraverseTopLevel

proc traverseExpr(e: var EContext; c: var Cursor)
proc traverseStmt(e: var EContext; c: var Cursor; mode = TraverseAll)
proc traverseLocal(e: var EContext; c: var Cursor; tag: SymKind; mode: TraverseMode)

type
  TypeFlag = enum
    IsTypeBody
    IsPointerOf
    IsNodecl

proc traverseType(e: var EContext; c: var Cursor; flags: set[TypeFlag] = {})

type
  CollectedPragmas = object
    externName: string
    flags: set[PragmaKind]
    align, bits: IntId
    header: StrId
    callConv: CallConv

proc parsePragmas(e: var EContext; c: var Cursor): CollectedPragmas

proc traverseField(e: var EContext; c: var Cursor; flags: set[TypeFlag] = {}) =
  e.dest.add c # fld
  inc c

  expectSymdef(e, c)
  let (s, sinfo) = getSymDef(e, c)
  e.dest.add symdefToken(s, sinfo)
  e.offer s

  skipExportMarker e, c

  let prag = parsePragmas(e, c)

  e.dest.addDotToken() # adds pragmas

  if prag.externName.len > 0:
    e.registerMangle(s, prag.externName & ".c")

  traverseType e, c, flags

  skip c # skips value
  takeParRi e, c

proc ithTupleField(counter: int): SymId {.inline.} =
  pool.syms.getOrIncl("fld." & $counter)

proc genTupleField(e: var EContext; typ: var Cursor; counter: int) =
  e.dest.add tagToken("fld", typ.info)
  let name = ithTupleField(counter)
  e.dest.add symdefToken(name, typ.info)
  e.offer name
  e.dest.addDotToken() # pragmas
  e.traverseType(typ, {})
  e.dest.addParRi() # "fld"

proc traverseEnumField(e: var EContext; c: var Cursor; flags: set[TypeFlag] = {}) =
  e.dest.add c # efld
  inc c

  expectSymdef(e, c)
  let (s, sinfo) = getSymDef(e, c)
  e.dest.add symdefToken(s, sinfo)
  e.offer s

  skipExportMarker e, c

  skip c # pragmas: must be empty

  skip c # type: must be the enum itself

  inc c # skips TupleConstr
  traverseExpr e, c
  skip c
  skipParRi e, c

  takeParRi e, c

proc genStringType(e: var EContext; info: PackedLineInfo) =
  let s = pool.syms.getOrIncl(StringName)
  e.dest.add tagToken("type", info)
  e.dest.add symdefToken(s, info)
  e.offer s

  e.dest.addDotToken()
  e.dest.add tagToken("object", info)
  e.dest.addDotToken()

  e.dest.add tagToken("fld", info)
  let strField = pool.syms.getOrIncl(StringAField)
  e.dest.add symdefToken(strField, info)
  e.offer strField
  e.dest.addDotToken()
  e.dest.add tagToken("ptr", info)
  e.dest.add tagToken("c", info)
  e.dest.addIntLit(8, info)
  e.dest.addParRi() # "c"
  e.dest.addParRi() # "ptr"
  e.dest.addParRi() # "fld"

  e.dest.add tagToken("fld", info)
  let lenField = pool.syms.getOrIncl(StringIField)
  e.dest.add symdefToken(lenField, info)
  e.offer lenField
  e.dest.addDotToken()
  e.dest.add tagToken("i", info)
  e.dest.addIntLit(-1, info)
  e.dest.addParRi() # "i"
  e.dest.addParRi() # "fld"

  e.dest.addParRi() # "object"
  e.dest.addParRi() # "type"

proc useStringType(e: var EContext; info: PackedLineInfo) =
  let s = pool.syms.getOrIncl(StringName)
  e.dest.add symToken(s, info)
  e.demand s

proc traverseTupleBody(e: var EContext; c: var Cursor) =
  let info = c.info
  inc c
  e.dest.add tagToken("object", info)
  e.dest.addDotToken()
  var counter = 0
  while c.kind != ParRi:
    if c.substructureKind == KvU:
      inc c # skip tag
      skip c # skip name
      genTupleField(e, c, counter)
      skipParRi e, c
    else:
      genTupleField(e, c, counter)
    inc counter
  takeParRi e, c

proc traverseArrayBody(e: var EContext; c: var Cursor) =
  e.dest.add c
  inc c
  traverseType e, c
  if c.typeKind == RangetypeT:
    inc c
    skip c
    expectIntLit e, c
    let first = pool.integers[c.intId]
    inc c
    expectIntLit e, c
    let last = pool.integers[c.intId]
    inc c
    skipParRi e, c
    e.dest.addIntLit(last - first + 1, c.info)
  else:
    # should not be possible, but assume length anyway
    traverseExpr e, c
  takeParRi e, c

type
  GenPragmas = object
    opened: bool

proc openGenPragmas(): GenPragmas = GenPragmas(opened: false)

proc maybeOpen(e: var EContext; g: var GenPragmas; info: PackedLineInfo) {.inline.} =
  if not g.opened:
    g.opened = true
    e.dest.add tagToken("pragmas", info)

proc addKey(e: var EContext; g: var GenPragmas; key: string; info: PackedLineInfo) =
  maybeOpen e, g, info
  e.dest.add tagToken(key, info)
  e.dest.addParRi()

proc addKeyVal(e: var EContext; g: var GenPragmas; key: string; val: PackedToken; info: PackedLineInfo) =
  maybeOpen e, g, info
  e.dest.add tagToken(key, info)
  e.dest.add val
  e.dest.addParRi()

proc closeGenPragmas(e: var EContext; g: GenPragmas) =
  if g.opened:
    e.dest.addParRi()
  else:
    e.dest.addDotToken()

proc traverseParams(e: var EContext; c: var Cursor)

proc traverseProcTypeBody(e: var EContext; c: var Cursor) =
  e.dest.add tagToken("proctype", c.info)
  # This is really stupid...
  e.dest.addDotToken() # name
  inc c # proc
  # name, export marker, pattern, type vars:
  for i in 0..<4: skip c
  traverseParams e, c

  let pinfo = c.info
  let prag = parsePragmas(e, c)
  var genPragmas = openGenPragmas()
  if prag.callConv != NoCallConv:
    let name = $prag.callConv
    e.addKey genPragmas, name, pinfo
  closeGenPragmas e, genPragmas

  # ignore, effects and body:
  skip c
  skip c
  takeParRi e, c

proc traverseRefBody(e: var EContext; c: var Cursor; key: string) =
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
  e.dest.add tagToken("object", info)
  e.dest.addDotToken()

  e.dest.add tagToken("fld", info)
  let rcField = pool.syms.getOrIncl(RcField)
  e.dest.add symdefToken(rcField, info)
  e.offer rcField
  e.dest.addDotToken() # pragmas
  e.dest.add tagToken("i", info)
  e.dest.addIntLit(-1, info)
  e.dest.addParRi() # "i"
  e.dest.addParRi() # "fld"

  let dataField = pool.syms.getOrIncl(DataField)
  e.dest.add tagToken("fld", info)
  e.dest.add symdefToken(dataField, info)
  e.offer dataField
  e.dest.addDotToken() # pragmas
  e.traverseType(c, {})
  e.dest.addParRi() # "fld"

  e.dest.addParRi() # "object"

proc traverseAsNamedType(e: var EContext; c: var Cursor) =
  let info = c.info
  var body = c
  let k = body.typeKind
  let key = takeMangle c

  var val = e.newTypes.getOrDefault(key)
  if val == SymId(0):
    val = pool.syms.getOrIncl(key & GeneratedTypeSuffix)
    e.newTypes[key] = val

    var buf = createTokenBuf(30)
    swap e.dest, buf

    e.dest.add tagToken("type", info)
    e.dest.add symdefToken(val, info)
    e.offer val

    e.dest.addDotToken()
    case k
    of TupleT:
      traverseTupleBody e, body
    of ArrayT:
      traverseArrayBody e, body
    of ProctypeT:
      traverseProcTypeBody e, body
    of RefT:
      traverseRefBody e, body, key
    else:
      error e, "expected tuple or array, but got: ", body
    e.dest.addParRi() # "type"

    swap e.dest, buf
    e.pending.add buf
  # regardless of what we had to do, we still need to add the typename:
  if k == RefT:
    e.dest.add tagToken("ptr", info)
    e.dest.add symToken(val, info)
    e.dest.addParRi()
  else:
    e.dest.add symToken(val, info)

proc traverseType(e: var EContext; c: var Cursor; flags: set[TypeFlag] = {}) =
  case c.kind
  of DotToken:
    e.dest.add c
    inc c
  of Symbol:
    let s = c.symId
    let ext = maybeMangle(e, s)
    if ext.len != 0:
      e.dest.addSymUse pool.syms.getOrIncl(ext), c.info
      inc c
      return
    let res = tryLoadSym(s)
    if res.status == LacksNothing:
      var body = asTypeDecl(res.decl).body
      if body.typeKind == DistinctT: # skips DistinctT
        inc body
        traverseType(e, body, flags)
        inc c
      else:
        e.demand s
        e.dest.add c
        inc c
    else:
      error e, "could not find symbol: " & pool.syms[s]
  of ParLe:
    case c.typeKind
    of NoType, ErrT, OrT, AndT, NotT, TypedescT, UntypedT, TypedT, TypeKindT, OrdinalT:
      error e, "type expected but got: ", c
    of IntT, UIntT:
      let start = e.dest.len
      e.dest.add c
      inc c
      e.dest.add c
      inc c
      if c.kind != ParRi and c.pragmaKind in {ImportcP, ImportcppP}:
        e.dest.shrink start
        inc c
        e.dest.addSymUse pool.syms.getOrIncl(pool.strings[c.litId] & ".c"), c.info
        inc c
        skipParRi e, c
        skipParRi e, c
      else:
        takeParRi e, c
    of FloatT, CharT, BoolT, AutoT, SymKindT:
      e.loop c:
        e.dest.add c
        inc c
    of MutT, LentT:
      e.dest.add tagToken("ptr", c.info)
      inc c
      if isViewType(c):
        e.dest.shrink e.dest.len-1 # remove the "ptr" again
        traverseType e, c, {}
        skipParRi c
      else:
        e.loop c:
          traverseType e, c, {IsPointerOf}
    of PtrT, OutT:
      e.dest.add tagToken("ptr", c.info)
      inc c
      e.loop c:
        traverseType e, c, {IsPointerOf}
    of RefT:
      traverseAsNamedType e, c
    of ArrayT, ProctypeT:
      if IsNodecl in flags:
        traverseArrayBody e, c
      else:
        traverseAsNamedType e, c
    of RangetypeT:
      # skip to base type
      inc c
      traverseType e, c
      skip c
      skip c
      skipParRi e, c
    of UarrayT:
      if IsPointerOf in flags:
        inc c
        traverseType e, c
        skipParRi e, c
      else:
        e.dest.add tagToken("flexarray", c.info)
        inc c
        traverseType e, c
        takeParRi e, c
    of PointerT:
      e.dest.add tagToken("ptr", c.info)
      e.dest.add tagToken("void", c.info)
      e.dest.addParRi()
      inc c
      takeParRi e, c
    of CstringT:
      e.dest.add tagToken("ptr", c.info)
      e.dest.add tagToken($CharT, c.info)
      e.dest.addIntLit(8, c.info)
      e.dest.addParRi()
      inc c
      takeParRi e, c
    of StaticT, SinkT, DistinctT:
      inc c
      traverseType e, c, flags
      skipParRi e, c
    of TupleT:
      traverseAsNamedType e, c
    of ObjectT:
      e.dest.add c
      inc c
      if c.kind == DotToken:
        e.dest.add c
        inc c
      else:
        # inherited symbol
        let (s, sinfo) = getSym(e, c)
        e.dest.add symToken(s, sinfo)
        e.demand s

      if c.kind == DotToken:
        e.dest.add c
        inc c
      else:
        while c.substructureKind == FldU:
          traverseField(e, c, flags)

      takeParRi e, c
    of EnumT, HoleyEnumT:
      e.dest.add tagToken("enum", c.info)
      inc c
      traverseType e, c, flags # base type

      while c.substructureKind == EfldU:
        traverseEnumField(e, c, flags)

      takeParRi e, c
    of SetT:
      let info = c.info
      inc c
      let sizeOrig = bitsetSizeInBytes(c)
      var err = false
      let size = asSigned(sizeOrig, err)
      if err:
        error e, "invalid set element type: ", c
      else:
        case size
        of 1, 2, 4, 8:
          e.dest.add tagToken("u", info)
          e.dest.addIntLit(size * 8, info)
          e.dest.addParRi()
        else:
          var arrBuf = createTokenBuf(16)
          arrBuf.add tagToken("array", info)
          arrBuf.add tagToken("u", info)
          arrBuf.addIntLit(8, info)
          arrBuf.addParRi()
          arrBuf.addIntLit(size, info)
          arrBuf.addParRi()
          var arrCursor = cursorAt(arrBuf, 0)
          traverseAsNamedType(e, arrCursor)
      skip c
      skipParRi e, c
    of VoidT, VarargsT, NiltT, ConceptT,
       IteratorT, InvokeT, ParamsT, ItertypeT:
      error e, "unimplemented type: ", c
  else:
    error e, "type expected but got: ", c

proc maybeByConstRef(e: var EContext; c: var Cursor) =
  let param = asLocal(c)
  if param.typ.typeKind in {TypedescT, StaticT}:
    # do not produce any code for this as it's a compile-time parameter
    skip c
  elif passByConstRef(param.typ, param.pragmas, e.bits div 8):
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
    traverseLocal(e, paramCursor, ParamY, TraverseSig)
    endRead(paramBuf)
    skip c
  else:
    traverseLocal(e, c, ParamY, TraverseSig)

proc traverseParams(e: var EContext; c: var Cursor) =
  if c.kind == DotToken:
    e.dest.add c
    inc c
  elif c.kind == ParLe and c.typeKind == ParamsT:
    e.dest.add c
    inc c
    loop e, c:
      if c.symKind != ParamY:
        error e, "expected (param) but got: ", c
      maybeByConstRef(e, c)
  else:
    error e, "expected (params) but got: ", c
  # the result type
  traverseType e, c

proc parsePragmas(e: var EContext; c: var Cursor): CollectedPragmas =
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
        error e, "expected ')', but EOF reached"
      else: discard
      if c.kind == ParLe:
        let pk = c.pragmaKind
        case pk
        of NoPragma:
          let cc = c.callConvKind
          if cc == NoCallConv:
            error e, "unknown pragma: ", c
          else:
            result.callConv = cc
          inc c
        of MagicP:
          inc c
          if c.kind notin {StringLit, Ident}:
            error e, "expected string literal or ident, but got: ", c
          result.flags.incl NodeclP
          inc c
        of ImportcP, ImportcppP:
          inc c
          expectStrLit e, c
          result.externName = pool.strings[c.litId]
          result.flags.incl pk
          inc c
        of ExportcP, PluginP:
          inc c
          expectStrLit e, c
          result.externName = pool.strings[c.litId]
          inc c
        of NodeclP, SelectanyP, ThreadvarP, GlobalP, DiscardableP, NoReturnP,
           VarargsP, BorrowP, NoSideEffectP, NoDestroyP, ByCopyP, ByRefP,
           InlineP, NoinlineP, NoInitP, InjectP, GensymP, UntypedP, ViewP:
          result.flags.incl pk
          inc c
        of HeaderP:
          inc c
          expectStrLit e, c
          result.header = c.litId
          result.flags.incl NodeclP
          inc c
        of AlignP:
          inc c
          expectIntLit e, c
          result.align = c.intId
          inc c
        of BitsP:
          inc c
          expectIntLit e, c
          result.bits = c.intId
          inc c
        of RequiresP, EnsuresP, StringP, RaisesP, ErrorP:
          skip c
          continue
        of BuildP, EmitP:
          raiseAssert "unreachable"
        skipParRi e, c
      else:
        error e, "unknown pragma: ", c
  else:
    error e, "(pragmas) or '.' expected, but got: ", c

proc traverseProcBody(e: var EContext; c: var Cursor) =
  if c.stmtKind == StmtsS:
    e.dest.add c
    inc c
    var prevStmt = NoStmt
    while c.kind != ParRi:
      prevStmt = c.stmtKind
      traverseStmt e, c, TraverseAll
    if prevStmt == RetS or e.resultSym == SymId(0):
      discard "ok, do not add another return"
    else:
      e.dest.add parLeToken(RetS, c.info)
      e.dest.add symToken(e.resultSym, c.info)
      e.dest.addParRi()
    takeParRi e, c
  else:
    traverseStmt e, c, TraverseAll

proc traverseProc(e: var EContext; c: var Cursor; mode: TraverseMode) =
  e.openMangleScope()
  var dst = createTokenBuf(50)
  swap e.dest, dst
  #let toPatch = e.dest.len
  let oldResultSym = e.resultSym
  e.resultSym = SymId(0)

  let vinfo = c.info
  e.add "proc", vinfo
  inc c
  let (s, sinfo) = getSymDef(e, c)

  # namePos
  e.dest.add symdefToken(s, sinfo)
  e.offer s

  var isGeneric = false
  if c.kind == ParLe:
    isGeneric = true
  skipExportMarker e, c

  skip c # patterns

  if c.substructureKind == TypevarsU:
    isGeneric = true
    # count each typevar as used:
    inc c
    while c.kind != ParRi:
      assert c.symKind == TypevarY
      inc c
      let (typevar, _) = getSymDef(e, c)
      e.offer typevar
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
      let (param, _) = getSymDef(e, c)
      e.offer param
      skipToEnd c
    inc c
    skip c # skip return type
  else:
    traverseParams e, c

  let pinfo = c.info
  let prag = parsePragmas(e, c)

  let oldOwner = setOwner(e, s)

  var genPragmas = openGenPragmas()
  if prag.callConv != NoCallConv:
    let name = $prag.callConv
    e.addKey genPragmas, name, pinfo
  if InlineP in prag.flags:
    e.addKey genPragmas, "inline", pinfo

  if prag.externName.len > 0:
    e.registerMangleInParent(s, prag.externName & ".c")
    e.addKeyVal genPragmas, "was", symToken(s, pinfo), pinfo
  if SelectanyP in prag.flags:
    e.addKey genPragmas, "selectany", pinfo

  if BorrowP in prag.flags:
    e.addKey genPragmas, $InlineP, pinfo
  closeGenPragmas e, genPragmas

  skip c # miscPos

  # body:
  if isGeneric:
    skip c
  elif mode != TraverseSig or InlineP in prag.flags:
    traverseProcBody e, c
  else:
    e.dest.addDotToken()
    skip c
  takeParRi e, c
  swap dst, e.dest
  if NodeclP in prag.flags or isGeneric:
    discard "do not add to e.dest"
  elif prag.flags * {ImportcP, ImportcppP} != {} and e.inImpSection == 0:
    e.dest.add tagToken("imp", c.info)
    e.dest.add dst
    e.dest.addParRi()
  else:
    e.dest.add dst
  if prag.header != StrId(0):
    e.headers.incl prag.header
  discard setOwner(e, oldOwner)
  e.closeMangleScope()
  e.resultSym = oldResultSym

proc traverseTypeDecl(e: var EContext; c: var Cursor) =
  var dst = createTokenBuf(50)
  swap e.dest, dst
  #let toPatch = e.dest.len
  let vinfo = c.info
  e.add "type", vinfo
  inc c
  let (s, sinfo) = getSymDef(e, c)
  let oldOwner = setOwner(e, s)

  e.dest.add symdefToken(s, sinfo)
  e.offer s

  var isGeneric = c.kind == ParLe
  skipExportMarker e, c
  if c.substructureKind == TypevarsU:
    isGeneric = true
    # count each typevar as used:
    inc c
    while c.kind != ParRi:
      assert c.symKind == TypevarY
      inc c
      let (typevar, _) = getSymDef(e, c)
      e.offer typevar
      skipToEnd c
    inc c
  else:
    skip c # generic parameters

  let prag = parsePragmas(e, c)

  e.dest.addDotToken() # adds pragmas

  if prag.externName.len > 0:
    e.registerMangle(s, prag.externName & ".c")
  if c.typeKind in TypeclassKinds:
    isGeneric = true
  if isGeneric:
    skip c
  else:
    traverseType e, c, {IsTypeBody} + (if NodeclP in prag.flags: {IsNodecl} else: {})
  takeParRi e, c
  swap dst, e.dest
  if NodeclP in prag.flags or isGeneric:
    discard "do not add to e.dest"
  else:
    e.dest.add dst
  if prag.header != StrId(0):
    e.headers.incl prag.header
  discard setOwner(e, oldOwner)

proc genStringLit(e: var EContext; s: string; info: PackedLineInfo) =
  when false:
    # cannot use this logic because C is stupid crap.
    let existing = e.strLits.getOrDefault(s)
    if existing != SymId(0):
      e.dest.add symToken(existing, info)
    else:
      let strName = pool.syms.getOrIncl("str`." & $e.strLits.len)
      e.strLits[s] = strName
      e.pending.add tagToken("const", info)
      e.pending.add symdefToken(strName, info)
      e.offer strName

      e.pending.add tagToken("pragmas", info)
      e.pending.add tagToken("static", info)
      e.pending.addParRi()
      e.pending.addParRi()

      # type:
      e.pending.add symToken(pool.syms.getOrIncl(StringName), info)
      # value:
      e.pending.add tagToken("oconstr", info)
      e.pending.add symToken(pool.syms.getOrIncl(StringName), info)

      e.pending.add parLeToken(KvU, info)
      let strField = pool.syms.getOrIncl(StringAField)
      e.pending.add symToken(strField, info)
      e.pending.addStrLit(s)
      e.pending.addParRi() # "kv"

      e.pending.add parLeToken(KvU, info)
      let lenField = pool.syms.getOrIncl(StringIField)
      e.pending.add symToken(lenField, info)
      # length also contains the "isConst" flag:
      e.pending.addIntLit(s.len * 2, info)
      e.pending.addParRi() # "kv"

      e.pending.addParRi() # "oconstr"
      e.pending.addParRi() # "const"
      e.dest.add symToken(strName, info)
  else:
    e.dest.add tagToken("oconstr", info)
    useStringType e, info

    e.dest.add parLeToken(KvU, info)
    let strField = pool.syms.getOrIncl(StringAField)
    e.dest.add symToken(strField, info)
    e.dest.addStrLit(s)
    e.dest.addParRi() # "kv"

    e.dest.add parLeToken(KvU, info)
    let lenField = pool.syms.getOrIncl(StringIField)
    e.dest.add symToken(lenField, info)
    # length also contains the "isConst" flag:
    e.dest.addIntLit(s.len * 2, info)
    e.dest.addParRi() # "kv"

    e.dest.addParRi() # "oconstr"

proc genStringLit(e: var EContext; c: Cursor) =
  assert c.kind == StringLit
  let info = c.info
  let s {.cursor.} = pool.strings[c.litId]
  genStringLit(e, s, info)

proc traverseStmtsExpr(e: var EContext; c: var Cursor) =
  let head = c.load()
  inc c
  if isLastSon(c):
    traverseExpr e, c
    skipParRi e, c
  else:
    e.dest.add head
    while c.kind != ParRi:
      if not isLastSon(c):
        traverseStmt e, c
      else:
        traverseExpr e, c
    takeParRi e, c

proc traverseTupleConstr(e: var EContext; c: var Cursor) =
  e.dest.add tagToken("oconstr", c.info)
  var tupleType = e.typeCache.getType(c)
  e.traverseType(tupleType, {})
  inc c
  var counter = 0
  while c.kind != ParRi:
    e.dest.add tagToken("kv", c.info)
    e.dest.add symToken(ithTupleField(counter), c.info)
    inc counter
    if c.substructureKind == KvU:
      inc c # skip "kv"
      skip c # skip key
      traverseExpr e, c
      skipParRi e, c
    else:
      traverseExpr e, c
    e.dest.addParRi() # "kv"
  takeParRi e, c

proc traverseConv(e: var EContext; c: var Cursor) =
  let info = c.info
  let beforeConv = e.dest.len
  e.dest.add tagToken("conv", info)
  inc c
  let destType = c
  traverseType(e, c)
  let srcType = getType(e.typeCache, c)
  if destType.typeKind == CstringT and isStringType(srcType):
    var isSuffix = false
    if c.exprKind == SufX:
      isSuffix = true
      inc c
    if c.kind == StringLit:
      # evaluate the conversion at compile time:
      e.dest.shrink beforeConv
      e.dest.addStrLit pool.strings[c.litId]
      inc c
      if isSuffix:
        inc c
        skipParRi e, c
      skipParRi e, c
    else:
      let strField = pool.syms.getOrIncl(StringAField)
      e.dest.add tagToken("dot", info)
      traverseExpr(e, c)
      e.dest.add symToken(strField, info)
      e.dest.addIntLit(0, info)
      e.dest.addParRi()
      takeParRi e, c
  else:
    traverseExpr(e, c)
    takeParRi e, c

proc isSimpleLiteral(n: var Cursor): bool =
  case n.kind
  of IntLit, UIntLit, FloatLit, CharLit, StringLit, DotToken:
    result = true
    inc n
  else:
    case n.exprKind
    of FalseX, TrueX, InfX, NegInfX, NanX, NilX:
      result = true
      skip n
    of SufX:
      inc n
      result = isSimpleLiteral(n)
      skip n # type suffix
      skipParRi n
    of CastX, ConvX:
      result = true
      inc n
      skip n # type
      while n.kind != ParRi:
        if not isSimpleLiteral(n): return false
      skipParRi n
    else:
      result = false

proc traverseExpr(e: var EContext; c: var Cursor) =
  case c.kind
  of EofToken, ParRi:
    error e, "BUG: unexpected ')' or EofToken"
  of ParLe:
    case c.exprKind
    of EqX, NeqX, LeX, LtX:
      e.dest.add c
      inc c
      let beforeType = e.dest.len
      traverseType(e, c)
      e.dest.shrink beforeType
      traverseExpr(e, c)
      traverseExpr(e, c)
      takeParRi e, c
    of CastX:
      e.dest.add c
      inc c
      traverseType(e, c)
      traverseExpr(e, c)
      takeParRi e, c
    of HconvX, ConvX:
      traverseConv e, c
    of DconvX:
      inc c
      let beforeType = e.dest.len
      traverseType(e, c)
      e.dest.shrink beforeType
      traverseExpr(e, c)
      skipParRi(e, c)
    of AconstrX:
      e.dest.add tagToken("aconstr", c.info)
      inc c
      traverseType(e, c)
      while c.kind != ParRi:
        traverseExpr(e, c)
      takeParRi e, c
    of OconstrX:
      e.dest.add tagToken("oconstr", c.info)
      inc c
      traverseType(e, c)
      while c.kind != ParRi:
        traverseExpr(e, c)
      takeParRi e, c
    of TupX:
      traverseTupleConstr e, c
    of CmdX, CallStrLitX, InfixX, PrefixX, HcallX, CallX:
      e.dest.add tagToken("call", c.info)
      inc c
      while c.kind != ParRi:
        traverseExpr(e, c)
      takeParRi e, c
    of ExprX:
      traverseStmtsExpr e, c
    of ArrAtX:
      # XXX does not handle index type with offset low(I), maybe should be done in sem
      e.dest.add tagToken("at", c.info)
      inc c
      traverseExpr(e, c)
      traverseExpr(e, c)
      takeParRi e, c
    of TupAtX:
      e.dest.add tagToken("dot", c.info)
      inc c # skip tag
      traverseExpr e, c # tuple
      expectIntLit e, c
      e.dest.add symToken(ithTupleField(pool.integers[c.intId]), c.info)
      inc c # skip index
      e.dest.addIntLit(0, c.info) # inheritance
      takeParRi e, c
    of DotX:
      e.dest.add tagToken("dot", c.info)
      inc c # skip tag
      traverseExpr e, c # obj
      traverseExpr e, c # field
      traverseExpr e, c # inheritance depth
      takeParRi e, c
    of DdotX:
      e.dest.add tagToken("dot", c.info)
      e.dest.add tagToken("deref", c.info)
      inc c # skip tag
      traverseExpr e, c
      e.dest.addParRi()
      traverseExpr e, c
      traverseExpr e, c
      takeParRi e, c
    of HaddrX, AddrX:
      e.dest.add tagToken("addr", c.info)
      inc c
      traverseExpr(e, c)
      takeParRi e, c
    of HderefX, DerefX:
      e.dest.add tagToken("deref", c.info)
      inc c
      traverseExpr(e, c)
      takeParRi e, c
    of SufX:
      var suf = c
      inc suf
      let arg = suf
      skip suf
      assert suf.kind == StringLit
      if arg.kind == StringLit and pool.strings[suf.litId] == "R":
        # cstring conversion
        inc c
        e.dest.add c # add string lit directly
        inc c # arg
        inc c # suf
      else:
        e.dest.add c
        inc c
        traverseExpr e, c
        e.dest.add c
        inc c
      takeParRi e, c
    of AshrX:
      e.dest.add tagToken("shr", c.info)
      inc c
      var bits = -1'i64
      if c.typeKind in {IntT, UIntT}:
        var bitsToken = c
        inc bitsToken
        bits = pool.integers[bitsToken.intId]
      else:
        #error e, "expected int/uint type for ashr, got: ", c
        discard
      traverseType(e, c)
      e.dest.copyIntoKind CastX, c.info:
        e.dest.add tagToken("i", c.info)
        e.dest.addIntLit(bits, c.info)
        e.dest.addParRi()
        traverseExpr e, c
      e.dest.copyIntoKind CastX, c.info:
        e.dest.add tagToken("u", c.info)
        e.dest.addIntLit(bits, c.info)
        e.dest.addParRi()
        traverseExpr e, c
      takeParRi e, c
    of ErrX, NewobjX, SetConstrX, PlusSetX, MinusSetX, MulSetX, XorSetX, EqSetX, LeSetX, LtSetX,
       InSetX, CardX, BracketX, CurlyX, CompilesX, DeclaredX, DefinedX, HighX, LowX, TypeofX, UnpackX,
       EnumtostrX, IsmainmoduleX, DefaultobjX, DefaulttupX, DoX, CchoiceX, OchoiceX,
       EmoveX, DestroyX, DupX, CopyX, WasmovedX, SinkhX, TraceX, CurlyatX, PragmaxX, QuotedX, TabconstrX:
      error e, "BUG: not eliminated: ", c
      #skip c
    of AtX, PatX, ParX, NilX, InfX, NeginfX, NanX, FalseX, TrueX, AndX, OrX, NotX, NegX,
       SizeofX, AlignofX, OffsetofX, AddX, SubX, MulX, DivX, ModX, ShrX, ShlX,
       BitandX, BitorX, BitxorX, BitnotX, OconvX, NoExpr:
      # XXX Refine NoExpr case here. For now types like `(i -1)` can enter here.
      e.dest.add c
      inc c
      while c.kind != ParRi:
        traverseExpr e, c
      takeParRi e, c
  of SymbolDef:
    e.dest.add c
    e.offer c.symId
    inc c
  of Symbol:
    let inlineValue = getInitValue(e.typeCache, c.symId)
    var inlineValueCopy = inlineValue
    if not cursorIsNil(inlineValue) and isSimpleLiteral(inlineValueCopy):
      e.dest.addSubtree inlineValue
    else:
      let ext = maybeMangle(e, c.symId)
      if ext.len != 0:
        e.dest.addSymUse pool.syms.getOrIncl(ext), c.info
      else:
        e.dest.add c
      e.demand c.symId
    inc c
  of StringLit:
    genStringLit e, c
    inc c
  of UnknownToken, DotToken, Ident, CharLit, IntLit, UIntLit, FloatLit:
    e.dest.add c
    inc c

proc traverseLocal(e: var EContext; c: var Cursor; tag: SymKind; mode: TraverseMode) =
  var symKind = if tag == ResultY: VarY else: tag
  var localDecl = c
  let toPatch = e.dest.len
  let vinfo = c.info
  e.dest.addParLe symKind, vinfo
  inc c
  let (s, sinfo) = getSymDef(e, c)
  if tag == ResultY:
    e.resultSym = s
  skipExportMarker e, c
  let pinfo = c.info
  let prag = parsePragmas(e, c)

  e.dest.add symdefToken(s, sinfo)
  e.offer s

  var genPragmas = openGenPragmas()

  if prag.externName.len > 0:
    e.registerMangle(s, prag.externName & ".c")
    e.addKeyVal genPragmas, "was", symToken(s, pinfo), pinfo

  if ThreadvarP in prag.flags:
    e.dest[toPatch] = tagToken("tvar", vinfo)
    symKind = TvarY
  elif GlobalP in prag.flags:
    e.dest[toPatch] = tagToken("gvar", vinfo)
    symKind = GvarY

  if prag.align != IntId(0):
    e.addKeyVal genPragmas, "align", intToken(prag.align, pinfo), pinfo
  if prag.bits != IntId(0):
    e.addKeyVal genPragmas, "bits", intToken(prag.bits, pinfo), pinfo
  closeGenPragmas e, genPragmas

  var nodecl = prag.flags.contains(NodeclP)
  e.typeCache.registerLocal(s, symKind, c)
  if tag == ParamY and typeKind(c) == VarargsT:
    skip c
    nodecl = true
  else:
    traverseType e, c

  if mode == TraverseSig and localDecl.substructureKind == ParamU:
    # Parameter decls in NIFC have no dot token for the default value!
    skip c
  else:
    traverseExpr e, c
  takeParRi e, c
  if nodecl:
    e.dest.shrink toPatch
  if prag.header != StrId(0):
    e.headers.incl prag.header

proc traverseWhile(e: var EContext; c: var Cursor) =
  let info = c.info
  e.nestedIn.add (WhileS, SymId(0))
  e.dest.add c
  inc c
  traverseExpr e, c
  traverseStmt e, c
  takeParRi e, c
  let lab = e.nestedIn[^1][1]
  if lab != SymId(0):
    e.dest.add tagToken("lab", info)
    e.dest.add symdefToken(lab, info)
    e.offer lab
    e.dest.addParRi()
  discard e.nestedIn.pop()

proc traverseBlock(e: var EContext; c: var Cursor) =
  let info = c.info
  inc c
  if c.kind == DotToken:
    e.nestedIn.add (BlockS, SymId(0))
    inc c
  else:
    let (s, _) = getSymDef(e, c)
    e.nestedIn.add (BlockS, s)
  e.dest.add tagToken("scope", info)
  traverseStmt e, c
  takeParRi e, c
  let lab = e.nestedIn[^1][1]
  if lab != SymId(0):
    e.dest.add tagToken("lab", info)
    e.dest.add symdefToken(lab, info)
    e.offer lab
    e.dest.addParRi()
  discard e.nestedIn.pop()

proc traverseBreak(e: var EContext; c: var Cursor) =
  let info = c.info
  inc c
  if c.kind == DotToken:
    inc c
    e.dest.add tagToken("break", info)
  else:
    expectSym e, c
    let lab = c.symId
    inc c
    e.dest.add tagToken("jmp", info)
    e.dest.add symToken(lab, info)
  takeParRi e, c

proc traverseIf(e: var EContext; c: var Cursor) =
  # (if cond (.. then ..) (.. else ..))
  e.dest.add c
  inc c
  while c.kind == ParLe and c.substructureKind == ElifU:
    e.dest.add c
    inc c # skips '(elif'
    traverseExpr e, c
    traverseStmt e, c
    takeParRi e, c
  if c.kind == ParLe and c.substructureKind == ElseU:
    e.dest.add c
    inc c
    traverseStmt e, c
    takeParRi e, c
  takeParRi e, c

include stringcases

proc traverseStringCase(e: var EContext; c: var Cursor): bool =
  var n = c
  inc n
  let selectorType = getType(e.typeCache, n)
  if isSomeStringType(selectorType):
    transformStringCase(e, c)
    result = true
  else:
    result = false

proc traverseCase(e: var EContext; c: var Cursor) =
  if traverseStringCase(e, c):
    return
  e.dest.add c
  inc c
  traverseExpr e, c
  while c.kind != ParRi:
    case c.substructureKind
    of OfU:
      e.dest.add c
      inc c
      if c.kind == ParLe and c.substructureKind == RangesU:
        inc c
        e.add "ranges", c.info
        while c.kind != ParRi:
          traverseExpr e, c
        takeParRi e, c
      else:
        traverseExpr e, c
      traverseStmt e, c
      takeParRi e, c
    of ElseU:
      e.dest.add c
      inc c
      traverseStmt e, c
      takeParRi e, c
    else:
      error e, "expected (of) or (else) but got: ", c
  takeParRi e, c

proc traverseStmt(e: var EContext; c: var Cursor; mode = TraverseAll) =
  case c.kind
  of DotToken:
    e.dest.add c
    inc c
  of ParLe:
    case c.stmtKind
    of NoStmt:
      error e, "unknown statement: ", c
    of StmtsS:
      if mode == TraverseTopLevel:
        inc c
        while c.kind notin {EofToken, ParRi}:
          traverseStmt e, c, mode
        skipParRi e, c
      else:
        e.dest.add c
        inc c
        e.loop c:
          traverseStmt e, c, mode
    of ScopeS:
      e.openMangleScope()
      if mode == TraverseTopLevel:
        inc c
        while c.kind notin {EofToken, ParRi}:
          traverseStmt e, c, mode
        skipParRi e, c
      else:
        e.dest.add c
        inc c
        e.loop c:
          traverseStmt e, c, mode
      e.closeMangleScope()
    of VarS, LetS, CursorS:
      traverseLocal e, c, VarY, mode
    of ResultS:
      traverseLocal e, c, ResultY, mode
    of GvarS, GletS:
      traverseLocal e, c, GvarY, mode
    of TvarS, TletS:
      traverseLocal e, c, TvarY, mode
    of ConstS:
      traverseLocal e, c, ConstY, mode
    of CmdS, CallS:
      e.dest.add tagToken("call", c.info)
      inc c
      e.loop c:
        traverseExpr e, c
    of EmitS, AsmS:
      e.dest.add c
      inc c
      e.loop c:
        if c.kind == StringLit:
          e.dest.add c
          inc c
        else:
          traverseExpr e, c
    of AsgnS, RetS:
      e.dest.add c
      inc c
      e.loop c:
        traverseExpr e, c
    of DiscardS:
      let discardToken = c
      inc c
      if c.kind == DotToken:
        # eliminates discard without side effects
        inc c
        skipParRi e, c
      else:
        e.dest.add discardToken
        traverseExpr e, c
        takeParRi e, c
    of BreakS: traverseBreak e, c
    of WhileS: traverseWhile e, c
    of BlockS: traverseBlock e, c
    of IfS: traverseIf e, c
    of CaseS: traverseCase e, c
    of YldS, ForS, InclS, ExclS, DeferS, UnpackDeclS:
      error e, "BUG: not eliminated: ", c
    of TryS, RaiseS:
      error e, "BUG: not implemented: ", c
    of FuncS, ProcS, ConverterS, MethodS:
      traverseProc e, c, mode
    of MacroS, TemplateS, IncludeS, ImportS, FromS, ImportExceptS, ExportS, CommentS, IteratorS,
       ImportasS, ExportexceptS, BindS, MixinS, UsingS, StaticstmtS:
      # pure compile-time construct, ignore:
      skip c
    of TypeS:
      traverseTypeDecl e, c
    of ContinueS, WhenS:
      error e, "unreachable: ", c
    of PragmasS:
      skip c
  else:
    error e, "statement expected, but got: ", c

proc transformInlineRoutines(e: var EContext; c: var Cursor) =
  var swapped = createTokenBuf()
  swap e.dest, swapped

  var toTransform = createTokenBuf()
  toTransform.copyIntoKind StmtsS, c.info:
    takeTree(toTransform, c)
  var c0 = beginRead(toTransform)
  var dest = transform(e, c0, e.main)
  var c1 = beginRead(dest)
  inc c1 # skips (stmts

  swap e.dest, swapped

  traverseStmt e, c1, TraverseSig

proc importSymbol(e: var EContext; s: SymId) =
  let res = tryLoadSym(s)
  if res.status == LacksNothing:
    var c = res.decl
    let kind = c.symKind
    case kind
    of TypeY:
      traverseTypeDecl e, c
    of EfldY:
      # import full enum type:
      let typ = asLocal(c).typ
      assert typ.kind == Symbol
      e.demand typ.symId
    else:
      let isR = isRoutine(kind)
      if isR or isLocal(kind):
        var pragmas = if isR:
                        asRoutine(c).pragmas
                      else:
                        asLocal(c).pragmas
        let prag = parsePragmas(e, pragmas)
        if isR and InlineP in prag.flags:
          transformInlineRoutines(e, c)
          return
        if NodeclP in prag.flags:
          if prag.externName.len > 0:
            e.registerMangle(s, prag.externName & ".c")
          if prag.header != StrId(0):
            e.headers.incl prag.header
          return

      # XXX This is a stupid hack to avoid producing (imp (imp ...))
      inc e.inImpSection
      e.dest.add tagToken("imp", c.info)
      traverseStmt e, c, TraverseSig
      e.dest.addParRi()
      dec e.inImpSection
  else:
    error e, "could not find symbol: " & pool.syms[s]

proc writeOutput(e: var EContext, rootInfo: PackedLineInfo) =
  var b = nifbuilder.open(e.dir / e.main & ".c.nif")
  b.addHeader "hexer", "nifc"
  var stack: seq[PackedLineInfo] = @[]
  if rootInfo.isValid:
    stack.add rootInfo
    let rawInfo = unpack(pool.man, rootInfo)
    b.addLineInfo(rawInfo.col, rawInfo.line, pool.files[rawInfo.file])
  b.addTree "stmts"
  for h in e.headers:
    b.withTree "incl":
      b.addStrLit pool.strings[h]

  var c = beginRead(e.dest)
  var ownerStack = @[(SymId(0), -1)]

  var nested = 0
  var nextIsOwner = -1
  for n in 0 ..< e.dest.len:
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
      let val = e.maybeMangle(c.symId)
      if val.len > 0:
        b.addSymbol(val)
      else:
        b.addSymbol(pool.syms[c.symId])
    of SymbolDef:
      let val = e.maybeMangle(c.symId)
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
  var e = EContext(dir: (if dir.len == 0: getCurrentDir() else: dir), ext: ext, main: file,
    dest: createTokenBuf(),
    nestedIn: @[(StmtsS, SymId(0))],
    typeCache: createTypeCache(),
    bits: bits
    )
  e.openMangleScope()

  var c0 = setupProgram(infile, infile.changeFileExt ".c.nif", true)
  var dest = transform(e, c0, file)

  var c = beginRead(dest)
  let rootInfo = c.info

  if stmtKind(c) == StmtsS:
    inc c
    #genStringType e, c.info
    while c.kind != ParRi:
      traverseStmt e, c, TraverseTopLevel
  else:
    error e, "expected (stmts) but got: ", c

  # fix point expansion:
  var i = 0
  while i < e.requires.len:
    let imp = e.requires[i]
    if not e.declared.contains(imp):
      importSymbol(e, imp)
    inc i
  e.dest.add e.pending
  skipParRi e, c
  writeOutput e, rootInfo
  e.closeMangleScope()

when isMainModule:
  echo splitModulePath("/abc/def/name.4.nif")
