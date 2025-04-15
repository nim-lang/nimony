import std/assertions
include nifprelude
import nimony_model, decls, xints, semdata, programs, nifconfig
import ".." / models / tags

const
  DefaultSetElements* = createXint(1'u64 shl 8)
  MaxSetElements* = createXint(1'u64 shl 16)

proc typebits*(config: NifConfig; n: PackedToken): int =
  if n.kind == IntLit:
    result = pool.integers[n.intId]
  elif n.kind == InlineInt:
    result = n.soperand
  else:
    result = 0
  if result == -1:
    result = config.bits

proc isOrdinalTypeKind*(kind: TypeKind): bool {.inline.} =
  result = kind in {EnumT, IntT, UIntT, CharT, BoolT, RangetypeT}

proc isOrdinalType*(typ: TypeCursor; allowEnumWithHoles: bool = false): bool =
  case typ.kind
  of Symbol:
    let s = tryLoadSym(typ.symId)
    if s.status != LacksNothing:
      return false
    if s.decl.symKind != TypeY:
      return false
    let decl = asTypeDecl(s.decl)
    case decl.body.typeKind
    of EnumT:
      result = true
    of HoleyEnumT:
      result = allowEnumWithHoles
    of DistinctT:
      # check base type
      var baseType = decl.body
      inc baseType # skip distinct tag
      result = isOrdinalType(baseType, allowEnumWithHoles)
    else:
      result = isOrdinalType(decl.body, allowEnumWithHoles)
  of ParLe:
    case typ.typeKind
    of IntT, UIntT, CharT, BoolT, RangetypeT:
      result = true
    of InvokeT:
      # check base type
      var base = typ
      inc base # skip invoke tag
      result = isOrdinalType(base, allowEnumWithHoles)
    else:
      result = false
  else:
    result = false

proc firstOrd*(c: var SemContext; typ: TypeCursor): xint =
  case typ.kind
  of Symbol:
    let s = tryLoadSym(typ.symId)
    if s.status != LacksNothing:
      result = createNaN()
      return
    if s.decl.symKind != TypeY:
      result = createNaN()
      return
    let decl = asTypeDecl(s.decl)
    case decl.body.typeKind
    of EnumT, HoleyEnumT:
      var field = asEnumDecl(decl.body).firstField
      var firstVal = asLocal(field).val
      inc firstVal # skip tuple tag
      case firstVal.kind
      of IntLit:
        result = createXint pool.integers[firstVal.intId]
      of UIntLit:
        result = createXint pool.uintegers[firstVal.uintId]
      else:
        # enum field with non int/uint value?
        result = createNaN()
    of DistinctT:
      # check base type
      var baseType = decl.body
      inc baseType # skip distinct tag
      result = firstOrd(c, baseType)
    else:
      result = firstOrd(c, decl.body)
  of ParLe:
    case typ.typeKind
    of IntT:
      var bits = typ
      inc bits # skip int tag
      case typebits(c.g.config, bits.load)
      of 8: result = createXint low(int8).int64
      of 16: result = createXint low(int16).int64
      of 32: result = createXint low(int32).int64
      of 64: result = createXint low(int64)
      else: result = createNaN()
    of UIntT, CharT, BoolT:
      result = zero()
    of RangetypeT:
      var first = typ
      inc first # tag
      skip first # base type
      case first.kind
      of IntLit:
        result = createXint pool.integers[first.intId]
      of UIntLit:
        result = createXint pool.uintegers[first.uintId]
      else:
        result = createNaN()
    of InvokeT:
      # check base type
      var base = typ
      inc base # skip invoke tag
      result = firstOrd(c, base)
    else:
      result = createNaN()
  else:
    result = createNaN()

proc lastOrd*(c: var SemContext; typ: TypeCursor): xint =
  case typ.kind
  of Symbol:
    let s = tryLoadSym(typ.symId)
    if s.status != LacksNothing:
      result = createNaN()
      return
    if s.decl.symKind != TypeY:
      result = createNaN()
      return
    let decl = asTypeDecl(s.decl)
    case decl.body.typeKind
    of EnumT, HoleyEnumT:
      var field = asEnumDecl(decl.body).firstField
      var last = field
      while field.kind != ParRi:
        last = field
        skip field
      var lastVal = asLocal(last).val
      inc lastVal # skip tuple tag
      case lastVal.kind
      of IntLit:
        result = createXint pool.integers[lastVal.intId]
      of UIntLit:
        result = createXint pool.uintegers[lastVal.uintId]
      else:
        # enum field with non int/uint value?
        result = createNaN()
    of DistinctT:
      # check base type
      var baseType = decl.body
      inc baseType # skip distinct tag
      result = lastOrd(c, baseType)
    else:
      result = lastOrd(c, decl.body)
  of ParLe:
    case typ.typeKind
    of IntT:
      var bits = typ
      inc bits # skip int tag
      case typebits(c.g.config, bits.load)
      of 8: result = createXint high(int8).int64
      of 16: result = createXint high(int16).int64
      of 32: result = createXint high(int32).int64
      of 64: result = createXint high(int64)
      else: result = createNaN()
    of UIntT, CharT:
      var bits = typ
      inc bits # skip int tag
      case typebits(c.g.config, bits.load)
      of 8: result = createXint high(uint8).uint64
      of 16: result = createXint high(uint16).uint64
      of 32: result = createXint high(uint32).uint64
      of 64: result = createXint high(uint64)
      else: result = createNaN()
    of RangetypeT:
      var last = typ
      inc last # tag
      skip last # base type
      skip last # first
      case last.kind
      of IntLit:
        result = createXint pool.integers[last.intId]
      of UIntLit:
        result = createXint pool.uintegers[last.uintId]
      else:
        result = createNaN()
    of BoolT:
      result = createXint 1.uint64
    of InvokeT:
      # check base type
      var base = typ
      inc base # skip invoke tag
      result = lastOrd(c, base)
    else:
      result = createNaN()
  else:
    result = createNaN()

proc lengthOrd*(c: var SemContext; typ: TypeCursor): xint =
  let first = firstOrd(c, typ)
  if first.isNaN: return first
  let last = lastOrd(c, typ)
  if last.isNaN: return last
  result = last - first + createXint(1.uint64)

proc containsGenericParams*(n: TypeCursor): bool =
  var n = n
  var nested = 0
  while true:
    case n.kind
    of Symbol:
      let res = tryLoadSym(n.symId)
      if res.status == LacksNothing and res.decl.tagEnum == TypevarTagId:
        return true
    of ParLe:
      inc nested
    of ParRi:
      dec nested
    else: discard
    if nested == 0: break
    inc n
  return false

proc nominalRoot*(t: TypeCursor; allowTypevar = false; skipPtrs = false): SymId =
  result = SymId(0)
  var t = t
  var ptrs = 1
  while true:
    case t.kind
    of Symbol:
      let res = tryLoadSym(t.symId)
      assert res.status == LacksNothing
      if res.decl.symKind == TypeY:
        let decl = asTypeDecl(res.decl)
        if decl.typevars.typeKind == InvokeT:
          var root = decl.typevars
          inc root
          assert root.kind == Symbol
          return root.symId
        else:
          return t.symId
      elif allowTypevar and res.decl.symKind == TypevarY:
        return t.symId
      else:
        break
    of ParLe:
      case t.typeKind
      of MutT, OutT, LentT, SinkT, StaticT, TypedescT:
        inc t
      of InvokeT:
        inc t
      of RefT, PtrT:
        if skipPtrs and ptrs > 0:
          inc t
          dec ptrs
        else:
          break
      else:
        break
    else:
      break

proc getClass*(t: TypeCursor): SymId =
  result = nominalRoot(t, false, true)

proc typeHasPragma*(n: Cursor; pragma: NimonyPragma; bodyKindRestriction = NoType): bool =
  var counter = 20
  var n = n
  while counter > 0 and n.kind == Symbol:
    dec counter
    let res = tryLoadSym(n.symId)
    assert res.status == LacksNothing
    let impl = asTypeDecl(res.decl)
    if impl.kind == TypeY:
      if bodyKindRestriction != NoType and impl.body.typeKind != bodyKindRestriction:
        return false
      if hasPragma(impl.pragmas, pragma):
        return true
      # Might be an alias, so traverse this one here:
      if impl.body.kind == Symbol:
        n = impl.body
      else:
        break
  return false

proc isViewType*(n: Cursor): bool =
  typeHasPragma(n, ViewP)

proc typeImpl*(s: SymId): Cursor =
  let res = tryLoadSym(s)
  assert res.status == LacksNothing
  result = res.decl
  assert result.stmtKind == TypeS
  inc result # skip ParLe
  for i in 1..4:
    skip(result) # name, export marker, pragmas, generic parameter

proc objtypeImpl*(s: SymId): Cursor =
  result = typeImpl(s)
  let k = typeKind result
  if k in {RefT, PtrT}:
    inc result

iterator inheritanceChain*(s: SymId): SymId =
  var objbody = objtypeImpl(s)
  while true:
    let od = asObjectDecl(objbody)
    if od.kind == ObjectT:
      var parent = od.parentType
      if parent.typeKind in {RefT, PtrT}:
        inc parent
      if parent.kind == Symbol:
        let ps = parent.symId
        yield ps
        objbody = objtypeImpl(ps)
      else:
        break
    else:
      break

proc isInheritable*(n: Cursor; skipPtrs = false): bool =
  var n = n
  if skipPtrs and n.typeKind in {RefT, PtrT}:
    inc n
  if typeHasPragma(n, FinalP, ObjectT): return false
  if n.kind == Symbol:
    if typeHasPragma(n, InheritableP, ObjectT): return true
    for parent in inheritanceChain(n.symId):
      # well it has a parent and is not final so it is inheritable:
      return true
  return false

proc isPure*(n: Cursor): bool =
  typeHasPragma(n, PureP)

proc isFinal*(n: Cursor): bool =
  var n = n
  if n.typeKind in {RefT, PtrT}: inc n
  result = typeHasPragma(n, FinalP, ObjectT)

proc hasRtti*(s: SymId): bool =
  var root = s
  for r in inheritanceChain(s):
    root = r

  let res = tryLoadSym(root)
  assert res.status == LacksNothing
  var n = res.decl
  assert n.stmtKind == TypeS
  inc n # skip ParLe
  skip n # name
  skip n # export marker
  skip n # type vars
  result = hasPragma(n, InheritableP) and not hasPragma(n, PureP)

proc hasRtti*(pragmas: Cursor): bool =
  hasPragma(pragmas, InheritableP) and not hasPragma(pragmas, PureP)

proc getTypeSection*(s: SymId): TypeDecl =
  let res = tryLoadSym(s)
  assert res.status == LacksNothing
  result = asTypeDecl(res.decl)

proc skipDistinct*(n: TypeCursor; isDistinct: var bool): TypeCursor =
  # XXX Consider generic types here and construct `DistinctType[Params...]` for these!
  var n = n
  var i = 0
  while i < 10:
    n = skipModifier(n)
    if n.kind == Symbol:
      let section = getTypeSection(n.symId)
      if section.kind == TypeY:
        let s = n
        n = section.body
        if n.typeKind == DistinctT:
          isDistinct = true
          inc n
        elif isNominal(n.typeKind):
          n = s
          break
      inc i
    else:
      break
  result = n
