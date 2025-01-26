#       Nimony
# (c) Copyright 2024 Andreas Rumpf
#
# See the file "license.txt", included in this
# distribution, for details about the copyright.

import std / [assertions, tables]

include nifprelude
import nimony_model, decls, programs, xints, semdata, expreval

proc align(address, alignment: int): int {.inline.} =
  result = (address + (alignment - 1)) and not (alignment - 1)

type
  SizeofValue* = object
    size, maxAlign: int
    overflow, strict: bool

proc update(c: var SizeofValue; size, align: int) =
  c.maxAlign = max(c.maxAlign, align)
  c.size = align(c.size, align) + size

proc combine(c: var SizeofValue; inner: SizeofValue) =
  c.maxAlign = max(c.maxAlign, inner.maxAlign)
  c.size = c.size + inner.size
  c.overflow = c.overflow or inner.overflow

proc createSizeofValue(strict: bool): SizeofValue =
  SizeofValue(size: 0, maxAlign: 1, overflow: false, strict: strict)

proc finish(c: var SizeofValue) =
  c.size = align(c.size, c.maxAlign)

#[
Structs and tuples currently share the same layout algorithm,
noted as the "Universal" layout algorithm in the compiler implementation.
The algorithm is as follows:

Start with a size of 0 and an alignment of 1.

Iterate through the fields, in element order for tuples, or in var declaration order for structs. For each field:
Update size by rounding up to the alignment of the field, that is, increasing it to the least value greater or
equal to size and evenly divisible by the alignment of the field.
Assign the offset of the field to the current value of size.
Update size by adding the size of the field.
Update alignment to the max of alignment and the alignment of the field.
The final size and alignment are the size and alignment of the aggregate.
The stride of the type is the final size rounded up to alignment.
]#

proc `<`(x: xint; b: int): bool = x < createXint(b)

proc getSize(c: var SizeofValue; cache: var Table[SymId, SizeofValue]; n: Cursor; ptrSize: int) =
  var counter = 20
  var n = n
  let cacheKey = if n.kind == Symbol: n.symId else: NoSymId
  while counter > 0 and n.kind == Symbol:
    if cache.hasKey(n.symId):
      let c2 = cache[n.symId]
      combine c, c2
      return
    let sym = tryLoadSym(n.symId)
    if sym.status == LacksNothing:
      var local = asTypeDecl(sym.decl)
      if local.kind == TypeY:
        n = local.body
    else:
      raiseAssert "could not load: " & pool.syms[n.symId]

  case n.typeKind
  of IntT, UIntT, FloatT:
    let n = n.firstSon
    assert n.kind == IntLit
    let s = pool.integers[n.intId] div 8
    update c, s, s
  of CharT, BoolT:
    update c, 1, 1
  of RefT, PtrT, MutT, OutT, ProcT, NilT, CstringT, PointerT, LentT, RefObjectT, PtrObjectT:
    update c, ptrSize, ptrSize
  of SinkT, DistinctT:
    getSize c, cache, n.firstSon, ptrSize
  of EnumT, HoleyEnumT:
    let b = enumBounds(n)
    if b.lo < 0:
      update c, 4, 4 # always int32
    else:
      if b.hi < (1 shl 8):
        update c, 1, 1
      elif b.hi < (1 shl 16):
        update c, 2, 2
      elif b.hi < (1'i64 shl 32):
        update c, 4, 4
      else:
        update c, 8, 8
  of ObjectT:
    if c.strict:
      # mark as invalid as we pretend to not to know the alignment the backend ends up using etc.
      c.overflow = true
    var n = n
    inc n
    var c2 = createSizeofValue(c.strict)
    if n.kind != DotToken:
      getSize(c2, cache, n, ptrSize)
    while n.kind != ParRi:
      let field = takeLocal(n, SkipFinalParRi)
      getSize c2, cache, field.typ, ptrSize
    finish c2
    if cacheKey != NoSymId: cache[cacheKey] = c2
    combine c, c2

  of ArrayT:
    var c2 = createSizeofValue(c.strict)
    getSize(c2, cache, n.firstSon, ptrSize)
    let al1 = asSigned(getArrayLen(n), c.overflow)
    if al1 >= high(int) div c2.size:
      c.overflow = true
    else:
      update c, int(al1 * c2.size), c2.maxAlign
      c.overflow = c2.overflow
    if cacheKey != NoSymId: cache[cacheKey] = c2

  of SetT:
    let size0 = bitsetSizeInBytes(n.firstSon)
    let size1 = asSigned(size0, c.overflow)
    update c, size1, 1
  of TupleT:
    if c.strict:
      # mark as invalid as we pretend to not to know the alignment the backend ends up using etc.
      c.overflow = true
    var n = n
    inc n
    var c2 = createSizeofValue(c.strict)
    while n.kind != ParRi:
      if n.substructureKind == FldS:
        let field = takeLocal(n, SkipFinalParRi)
        getSize c2, cache, field.typ, ptrSize
      else:
        getSize c2, cache, n, ptrSize
        skip n
    finish c2
    if cacheKey != NoSymId: cache[cacheKey] = c2
    combine c, c2
  of OpenArrayT:
    update c, ptrSize*2, ptrSize
  of RangeT:
    getSize c, cache, n.firstSon, ptrSize
  of NoType, VoidT, VarargsT, OrT, AndT, NotT,
     ConceptT, StaticT, IterT, InvokeT, UncheckedArrayT,
     AutoT, SymKindT, TypeKindT, TypedescT, UntypedT, TypedT, OrdinalT:
    raiseAssert "BUG: valid type kind for sizeof computation: " & $n.typeKind

proc getSize*(n: Cursor; ptrSize: int; strict=false): xint =
  var c = createSizeofValue(strict)
  var cache = initTable[SymId, SizeofValue]()
  getSize(c, cache, n, ptrSize)
  if not c.overflow:
    result = createXint(c.size)
  else:
    result = createNaN()

proc typeIsBig*(n: Cursor; ptrSize: int): bool {.inline.} =
  let s = getSize(n, ptrSize)
  result = s > createXint(24'i64)

proc typeSectionMode(n: Cursor): PragmaKind =
  var n = n
  var counter = 20
  while counter > 0 and n.kind == Symbol:
    let sym = tryLoadSym(n.symId)
    if sym.status == LacksNothing:
      var local = asTypeDecl(sym.decl)
      if local.kind == TypeY:
        if hasBuiltinPragma(local.pragmas, ByCopy):
          return ByCopy
        elif hasBuiltinPragma(local.pragmas, ByRef):
          return ByRef
        n = local.body
  return NoPragma

proc passByConstRef*(typ, pragmas: Cursor; ptrSize: int): bool =
  let k = typ.typeKind
  if k in {SinkT, MutT, OutT}:
    result = false
  elif typeIsBig(typ, ptrSize):
    result = not hasBuiltinPragma(pragmas, ByCopy) and typeSectionMode(typ) != ByCopy
  else:
    result = hasBuiltinPragma(pragmas, ByRef) or typeSectionMode(typ) == ByRef
