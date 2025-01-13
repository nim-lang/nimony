#       Nimony
# (c) Copyright 2024 Andreas Rumpf
#
# See the file "license.txt", included in this
# distribution, for details about the copyright.

import std / [assertions, tables]

include nifprelude
import nimony_model, decls, programs, xints, semdata

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

proc createSizeofContext(strict: bool): SizeofValue =
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
  of StringT:
    update c, ptrSize*2, ptrSize
  of RefT, PtrT, MutT, OutT, ProcT, NilT:
    update c, ptrSize, ptrSize
  of SinkT, DistinctT:
    getSize c, cache, n.firstSon, ptrSize
  of EnumT:
    let symId = fromModuleSymUse(p, p[typ.m], typ.t)
    var c2 = createSizeofContext(c.strict)
    cacheValue(c2, symId):
      let s = addr(p[symId])
      assert s.kind == TypeDecl and hasDeclPos(s[])
      let (a, lastOrd) = enumBounds(p, p[s.declPos.m], s.declPos.t)
      if a < 0:
        update c2, 4, 4 # always int32
      else:
        if lastOrd < (1 *^ 8):
          update c2, 1, 1
        elif lastOrd < (1 *^ 16):
          update c2, 2, 2
        elif lastOrd < (1'i64 *^ 32):
          update c2, 4, 4
        else:
          update c2, 8, 8
    combine c, c2
  of ObjectBody:
    let fieldList = ithSon(p, typ, objectBodyPos)
    for ch in sonsReadonly(p[fieldList.m], fieldList.t):
      getSize c, p, FullTypeId(m: fieldList.m, t: ch), tree, n
  of ArrayTy:
    var c2 = createSizeofContext(c.strict)
    getSize(c2, p, p.ithSon(typ, 1), tree, n)
    let n = getArrayLen(p[typ.m], typ.t, p)
    if n >= high(int) div c2.size:
      c.overflow = true
    else:
      update c, int(n * c2.size), c2.maxAlign
      c.overflow = c2.overflow
  of SetTy:
    let size = bitsetSizeInBytes(p, typ.firstSon)
    update c, size, 1
  of ObjectTy:
    if c.strict:
      # don't even try to compute the size because alignment etc.
      c.overflow = true
    else:
      let symId = fromModuleSymUse(p, p[typ.m], typ.t)
      var c2 = createSizeofContext(c.strict)
      cacheValue(c2, symId):
        getSize(c2, p, p[symId].declPos, tree, n)
        finish c2
      combine c, c2
  of TypeDecl:
    getSize(c, p, ithSon(p, typ, typeBodyPos), tree, n)
  of TupleTy:
    if c.strict:
      # don't even try to compute the size because alignment etc.
      c.overflow = true
    else:
      var c2 = createSizeofContext(c.strict)
      for ch in sonsReadonly(p[typ.m], typ.t):
        getSize(c2, p, FullTypeId(m: typ.m, t: ch), tree, n)
      finish c2
      combine c, c2

proc getSize*(p: Program; typ: FullTypeId; strict=false): int =
  var c = createSizeofContext(strict)
  getSize(c, p, typ, p[typ.m], typ.t)
  if not c.overflow:
    result = c.size
  else:
    result = high(int)

proc typeIsBig*(typ: FullTypeId): bool {.inline.} =
  let s = getSize(p, typ)
  result = s > 24

proc passByConstRef*(p: Program; typ: FullTypeId; tree: Tree; pragmas: NodePos): bool =
  let k = p[typ].kind
  #echo typeToString(c.p, typ)
  if k in {TypeVar, GenericTy, SymKindTy, StaticTy, VarTy, OutTy}:
    result = false
  elif typeIsBig(p, typ) and k != SinkTy:
    result = not hasBuiltinPragma(p, tree, pragmas, "bycopy")
  else:
    result = hasBuiltinPragma(p, tree, pragmas, "byref")
