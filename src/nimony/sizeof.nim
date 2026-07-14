#       Nimony
# (c) Copyright 2024 Andreas Rumpf
#
# See the file "license.txt", included in this
# distribution, for details about the copyright.

import std / [assertions, tables]

include ".." / lib / nifprelude
include ".." / lib / compat2
import nimony_model, decls, programs, xints, semdata, expreval

proc align(address, alignment: int): int {.inline.} =
  result = (address + (alignment - 1)) and not (alignment - 1)

type
  SizeofValue* = object
    size, maxAlign: int   # when maxAlign == 0, it is packed and fields of the object are placed without paddings.
    overflow, strict: bool

proc update(c: var SizeofValue; size, align: int) =
  if c.maxAlign == 0:
    c.size += size
  else:
    c.maxAlign = max(c.maxAlign, align)
    c.size = align(c.size, align) + size

proc combine(c: var SizeofValue; inner: SizeofValue) =
  if c.maxAlign != 0:
    c.maxAlign = max(c.maxAlign, inner.maxAlign)
  c.size = c.size + inner.size
  c.overflow = c.overflow or inner.overflow

proc combineCaseObject(c: var SizeofValue; inner: SizeofValue) =
  c.maxAlign = max(c.maxAlign, inner.maxAlign)
  c.size = max(c.size, inner.size)
  c.overflow = c.overflow or inner.overflow

proc createSizeofValue(strict: bool, packed = false): SizeofValue =
  SizeofValue(size: 0, maxAlign: if packed: 0 else: 1, overflow: false, strict: strict)

proc finish(c: var SizeofValue) =
  if c.maxAlign != 0:
    c.size = align(c.size, c.maxAlign)

#[Structs and tuples currently share the same layout algorithm, noted as the "Universal" layout algorithm in the compiler implementation.
The algorithm is as follows:

Start with a size of 0 and an alignment of 1.

Iterate through the fields, in element order for tuples, or in var declaration order for structs. For each field:
Update size by rounding up to the alignment of the field, that is, increasing it to the least value greater or
equal to size and evenly divisible by the alignment of the field.
Assign the offset of the field to the current value of size.
Update size by adding the size of the field.
Update alignment to the max of alignment and the alignment of the field.
The final size and alignment are the size and alignment of the aggregate.
The stride of the type is the final size rounded up to alignment.]#

type
  TypePragmas = object
    pragmas: set[NimonyPragma]

proc parseTypePragmas(n: Cursor): TypePragmas =
  result = default TypePragmas
  var n = n
  if n.substructureKind == PragmasU:
    n.into PragmasU:
      while n.hasMore:
        case n.pragmaKind:
        of {PackedP, UnionP, InheritableP, IncompleteStructP}:
          result.pragmas.incl n.pragmaKind
          skip n
        else:
          skip n
  elif n.kind != DotToken:
    error "illformed AST inside type section: ", n

proc `<`(x: xint; b: int64): bool = x < createXint(b)

proc getSize(c: var SizeofValue; cache: var Table[SymId, SizeofValue]; n: Cursor; ptrSize: int)

proc getSizeObject(c: var SizeofValue; cache: var Table[SymId, SizeofValue]; iter: var ObjFieldIter; n: var Cursor; ptrSize: int; pragmas: TypePragmas): bool =
  result = nextField(iter, n, keepCase = true)
  if result:
    if n.substructureKind == CaseU:
      assert UnionP notin pragmas.pragmas, "Case objects cannot work with union pragma."
      var cCase = createSizeofValue(c.strict)
      n.into CaseU:
        # selector
        let field = takeLocal(n, SkipFinalParRi)
        getSize c, cache, field.typ, ptrSize
        while n.hasMore:
          case n.substructureKind
          of OfU:
            n.into OfU:
              # ranges
              skip n
              var cOf = createSizeofValue(c.strict)
              n.into StmtsU:
                while n.hasMore:
                  if n.substructureKind == NilU:
                    skip n # empty branch body
                  else:
                    discard getSizeObject(cOf, cache, iter, n, ptrSize, pragmas)
              finish cOf
              combineCaseObject(cCase, cOf)
              while n.hasMore: skip n
          of ElseU:
            n.into ElseU:
              var cElse = createSizeofValue(c.strict)
              n.into StmtsU:
                while n.hasMore:
                  if n.substructureKind == NilU:
                    skip n # empty branch body
                  else:
                    discard getSizeObject(cElse, cache, iter, n, ptrSize, pragmas)
              finish cElse
              combineCaseObject(cCase, cElse)
              while n.hasMore: skip n
          else:
            error "illformed AST inside case object: ", n
            skip n  # avoid infinite loop on unexpected
      combine(c, cCase)
    else:
      let field = takeLocal(n, SkipFinalParRi)
      if UnionP in pragmas.pragmas:
        var c2 = createSizeofValue(c.strict)
        getSize c2, cache, field.typ, ptrSize
        combineCaseObject(c, c2)
      else:
        getSize c, cache, field.typ, ptrSize

proc getSize(c: var SizeofValue; cache: var Table[SymId, SizeofValue]; n: Cursor; ptrSize: int) =
  var counter = 20
  var n = n
  let cacheKey = if n.kind == Symbol: n.symId else: NoSymId
  var pragmas = default(TypePragmas)
  while counter > 0 and n.kind == Symbol:
    if cache.hasKey(n.symId):
      # `hasKey` just returned true, so `getOrQuit` will not quit.
      let c2 = cache.getOrQuit(n.symId)
      combine c, c2
      return
    let sym = tryLoadSym(n.symId)
    if sym.status == LacksNothing:
      var local = asTypeDecl(sym.decl)
      if local.kind == TypeY:
        n = local.body
        if n.kind != Symbol:
          pragmas = parseTypePragmas local.pragmas
    else:
      bug "could not load: " & pool.syms[n.symId]

  case n.typeKind
  of IntT, UIntT, FloatT:
    let bits = typebits(n.firstSon.load)
    let s = int(if bits != -1: bits div 8 else: ptrSize)
    update c, s, s
  of CharT, BoolT:
    update c, 1, 1
  of RefT, PtrT, MutT, OutT, RoutineTypes, NiltT, CstringT, PointerT, LentT:
    update c, ptrSize, ptrSize
  of SinkT, DistinctT:
    getSize c, cache, n.firstSon, ptrSize
  of EnumT, HoleyEnumT, AnumT:
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
    if c.strict or (IncompleteStructP in pragmas.pragmas):
      # mark as invalid as we pretend to not to know the alignment the backend ends up using etc.
      c.overflow = true
    var n = n
    inc n
    var c2 = createSizeofValue(c.strict, PackedP in pragmas.pragmas)
    if n.kind != DotToken:  # base type
      getSize(c2, cache, n, ptrSize)
    elif InheritableP in pragmas.pragmas:
      # No explicit base, but inheritable: account for the RTTI pointer in
      # `c2` so the *cached* size matches the actual size. Updating `c`
      # directly works for the immediate caller but stores size=0 for an
      # inheritable empty-base object in the cache, poisoning every later
      # lookup (and every derived type's base-size computation).
      update c2, ptrSize, ptrSize

    skip n
    var iter = initObjFieldIter()
    while getSizeObject(c2, cache, iter, n, ptrSize, pragmas):
      discard
    finish c2
    if cacheKey != NoSymId: cache[cacheKey] = c2
    combine c, c2

  of ArrayT:
    var elem = createSizeofValue(c.strict)
    getSize(elem, cache, n.firstSon, ptrSize)
    let al1 = asSigned(getArrayLen(n), c.overflow)
    var arr = createSizeofValue(c.strict)
    if elem.overflow or elem.size <= 0 or al1 >= high(int) div elem.size:
      arr.overflow = true
    else:
      update arr, int(al1 * elem.size), elem.maxAlign
      arr.overflow = elem.overflow
    if cacheKey != NoSymId: cache[cacheKey] = arr
    combine c, arr

  of SetT:
    let size0 = bitsetSizeInBytes(n.firstSon)
    let size1 = int asSigned(size0, c.overflow)
    update c, size1, 1
  of TupleT:
    if c.strict:
      # mark as invalid as we pretend to not to know the alignment the backend ends up using etc.
      c.overflow = true
    var n = n
    var c2 = createSizeofValue(c.strict)
    n.into:  # (tuple …)
      while n.hasMore:
        getSize c2, cache, getTupleFieldType(n), ptrSize
        skip n
    finish c2
    if cacheKey != NoSymId: cache[cacheKey] = c2
    combine c, c2
  of RangetypeT:
    getSize c, cache, n.firstSon, ptrSize
  of NoType, ErrT, VoidT, VarargsT, OrT, AndT, NotT,
     ConceptT, StaticT, InvokeT, UarrayT,
     AutoT, SymkindT, TypekindT, TypedescT, UntypedT, TypedT, OrdinalT:
    c.overflow = true

type
  SizeofCache* = Table[SymId, SizeofValue]
    ## Long-lived size-by-symbol memoization, shared across many `getSize`
    ## calls. Internal recursion already keys on `SymId`, but the public
    ## entrypoint used to throw the table away after each call. Pass a
    ## context-owned cache in to amortize the size walk across all the type
    ## queries a single hexer pass performs.

proc getSize*(n: Cursor; ptrSize: int; cache: var SizeofCache; strict=false): xint =
  var c = createSizeofValue(strict)
  getSize(c, cache, n, ptrSize)
  if not c.overflow:
    result = createXint(c.size)
  else:
    result = createNaN()

proc getSize*(n: Cursor; ptrSize: int; strict=false): xint =
  var cache = initTable[SymId, SizeofValue]()
  result = getSize(n, ptrSize, cache, strict)

proc typeIsBig*(n: Cursor; ptrSize: int; cache: var SizeofCache): bool {.inline.} =
  let s = getSize(n, ptrSize, cache)
  result = s > createXint(24'i64)

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
        if hasPragma(local.pragmas, BycopyP):
          return BycopyP
        elif hasPragma(local.pragmas, ByrefP):
          return ByrefP
        n = local.body
  return NoPragma

proc passByConstRef*(typ, pragmas: Cursor; ptrSize: int; cache: var SizeofCache): bool =
  let k = typ.typeKind
  if k in {SinkT, MutT, OutT, TypekindT, UntypedT, TypedT, VarargsT, TypedescT, StaticT}:
    result = false
  elif typeIsBig(typ, ptrSize, cache):
    result = not hasPragma(pragmas, BycopyP) and typeSectionMode(typ) != BycopyP
  else:
    result = hasPragma(pragmas, ByrefP) or typeSectionMode(typ) == ByrefP

proc passByConstRef*(typ, pragmas: Cursor; ptrSize: int): bool =
  var cache = initTable[SymId, SizeofValue]()
  result = passByConstRef(typ, pragmas, ptrSize, cache)

when isMainModule:
  setupProgramForTesting("", "", "")

  proc testSizeof(srcNif: string; expectedSize: int) =
    let symId = block:
      var srcBuf = parseFromBuffer(srcNif, "<invalid>")
      var n = beginRead srcBuf
      assert n.stmtKind == TypeS
      inc n
      assert n.kind == SymbolDef
      let result = n.symId
      endRead srcBuf
      publish result, srcBuf, SemcheckBodies
      result
    var symBuf = createTokenBuf(1)
    symBuf.add symToken(symId, NoLineInfo)
    let n = beginRead symBuf
    var sz = getSize(n, 8)
    endRead symBuf
    var err = false
    let size = sz.asSigned(err)
    assert size == expectedSize, "expected " & $expectedSize & " but got " & $size

  # type names in the following test Nif must be unique
  testSizeof("""
   (type :IntObj.0.mod123abc . . .
    (object .
     (fld :x.0.mod123abc . .
      (i 64) .)))""", 8)

  testSizeof("""
   (type :IntIntObj.0.mod123abc . . .
    (object .
     (fld :x.0.mod123abc . .
      (i 64) .)
     (fld :y.0.mod123abc . .
      (i 64) .)))""", 16)

  testSizeof("""
   (type :CharObj.0.mod123abc . . .
    (object .
     (fld :x.0.mod123abc . .
      (c 8) .)))""", 1)

  testSizeof("""
   (type :CharIntObj.0.mod123abc . . .
    (object .
     (fld :x.0.mod123abc . .
      (c 8) .)
     (fld :y.0.mod123abc . .
      (i 64) .)))""", 16)

  testSizeof("""
   (type :IntCharObj.0.mod123abc . . .
    (object .
     (fld :x.0.mod123abc . .
      (i 64) .)
     (fld :y.0.mod123abc . .
      (c 8) .)))""", 16)

  testSizeof("""
   (type :IntIntObjPacked.0.mod123abc . .
    (pragmas
     (packed))
    (object .
     (fld :x.1.mod123abc . .
      (i 64) .)
     (fld :y.1.mod123abc . .
      (i 64) .)))""", 16)

  testSizeof("""
   (type :IntCharObjPacked.0.mod123abc . .
    (pragmas
     (packed))
    (object .
     (fld :x.1.mod123abc . .
      (i 64) .)
     (fld :y.1.mod123abc . .
      (c 8) .)))""", 9)

  testSizeof("""
   (type :CharIntObjPacked.0.mod123abc . .
    (pragmas
     (packed))
    (object .
     (fld :x.1.mod123abc . .
      (c 8) .)
     (fld :y.1.mod123abc . .
      (i 64) .)))""", 9)

  testSizeof("""
   (type :CharPackedFieldObj.0.mod123abc . . .
    (object .
     (fld :x.1.mod123abc . .
      (c 8) .)
     (fld :y.1.mod123abc . .
      IntIntObjPacked.0.mod123abc .)))""", 17)

  testSizeof("""
   (type :PackedFieldCharObj.0.mod123abc . . .
    (object .
     (fld :x.1.mod123abc . .
      IntIntObjPacked.0.mod123abc .)
     (fld :y.1.mod123abc . .
      (c 8) .)))""", 17)

  testSizeof("""
   (type :CharIntObjPackedNested.0.mod123abc . .
    (pragmas
     (packed))
    (object .
     (fld :x.1.mod123abc . .
      (c 8) .)
     (fld :y.1.mod123abc . .
      IntIntObj.0.mod123abc .)))""", 17)

  testSizeof("""
   (type :IntUnion.0.mod123abc . .
    (pragmas
     (union))
    (object .
     (fld :y.0.mod123abc . .
      (i 64) .)))""", 8)

  testSizeof("""
   (type :CharIntUnion.0.mod123abc . .
    (pragmas
     (union))
    (object .
     (fld :x.0.mod123abc . .
      (c 8) .)
     (fld :y.0.mod123abc . .
      (i 64) .)))""", 8)

  testSizeof("""
   (type :IntObjFloatUnion.0.mod123abc . .
    (pragmas
     (union))
    (object .
     (fld :x.0.mod123abc . .
      (i 64) .)
     (fld :y.0.mod123abc . .
      IntIntObj.0.mod123abc .)
     (fld :z.0.mod123abc . .
      (f 32) .)))""", 16)

  testSizeof("""
   (type :XYEnum.0.mod123abc . . .
    (enum
     (u 8)
     (efld :X.0.mod123abc . . E.0.mod123abc
      (tup 0 "X"))
     (efld :Y.0.mod123abc . . E.0.mod123abc
      (tup 1 "Y"))))""", 1)

  testSizeof("""
   (type :CharCharCaseObj.0.mod123abc . . .
    (object .
     (case
      (fld :k.0.mod123abc . . XYEnum.0.mod123abc .)
      (of
       (ranges X.0.mod123abc)
       (stmts
        (fld :c.0.mod123abc . .
         (c 8) .)))
      (of
       (ranges Y.0.mod123abc)
       (stmts
        (fld :x.0.mod123abc . .
         (c 8) .))))))""", 2)

  testSizeof("""
   (type :IntCharCaseObj.0.mod123abc . . .
    (object .
     (case
      (fld :k.0.mod123abc . . XYEnum.0.mod123abc .)
      (of
       (ranges X.0.mod123abc)
       (stmts
        (fld :c.0.mod123abc . .
         (i 64) .)))
      (of
       (ranges Y.0.mod123abc)
       (stmts
        (fld :x.0.mod123abc . .
         (c 8) .))))))""", 16)
