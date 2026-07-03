#
#
#           Leng Compiler
#        (c) Copyright 2026 Andreas Rumpf
#
#    See the file "license.txt", included in this
#    distribution, for details about the copyright.
#

## Type layout for the JavaScript backend (Typed-Array model).
##
## Under Araq's M2 direction (PR #2043) Nim `object`/`array`/`seq`/`string` are
## laid out as bytes in a single `ArrayBuffer` — a byte-addressable linear memory
## — rather than as native JS objects/arrays, so that `cast` and low-level libs
## work. That requires the one thing neither the C nor the LLVM backend computes:
## explicit `sizeof`/alignment/field byte-offsets (both defer struct layout to
## their toolchains). This module computes them from the Leng type grammar using
## the platform C ABI (natural alignment, LLVM datalayout `e-…-i64:64-…`).
##
## It is a pure query over already-loaded Leng types: no code is emitted here.
## `typeLayout` gives `(size, align)` for any type; `objectFields` gives the
## per-field byte offsets an `oconstr`/`dot` must target.

import ".." / lib / nifcoreparse   # re-exports nifcore (Cursor, into, skip, intVal, ...)
import ".." / lib / nifcdecl        # typeKind/substructureKind + decl readers
import nifmodules                   # MainModule + getDeclOrNil

const PtrSize* = 4'i64   ## target pointer size. The JS target is a `--bits:32`
                         ## platform (Araq's steer: `int`/`uint` are platform-
                         ## specific → 32-bit JS `Number`; only explicit `int64`/
                         ## `uint64` become `BigInt`). Pointers are 4-byte byte
                         ## offsets into the linear-memory buffer; `(i -1)` uses it.

type
  Layout* = object
    size*, align*: int64

  FieldInfo* = object
    sym*: SymId       ## the field symbol (its key for `oconstr`/`dot`)
    offset*: int64    ## byte offset from the start of the object
    typ*: Cursor      ## the field's type (so codegen can pick the load/store width)

  AccessKind* = enum
    ## How a value of a type is loaded/stored at a byte offset in linear memory.
    ## Scalars pick a typed `DataView` accessor; `akAggregate` has none — an
    ## object/array/union is accessed by its sub-offsets or copied wholesale.
    akI8, akI16, akI32, akI64
    akU8, akU16, akU32, akU64
    akF32, akF64
    akPtr        ## a pointer, stored as a pointer-size integer offset
    akAggregate  ## object/array/union: no scalar load

proc roundUp(x, a: int64): int64 {.inline.} =
  if a <= 1: x else: (x + a - 1) and not (a - 1)

proc bitsOf(t: Cursor; dflt: int64): int64 =
  ## The bit-width operand of a primitive type node, e.g. `64` of `(i 64)`;
  ## `dflt` when there is none (`(bool)`). `(i -1)` reports -1 (pointer-size int).
  var n = t
  result = dflt
  n.into:
    if n.hasMore and n.kind == IntLit:
      result = intVal(n)
    while n.hasMore: skip n

proc typeLayout*(m: var MainModule; t: Cursor): Layout
proc objectFields*(m: var MainModule; t: Cursor): seq[FieldInfo]

proc layoutObject(m: var MainModule; t: Cursor): (Layout, seq[FieldInfo]) =
  ## Lay out an `(object <base|.> (fld …)+)` in declaration order: each field at
  ## the next offset rounded up to its alignment; object align = max field align;
  ## object size = end offset rounded up to that align. An inheritance base (a
  ## leading `Symbol`) is embedded first, exactly as the C backend's `Q` member;
  ## its fields sit at the front (offsets unchanged) and are included here so an
  ## `oconstr`/`dot` can target *inherited* fields, not just the ones declared on
  ## this type.
  var off = 0'i64
  var maxAlign = 1'i64
  var fields: seq[FieldInfo] = @[]
  var n = t
  n.into:
    if n.kind == Symbol:
      let base = typeLayout(m, n)   # embedded base occupies the front
      for bf in objectFields(m, n): # inherited fields keep their (front) offsets
        fields.add bf
      off = base.size
      maxAlign = max(maxAlign, base.align)
      inc n
    elif n.kind == DotToken:
      inc n
    while n.hasMore:
      if n.substructureKind == FldU:
        let f = takeFieldDecl(n)    # advances n past the whole field
        let fl = typeLayout(m, f.typ)
        off = roundUp(off, fl.align)
        fields.add FieldInfo(sym: f.name.symId, offset: off, typ: f.typ)
        off += fl.size
        maxAlign = max(maxAlign, fl.align)
      elif n.typeKind == UnionT:
        # A variant's `union`: its branches OVERLAY, so every branch's fields
        # start at the same offset. Each branch is an anonymous `object` wrapping
        # that branch's fields (Nim `case` objects); lay each out and flatten its
        # fields into this map at the shared union offset, reserving only the
        # largest branch. Without this an `oconstr`/`dot` on a branch field finds
        # no entry and mis-lays it (offset 0 / size 0).
        var uAlign = 1'i64
        var uSize = 0'i64
        var brFields: seq[FieldInfo] = @[]   # branch fields, still 0-based
        var u = n
        u.into:
          while u.hasMore:
            if u.typeKind in {ObjectT, UnionT}:
              let (bl, bf) = layoutObject(m, u)   # a branch object: 0-based fields
              uAlign = max(uAlign, bl.align)
              uSize = max(uSize, bl.size)
              for f in bf: brFields.add f
              skip u
            elif u.substructureKind == FldU:      # a bare `fld` branch (robustness)
              let f = takeFieldDecl(u)
              let fl = typeLayout(m, f.typ)
              uAlign = max(uAlign, fl.align)
              uSize = max(uSize, fl.size)
              brFields.add FieldInfo(sym: f.name.symId, offset: 0, typ: f.typ)
            else:
              skip u
        off = roundUp(off, uAlign)
        for f in brFields:
          fields.add FieldInfo(sym: f.sym, offset: off + f.offset, typ: f.typ)
        off += uSize
        maxAlign = max(maxAlign, uAlign)
        skip n
      elif n.typeKind == ObjectT:
        # a bare anonymous nested object (not a variant union): reserve its span.
        let (al, _) = layoutObject(m, n)
        off = roundUp(off, al.align)
        off += al.size
        maxAlign = max(maxAlign, al.align)
        skip n
      else:
        skip n
  result = (Layout(size: roundUp(off, maxAlign), align: maxAlign), fields)

proc unionLayout(m: var MainModule; t: Cursor): Layout =
  ## A `union` overlays its members: size = max member size, align = max member
  ## align. A member is either a bare `fld` or an anonymous `object` wrapping a
  ## variant branch's fields (Nim `case` objects) — handle both.
  var maxSize = 0'i64
  var maxAlign = 1'i64
  var n = t
  n.into:
    while n.hasMore:
      if n.substructureKind == FldU:
        let f = takeFieldDecl(n)
        let fl = typeLayout(m, f.typ)
        maxSize = max(maxSize, fl.size)
        maxAlign = max(maxAlign, fl.align)
      elif n.typeKind in {ObjectT, UnionT}:
        let (bl, _) = layoutObject(m, n)
        maxSize = max(maxSize, bl.size)
        maxAlign = max(maxAlign, bl.align)
        skip n
      else:
        skip n
  result = Layout(size: roundUp(maxSize, maxAlign), align: maxAlign)

proc typeLayout*(m: var MainModule; t: Cursor): Layout =
  ## `(size, align)` of any Leng type, resolving named types through their decls.
  case t.typeKind
  of IT, UT:
    let bits = bitsOf(t, PtrSize * 8)
    let sz = (if bits <= 0: PtrSize else: bits div 8)   # (i -1) => pointer-size
    result = Layout(size: sz, align: sz)
  of FT:
    let sz = bitsOf(t, 64) div 8
    result = Layout(size: sz, align: sz)
  of BoolT:
    result = Layout(size: 1, align: 1)
  of CT:
    let sz = max(1'i64, bitsOf(t, 8) div 8)
    result = Layout(size: sz, align: sz)
  of PtrT, AptrT, ProctypeT:
    result = Layout(size: PtrSize, align: PtrSize)
  of VoidT:
    result = Layout(size: 0, align: 1)
  of ArrayT:
    var n = t
    n.into:
      let el = typeLayout(m, n)
      skip n                        # past element type -> count
      var count = 0'i64
      if n.hasMore:
        if n.kind == IntLit: count = intVal(n)
        elif n.kind == UIntLit: count = int64(uintVal(n))
        skip n
      while n.hasMore: skip n
      result = Layout(size: roundUp(el.size, el.align) * count, align: el.align)
  of EnumT:
    var n = t
    n.into:
      result = typeLayout(m, n)     # enum is represented as its base type
      while n.hasMore: skip n
  of FlexarrayT:
    var n = t
    n.into:
      let el = typeLayout(m, n)     # trailing flexible member: no fixed size
      result = Layout(size: 0, align: el.align)
      while n.hasMore: skip n
  of ObjectT:
    result = layoutObject(m, t)[0]
  of UnionT:
    result = unionLayout(m, t)
  of NoType:
    if t.kind == Symbol:
      let def = m.getDeclOrNil(t.symId)
      if def != nil:
        var p = def.pos
        result = typeLayout(m, asTypeDecl(p).body)
      else:
        result = Layout(size: PtrSize, align: PtrSize)   # unknown: assume a word
    else:
      result = Layout(size: 0, align: 1)
  else:
    result = Layout(size: PtrSize, align: PtrSize)

proc accessOf*(m: var MainModule; t: Cursor): AccessKind =
  ## The scalar access class of a type (or `akAggregate`), resolving named types
  ## and enums (an enum accesses as its base type). This is what a `dot`/`at`
  ## load or `oconstr`/`store` write picks its `DataView` method from.
  result = akAggregate
  case t.typeKind
  of IT:
    let b0 = bitsOf(t, PtrSize * 8)
    let b = if b0 <= 0: PtrSize * 8 else: b0   # (i -1) => pointer-size (platform int)
    result = (if b == 8: akI8 elif b == 16: akI16 elif b == 32: akI32 else: akI64)
  of UT:
    let b0 = bitsOf(t, PtrSize * 8)
    let b = if b0 <= 0: PtrSize * 8 else: b0
    result = (if b == 8: akU8 elif b == 16: akU16 elif b == 32: akU32 else: akU64)
  of FT:
    result = (if bitsOf(t, 64) == 32: akF32 else: akF64)
  of BoolT, CT:
    result = akU8
  of PtrT, AptrT, ProctypeT:
    result = akPtr
  of EnumT:
    var n = t
    n.into:
      result = accessOf(m, n)
      while n.hasMore: skip n
  of ObjectT, UnionT, ArrayT, FlexarrayT:
    result = akAggregate
  of NoType:
    if t.kind == Symbol:
      let def = m.getDeclOrNil(t.symId)
      if def != nil:
        var p = def.pos
        result = accessOf(m, asTypeDecl(p).body)
      else:
        result = akPtr
    else:
      result = akAggregate
  else:
    result = akAggregate

proc objectFields*(m: var MainModule; t: Cursor): seq[FieldInfo] =
  ## Per-field byte offsets for an object (empty for non-objects). Resolves a
  ## named type to its declaration first.
  var ty = t
  if ty.typeKind == NoType and ty.kind == Symbol:
    let def = m.getDeclOrNil(ty.symId)
    if def == nil: return @[]
    var p = def.pos
    return objectFields(m, asTypeDecl(p).body)
  if ty.typeKind == ObjectT:
    result = layoutObject(m, ty)[1]
  else:
    result = @[]
