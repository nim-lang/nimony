#       Nimony
# (c) Copyright 2025 Andreas Rumpf
#
# See the file "license.txt", included in this
# distribution, for details about the copyright.

import std/assertions
include ".." / lib / nifprelude
import ".." / nimony / [nimony_model, decls, sigmatch]
import ".." / lib / [treemangler, symparser]

type
  MangleMode* = enum
    Backend, Frontend


proc mangleProctype(b: var Mangler; n: var Cursor; mm: MangleMode): string

proc mangleImpl(b: var Mangler; c: var Cursor; mm: MangleMode) =
  ## Mangles the single tree/token at `c` and advances past it.
  case c.kind
  of ParLe:
    let tag {.cursor.} = pool.tags[c.tagId]
    if c.substructureKind in {FldU, GfldU}:
      c.into:
        skip c, SkipName # name
        skip c, SkipExport # export marker
        skip c, SkipPragmas # pragmas
        mangleImpl b, c, mm # type is interesting
        skip c, SkipValue # value
    elif c.typeKind == ArrayT:
      b.addTree tag
      c.into:
        mangleImpl b, c, mm # type is interesting
        if c.kind == ParLe and c.typeKind == RangetypeT:
          var first, last: int64
          c.into:
            skip c # type is irrelevant, we care about the length
            assert c.kind == IntLit
            first = pool.integers[c.intId]
            inc c
            assert c.kind == IntLit
            last = pool.integers[c.intId]
            inc c
          b.addIntLit(last - first + 1)
        while c.hasMore:
          mangleImpl b, c, mm
      b.endTree()
    elif c.typeKind == TupleT:
      b.addTree(tag)
      c.into:
        while c.hasMore:
          if c.substructureKind == KvU:
            c.into KvU:
              skip c, SkipName # name
              mangleImpl b, c, mm # type is interesting
              while c.hasMore: skip c, SkipFull
          else:
            mangleImpl b, c, mm
      b.endTree()
    elif c.typeKind in {UIntT, IntT, FloatT}:
      b.addTree(tag)
      c.into:
        # normalize bits
        assert c.kind == IntLit
        let bits = pool.integers[c.intId]
        if bits < 0 and b.bits >= 0:
          b.addIntLit(b.bits)
        else:
          b.addIntLit(bits)
        inc c
        while c.hasMore:
          mangleImpl b, c, mm
      b.endTree()
    elif mm == Backend and c.typeKind in {RefT, PtrT, CstringT, PointerT}:
      b.addTree(tag)
      c.into:
        # mangle children except nil annotations:
        while c.hasMore:
          if isNilAnnotation(c):
            skip c
          else:
            mangleImpl b, c, mm
      b.endTree()
    elif mm == Backend and c.typeKind in RoutineTypes:
      b.addRaw mangleProctype(b, c, mm)
    elif isNilAnnotation(c):
      # skip nil/notnil/unchecked annotations in type keys
      skip c
    else:
      b.addTree(tag)
      c.into:
        while c.hasMore:
          mangleImpl b, c, mm
      b.endTree()
  of ParRi:
    discard "cannot happen: subtree ends are consumed by the bounded scope"
  of Symbol:
    # Strip the owning module's suffix from nested generic-instance
    # symbols (`Foo.0.I<hash>.modname`). Two modules that instantiate
    # the same generic with the same arguments would otherwise produce
    # mangle keys that differ only in the inner instance's owning
    # module — and the `c.newTypes` cache + `genericTypeName` (used by
    # lengcgen.trAsNamedType *and* duplifier.injectDup) would mint a
    # separate `(type :\`t.0.I<key>...)` per importer. The strip applies
    # in both `Frontend` and `Backend` modes because the two callers
    # disagree about which mode they pass — keeping the names aligned
    # across them is the whole point.
    let s = pool.syms[c.symId]
    if isInstantiation(s):
      b.addSymbol(removeModule(s))
    else:
      b.addSymbol(s)
    inc c
  of SymbolDef:
    let s = pool.syms[c.symId]
    if isInstantiation(s):
      b.addSymbolDef(removeModule(s))
    else:
      b.addSymbolDef(s)
    inc c
  of StringLit:
    b.addStrLit(pool.strings[c.litId])
    inc c
  of IntLit:
    b.addIntLit(pool.integers[c.intId])
    inc c
  of UIntLit:
    b.addUIntLit(pool.uintegers[c.uintId])
    inc c
  of FloatLit:
    b.addFloatLit(pool.floats[c.floatId])
    inc c
  of DotToken:
    b.addEmpty()
    inc c
  of CharLit:
    b.addCharLit(char c.uoperand)
    inc c
  of Ident:
    b.addIdent(pool.strings[c.litId])
    inc c
  of UnknownToken:
    b.addIdent "!unknown!"
    inc c
  of EofToken:
    b.addIdent "!eof!"
    inc c

proc takeMangle*(c: var Cursor; mm: MangleMode; bits = -1): string =
  var b = createMangler(30, bits)
  mangleImpl b, c, mm
  result = b.extract()

proc mangle*(c: Cursor; mm: MangleMode; bits = -1): string =
  var c = c
  takeMangle c, mm, bits

proc mangle*(b: var Mangler; c: Cursor; mm: MangleMode) =
  var c = c
  mangleImpl b, c, mm

proc mangleProctype(b: var Mangler; n: var Cursor; mm: MangleMode): string =
  let kind = n.typeKind
  var b = createMangler(60)
  n.into:
    skipRoutinePrefix n, kind
    if n.kind != DotToken:
      n.into:  # (params …)
        while n.hasMore:
          let pa = takeLocal(n, SkipFinalParRi)
          assert pa.kind == ParamY
          mangle b, pa.typ, mm
    else:
      inc n, AnyType  # the `.` placeholder for missing params
    # also add return type:
    mangle b, n, Backend
    skip n
    # handle pragmas:
    let props = extractProcProps(n)
    b.addKeyw $props.cc
    b.addKeyw $props.usesRaises
    b.addKeyw $props.usesClosure
    result = b.extract()
    if n.hasMore:
      skip n # effects
      if n.hasMore:
        skip n # body
    if n.hasMore:
      bug "expected ')', but got: ", n
