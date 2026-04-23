#       Nimony
# (c) Copyright 2025 Andreas Rumpf
#
# See the file "license.txt", included in this
# distribution, for details about the copyright.

import std/assertions
include nifprelude
import ".." / nimony / [nimony_model, decls, sigmatch]
import treemangler

type
  MangleMode* = enum
    Backend, Frontend


proc mangle*(b: var Mangler; c: Cursor; mm: MangleMode)

proc mangleProctype(b: var Mangler; n: var Cursor; mm: MangleMode): string =
  skipToParams n

  var b = createMangler(60)
  if n.kind != DotToken:
    inc n # params tag
    while n.kind != ParRi:
      let pa = takeLocal(n, SkipFinalParRi)
      assert pa.kind == ParamY
      mangle b, pa.typ, mm
  inc n # DotToken or ParRi
  # also add return type:
  mangle b, n, Backend
  skip n
  # handle pragmas:
  let props = extractProcProps(n)
  b.addKeyw $props.cc
  b.addKeyw $props.usesRaises
  b.addKeyw $props.usesClosure
  result = b.extract()
  if n.kind != ParRi:
    skip n # effects
    if n.kind != ParRi:
      skip n # body
  if n.kind != ParRi:
    bug "expected ')', but got: ", n
  inc n


proc mangleImpl(b: var Mangler; c: var Cursor; mm: MangleMode) =
  var nested = 0
  while true:
    case c.kind
    of ParLe:
      let tag {.cursor.} = pool.tags[c.tagId]
      if c.substructureKind == FldU:
        inc c
        skip c # name
        skip c # export marker
        skip c # pragmas
        mangleImpl b, c, mm # type is interesting
        skip c # value
        inc c # ParRi
      elif c.typeKind == ArrayT:
        b.addTree tag
        inc c
        mangleImpl b, c, mm # type is interesting
        if c.kind == ParLe and c.typeKind == RangetypeT:
          inc c # RangeT
          skip c # type is irrelevant, we care about the length
          assert c.kind == IntLit
          let first = pool.integers[c.intId]
          inc c
          assert c.kind == IntLit
          let last = pool.integers[c.intId]
          inc c
          inc c # ParRi
          b.addIntLit(last - first + 1)
        else:
          mangleImpl b, c, mm
        inc nested
      elif c.typeKind == TupleT:
        b.addTree(tag)
        inc c
        while c.kind != ParRi:
          if c.substructureKind == KvU:
            inc c
            skip c # name
            mangleImpl b, c, mm # type is interesting
            inc c # ParRi
          else:
            mangleImpl b, c, mm
        b.endTree()
        inc c # ParRi
      elif c.typeKind in {UintT, IntT, FloatT}:
        b.addTree(tag)
        inc c
        # normalize bits
        assert c.kind == IntLit
        let bits = pool.integers[c.intID]
        if bits < 0 and b.bits >= 0:
          b.addIntLit(b.bits)
        else:
          b.addIntLit(bits)
        inc c
        inc nested
      elif mm == Backend and c.typeKind in {RefT, PtrT, CstringT, PointerT}:
        b.addTree(tag)
        inc c
        # mangle children except nil annotations:
        while c.kind != ParRi:
          if isNilAnnotation(c):
            skip c
          else:
            mangleImpl b, c, mm
        assert c.kind == ParRi
        b.endTree()
        inc c
      elif mm == Backend and c.typeKind in RoutineTypes:
        b.addRaw mangleProctype(b, c, mm)
      elif isNilAnnotation(c):
        # skip nil/notnil/unchecked annotations in type keys
        skip c
      else:
        b.addTree(tag)
        inc nested
        inc c
    of ParRi:
      dec nested
      b.endTree()
      inc c
    of Symbol:
      b.addSymbol(pool.syms[c.symId])
      inc c
    of SymbolDef:
      b.addSymbolDef(pool.syms[c.symId])
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
    if nested == 0: break

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
