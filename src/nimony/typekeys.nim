#       Nimony
# (c) Copyright 2025 Andreas Rumpf
#
# See the file "license.txt", included in this
# distribution, for details about the copyright.

import std/assertions
include nifprelude
import ".." / nimony / nimony_model
import treemangler

type
  MangleMode* = enum
    Backend, Frontend

proc mangleImpl(b: var Mangler; c: var Cursor; mm: MangleMode) =
  var nested = 0
  while true:
    case c.kind
    of ParLe:
      let tag {.cursor.} = pool.tags[c.tagId]
      if tag == "fld":
        inc c
        skip c # name
        skip c # export marker
        skip c # pragmas
        mangleImpl b, c, mm # type is interesting
        skip c # value
        inc c # ParRi
      elif tag == "array":
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
      elif tag == "tuple":
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
      elif tag == "u" or tag == "i" or tag == "f":
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
      elif mm == Backend and tag in ["ref", "ptr"]:
        b.addTree(tag)
        inc c
        mangleImpl b, c, mm
        # skip optional not-nil markers:
        if c.kind != ParRi:
          skip c
        assert c.kind == ParRi
        b.endTree()
        inc c
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
