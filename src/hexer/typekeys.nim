#
#
#           Hexer Compiler
#        (c) Copyright 2025 Andreas Rumpf
#
#    See the file "copying.txt", included in this
#    distribution, for details about the copyright.
#

import std/assertions
include nifprelude
import ".." / nimony / nimony_model
import treemangler

proc mangleImpl(b: var Mangler; c: var Cursor) =
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
        mangleImpl b, c # type is interesting
        skip c # value
        inc c # ParRi
      elif tag == "array":
        b.addTree tag
        inc c
        mangleImpl b, c # type is interesting
        if c.kind == ParLe and c.typeKind == RangeT:
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
          mangleImpl b, c
        inc nested
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

proc takeMangle*(c: var Cursor): string =
  var b = createMangler(30)
  mangleImpl b, c
  result = b.extract()

proc mangle*(c: Cursor): string =
  var c = c
  takeMangle c
