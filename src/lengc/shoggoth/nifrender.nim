#
#
#           NIFC Render Helper
#        (c) Copyright 2026 Andreas Rumpf
#
#    See the file "license.txt", included in this
#    distribution, for details about the copyright.
#

## Renders a `TokenBuf` to canonical NIF text by walking it via
## cursors and driving `nifbuilder` in compact mode.
##
## Used by the optimizer pass tests to assert against the full output
## buffer rather than counting individual token kinds.

import std / assertions
include "../../lib" / nifprelude
import nifstreams, nifcursors, nifbuilder

proc emit(c: var Cursor; b: var Builder) =
  if c.isTagLit:
    b.addTree(pool.tags[c.tag])
    c.loopInto:
      emit(c, b)
    b.endTree()
  else:
    case c.kind
    of DotToken:  b.addEmpty()
    of Ident:     b.addIdent(c.strVal)
    of Symbol:    b.addSymbol(c.symName)
    of SymbolDef: b.addSymbolDef(c.symName)
    of IntLit:    b.addIntLit(c.intVal)
    of UIntLit:   b.addUIntLit(c.uintVal)
    of FloatLit:  b.addFloatLit(c.floatVal)
    of CharLit:   b.addCharLit(charLit(c))
    of StrLit:    b.addStrLit(c.strVal)
    else: discard
    inc c

proc render*(buf: var TokenBuf): string =
  ## Canonical compact NIF text for `buf` (`nifbuilder` adds the
  ## explicit closing parens).
  var b = nifbuilder.open(buf.len * 20, compact = true)
  var c = beginRead(buf)
  # Emit the single outermost block; `emit` descends into its children.
  emit(c, b)
  result = b.extract()
  # nifbuilder always emits a leading newline before the first tree;
  # drop it so the result lines up with `"""…"""` literals that don't
  # carry a leading newline.
  if result.len > 0 and result[0] == '\n':
    result = result[1 .. ^1]

template assertRender*(buf: var TokenBuf; expected: string) =
  ## Assert that `render(buf)` equals `expected`; on failure prints
  ## both for easy diffing.
  let actual = render(buf)
  doAssert actual == expected,
    "\ngot:      " & actual & "\nexpected: " & expected
