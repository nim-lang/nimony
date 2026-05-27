#
#
#           NIFC Render Helper
#        (c) Copyright 2026 Andreas Rumpf
#
#    See the file "license.txt", included in this
#    distribution, for details about the copyright.
#

## Renders a `TokenBuf` (possibly encoded with `-d:virtualParRi`, whose
## closing parens are elided) to canonical NIF text by walking it via
## cursors and driving `nifbuilder` in compact mode.
##
## Used by the optimizer pass tests to assert against the full output
## buffer rather than counting individual token kinds.

import std / assertions
include "../../lib" / nifprelude
import nifstreams, nifcursors, nifbuilder

proc emit(c: var Cursor; b: var Builder) =
  case c.kind
  of ParLe:
    b.addTree(pool.tags[c.tagId])
    c.loopInto:
      emit(c, b)
    b.endTree()
    return
  of DotToken:    b.addEmpty()
  of Ident:       b.addIdent(pool.strings[c.litId])
  of Symbol:      b.addSymbol(pool.syms[c.symId])
  of SymbolDef:   b.addSymbolDef(pool.syms[c.symId])
  of IntLit:      b.addIntLit(pool.integers[c.intId])
  of UIntLit:     b.addUIntLit(pool.uintegers[c.uintId])
  of FloatLit:    b.addFloatLit(pool.floats[c.floatId])
  of CharLit:     b.addCharLit(char(c.uoperand))
  of StringLit:   b.addStrLit(pool.strings[c.litId])
  of UnknownToken, EofToken, ParRi: discard
  inc c

proc render*(buf: var TokenBuf): string =
  ## Canonical compact NIF text for `buf`. Handles virtualParRi-encoded
  ## buffers (the cursor walk understands the encoding; `nifbuilder`
  ## adds the explicit closing parens).
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
