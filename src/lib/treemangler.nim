#       Nif library
# (c) Copyright 2024 Andreas Rumpf
#
# See the file "license.txt", included in this
# distribution, for details about the copyright.

## Turn NIF trees into identifiers. See the spec section "NIF trees as identifiers"
## for the used algorithm.

import std / [assertions, tables, formatfloat, strutils]

type
  Mangler* = object ## In the end `extract` must be called.
    buf: string
    nesting: int
    seen: Table[string, int]
    bits*: int

proc `=copy`(dest: var Mangler; src: Mangler) {.error.}

proc createMangler*(sizeHint: int; bits = -1): Mangler =
  Mangler(buf: newStringOfCap(sizeHint), bits: bits)

proc extract*(b: sink Mangler): string =
  ## Extracts the buffer from the mangler.
  ## The mangler should not be used afterwards.
  assert b.nesting == 0, "unpaired '(' or ')'"
  var i = b.buf.len-1
  while i > 0 and b.buf[i] == 'Z': dec i
  b.buf.setLen i+1
  result = move(b.buf)

proc put(b: var Mangler; s: string) {.inline.} =
  b.buf.add s

proc put(b: var Mangler; s: char) {.inline.} =
  b.buf.add s

proc undoWhitespace(b: var Mangler) =
  var i = b.buf.len - 1
  while i >= 0 and b.buf[i] == 'S': dec i
  b.buf.setLen i+1

proc escape(b: var Mangler; c: char) =
  const HexChars = "0123456789abcdef"
  var n = int(c)
  b.put 'X'
  b.put HexChars[n shr 4 and 0xF]
  b.put HexChars[n and 0xF]

const
  ControlLetters = {'A', 'Z', 'E', 'S', 'O', 'U', 'X', 'R'} # not yet necessary: 'K'

template needsEscape(c: char): bool =
  c notin ({'a'..'z', '0'..'9', '_'} - ControlLetters)

proc addRaw*(b: var Mangler; s: string) =
  put b, s

proc addSep(b: var Mangler) =
  if b.buf.len > 0 and b.buf[b.buf.len-1] == 'Z':
    discard "space not required"
  b.put 'S'

proc addBegin(b: var Mangler) =
  if b.buf.len > 0 and b.buf[b.buf.len-1] == 'S':
    b.buf[b.buf.len-1] = 'A'
  else:
    b.put 'A'

proc referencePrevious(b: var Mangler; s: string): bool =
  result = false
  if s.len > 2:
    let existing = b.seen.getOrDefault(s)
    let oldLen = b.buf.len
    if existing == 0:
      b.seen[s] = oldLen
    else:
      b.buf.add 'R'
      b.buf.addInt(existing-1)
      if b.buf.len - oldLen >= s.len:
        # undo, it was a pessimization!
        b.buf.setLen oldLen
      else:
        result = true

#  ------------ Atoms ------------------------

proc putTag*(b: var Mangler; tag: string) =
  if not referencePrevious(b, tag):
    for c in tag:
      if needsEscape c:
        b.escape c
      else:
        b.put c

proc addIdent*(b: var Mangler; s: string) =
  addSep b
  if not referencePrevious(b, s):
    for c in s:
      if c == '.' or c.needsEscape:
        b.escape c
      else:
        b.put c

proc addSymbolBody(b: var Mangler; s: string) =
  for part in s.split('.'):
    if not referencePrevious(b, part):
      for c in part:
        if c.needsEscape:
          b.escape c
        else:
          b.put c

proc addSymbol*(b: var Mangler; s: string) =
  addSep b
  if not referencePrevious(b, s):
    addSymbolBody(b, s)

proc addSymbolDef*(b: var Mangler; s: string) =
  addSep b
  b.put 'O'
  if not referencePrevious(b, s):
    addSymbolBody(b, s)

proc addStrLit*(b: var Mangler; s: string) =
  addSep b
  b.put 'U'
  for c in s:
    if needsEscape c:
      b.escape c
    else:
      b.put c
  b.put 'U'

proc addEmpty*(b: var Mangler; count = 1) =
  addSep b
  for i in 1..count:
    b.put 'E'

proc addCharLit*(b: var Mangler; c: char) =
  addSep b
  b.escape '\''
  if c.needsEscape:
    escape b, c
  else:
    b.put c
  b.escape '\''

proc addIntLit*(b: var Mangler; i: BiggestInt) =
  addSep b
  b.put $i

proc addUIntLit*(b: var Mangler; u: BiggestUInt) =
  addSep b
  b.put $u

proc addFloatLit*(b: var Mangler; f: BiggestFloat) =
  addSep b
  var x = ""
  x.addFloat f
  for c in x:
    if c == 'e':
      b.put 'E'
    elif needsEscape c:
      b.escape c
    else:
      b.put c

proc addKeyw*(b: var Mangler; keyw: string) =
  ## Adds a complete compound node that has no children like `(nil)`.
  b.addBegin
  b.putTag keyw
  b.put 'Z'

proc addTree*(b: var Mangler; kind: string) =
  ## Starts a new compound node. Must be closed with `endTree`.
  ## See also `withTree`.
  ## `kind` is allowed to start with a dot. This emits a directive then.
  b.addBegin
  b.putTag kind
  inc b.nesting

proc endTree*(b: var Mangler) =
  assert b.nesting > 0, "generating ')' would produce a syntax error"
  dec b.nesting
  undoWhitespace b
  b.put 'Z'

template withTree*(b: var Mangler; kind: string; body: untyped) =
  ## Convenience template that wraps `body` around `addTree` and `endTree`
  ## calls.
  addTree b, kind
  body
  endTree b

when isMainModule:
  proc test(b: sink Mangler) =
    b.withTree "stmts":
      b.withTree "call":
        b.addSymbol "foo.3.mymod"
        b.addIntLit 3423
        b.addFloatLit 50.4

    echo b.extract()

     # (array (range 0 9) (array (range 0 4 (i 8))))
    var m = createMangler(60)
    m.withTree "array":
      m.withTree "range":
        m.addIntLit 0
        m.addIntLit 9
      m.withTree "array":
        m.withTree "range":
          m.addIntLit 0
          m.addIntLit 4
        m.withTree "i":
          m.addIntLit 8

    assert m.extract() == "AarrayArangeS0S9ZAK0AK1S0S4ZAiS8"

  proc main() =
    var b = createMangler(10)
    test b

  main()
