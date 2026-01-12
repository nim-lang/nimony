#       Nif library
# (c) Copyright 2024 Andreas Rumpf
#
# See the file "license.txt", included in this
# distribution, for details about the copyright.

## Support code for generating NIF code.

import std / [assertions, syncio, formatfloat, math]
from std / strutils import endsWith

type
  Mode = enum
    UsesMem, UsesFile
  Builder* = object ## A builder can be in-memory or directly write into a file.
                    ## In the end either `extract` or `close` must be called.
    buffer: string
    mode: Mode
    compact: bool
    f: File
    nesting: int
    offs: int

  LineInfoFormat = enum
    LineInfoNone, LineInfoCol, LineInfoColLine, LineInfoFile

proc `=copy`(dest: var Builder; src: Builder) {.error.}

proc open*(filename: string; compact = false): Builder =
  ## Opens a new builder and attached it to some output filename.
  Builder(f: open(filename, fmWrite), mode: UsesFile, compact: compact)

proc open*(sizeHint: int; compact = false): Builder =
  ## Opens a new builder with the intent to keep the produced
  ## code in memory.
  Builder(buffer: newStringOfCap(sizeHint), mode: UsesMem, compact: compact)

proc attachedToFile*(b: Builder): bool {.inline.} = b.mode == UsesFile

proc extract*(b: sink Builder): string =
  ## Extracts the buffer from the builder.
  ## The builder should not be used afterwards.
  when not defined(showBroken):
    assert b.nesting == 0, "unpaired '(' or ')'" & $b.nesting
  assert b.mode == UsesMem, "cannot extract from a file"
  result = move(b.buffer)

proc close*(b: var Builder) =
  if b.mode == UsesFile:
    write b.f, b.buffer
    close b.f
  when not defined(showBroken):
    assert b.nesting == 0, "unpaired '(' or ')'"

proc putPending(b: var Builder; s: string) =
  b.buffer.add s
  b.offs += s.len

proc drainPending(b: var Builder) =
  if b.mode == UsesFile:
    # handle pending data:
    write b.f, b.buffer
    b.buffer.setLen 0

proc put(b: var Builder; s: string) =
  if b.mode == UsesFile:
    drainPending b
    write b.f, s
  else:
    b.buffer.add s
  b.offs += s.len

proc put(b: var Builder; s: char) =
  if b.mode == UsesFile:
    drainPending b
    write b.f, s
  else:
    b.buffer.add s
  b.offs += 1

proc undoWhitespace(b: var Builder) =
  var i = b.buffer.len - 1
  while i >= 0 and b.buffer[i] in {' ', '\n'}:
    dec i
    b.offs -= 1
  b.buffer.setLen i+1


const
  ControlChars* = {'(', ')', '[', ']', '{', '}', '~', '#', '\'', '"', '\\', ':'}

proc escape(b: var Builder; c: char) =
  const HexChars = "0123456789ABCDEF"
  var n = int(c)
  b.put '\\'
  b.put HexChars[n shr 4 and 0xF]
  b.put HexChars[n and 0xF]

template needsEscape(c: char): bool = c < ' ' or c in ControlChars

proc addRaw*(b: var Builder; s: string) =
  put b, s

proc addSep(b: var Builder) =
  if b.buffer.len > 0 and b.buffer[b.buffer.len-1] in {'\n', ' '}:
    discard "space not required"
  elif b.nesting != 0:
    b.putPending " "

proc addNumber*(b: var Builder; s: string) =
  addSep b
  if s.len > 0 and s[0] notin {'+', '-'}:
    b.put "+"
  put b, s

#  ------------ Atoms ------------------------

proc addIdent*(b: var Builder; s: string) =
  addSep b
  if s.len > 0:
    let c = s[0]
    if c < ' ' or c in {'.', '0'..'9', '+', '-', '~'} or c.needsEscape:
      b.escape c
    else:
      b.put c
    for i in 1..<s.len:
      let c = s[i]
      if c < ' ' or (c in ControlChars+{'.'}):
        b.escape c
      else:
        b.put c

proc addSymbolImpl(b: var Builder; s: string; len: int): int {.inline.} =
  ## Returns the number of dots in the symbol.
  result = 0
  if s.len > 0:
    let c = s[0]
    if c in {'.', '0'..'9', '+', '-', '~'} or c.needsEscape:
      b.escape c
    else:
      b.put c
    for i in 1..<len:
      let c = s[i]
      # Symbols imported from C can have a space like "struct foo".
      if c == ' ' or c.needsEscape:
        b.escape c
      else:
        if c == '.': inc result
        b.put c

proc addSymbol*(b: var Builder; s: string) =
  addSep b
  discard addSymbolImpl(b, s, s.len)

proc addSymbolDef*(b: var Builder; s: string) =
  addSep b
  b.put ':'
  discard addSymbolImpl(b, s, s.len)

proc addSymbol*(b: var Builder; s, dottedSuffix: string) =
  addSep b
  var L = s.len
  if dottedSuffix.len > 0 and s.endsWith(dottedSuffix):
    L -= dottedSuffix.len + 1
  discard addSymbolImpl(b, s, L)

proc addSymbolDefRetIsGlobal*(b: var Builder; s: string; dottedSuffix = ""): bool =
  ## Returns true if the symbol is global.
  addSep b
  b.put ':'
  var L = s.len
  if dottedSuffix.len > 0 and s.endsWith(dottedSuffix):
    L -= dottedSuffix.len + 1
  result = addSymbolImpl(b, s, L) >= 2

proc addStrLit*(b: var Builder; s: string) =
  addSep b
  b.put '"'
  for c in s.items:
    if needsEscape c:
      b.escape c
    else:
      b.put c
  b.put '"'

proc addEmpty*(b: var Builder; count = 1) =
  addSep b
  for i in 1..count:
    b.put '.'

proc addCharLit*(b: var Builder; c: char) =
  addSep b
  b.put '\''
  if c.needsEscape:
    escape b, c
  else:
    b.put c
  b.put '\''

proc addIntLit*(b: var Builder; i: int64) =
  addSep b
  if i >= 0:
    b.buffer.add '+'
    b.offs += 1
  b.put $i

proc addUIntLit*(b: var Builder; u: uint64) =
  addSep b
  b.buffer.add '+'
  b.put $u
  b.buffer.add 'u'
  b.offs += 2

proc addFloatLit*(b: var Builder; f: float) =
  addSep b
  drainPending b
  let myLen = b.buffer.len
  case classify(f)
  of fcInf: b.buffer.add "(inf)"
  of fcNan: b.buffer.add "(nan)"
  of fcNegInf: b.buffer.add "(neginf)"
  of fcNegZero: b.buffer.add "-0.0"
  of fcNormal, fcSubnormal, fcZero:
    if f >= 0.0:
      b.buffer.add '+'
    b.buffer.addFloat f
    for i in myLen ..< b.buffer.len:
      if b.buffer[i] == 'e': b.buffer[i] = 'E'
  b.offs += b.buffer.len - myLen
  if b.mode == UsesFile:
    b.f.write b.buffer
    b.buffer.setLen 0

proc addLine(b: var Builder; x: int32) =
  let oldLen = b.buffer.len
  if x < 0:
    b.buffer.add '~'
    b.buffer.addInt(-x)
  else:
    b.buffer.addInt(x)
  b.offs += b.buffer.len - oldLen

template addLineIgnoreZero(b: var Builder; x: int32) =
  # Adds a number if it is not zero.
  if x != 0:
    addLine b, x

proc addLineInfo*(b: var Builder; col, line: int32; file = "") =
  var format = LineInfoNone
  if col != 0'i32:
    format = LineInfoCol
  if line != 0'i32:
    format = LineInfoColLine
  if file.len > 0:
    format = LineInfoFile
  drainPending b
  case format
  of LineInfoCol:
    addSep b
    b.addLine col
  of LineInfoColLine:
    addSep b
    b.addLineIgnoreZero col
    b.buffer.add ','
    b.offs += 1
    b.addLine line
  of LineInfoFile:
    addSep b
    b.addLine col
    b.buffer.add ','
    b.addLine line
    b.buffer.add ','
    b.offs += 2
    for c in file.items:
      if c.needsEscape:
        b.escape c
      else:
        b.buffer.add c
        b.offs += 1
  of LineInfoNone:
    discard "same line info"

proc addKeyw*(b: var Builder; keyw: string) =
  ## Adds a complete compound node that has no children like `(nil)`.
  drainPending b
  b.put '('
  b.put keyw
  b.put ')'

proc addTree*(b: var Builder; kind: string) =
  ## Starts a new compound node. Must be closed with `endTree`.
  ## See also `withTree`.
  ## `kind` is allowed to start with a dot. This emits a directive then.
  drainPending b
  if not b.compact:
    if b.nesting > 0:
      b.put "\n"
      for i in 1..b.nesting: b.put ' '
    b.put '('
  else:
    b.put "\n("
  b.put kind
  inc b.nesting

proc endTree*(b: var Builder) =
  when not defined(showBroken):
    assert b.nesting > 0, "generating ')' would produce a syntax error"
  if b.nesting >= 0:
    dec b.nesting
  undoWhitespace b
  b.put ')'

template withTree*(b: var Builder; kind: string; body: untyped) =
  ## Convenience template that wraps `body` around `addTree` and `endTree`
  ## calls.
  addTree b, kind
  body
  endTree b

proc addUIntLit*(b: var Builder; u: uint64; suffix: string) =
  withTree(b, "suf"):
    addUIntLit(b, u)
    addStrLit(b, suffix)

proc addStrLit*(b: var Builder; s: string; suffix: string) =
  withTree(b, "suf"):
    addStrLit(b, s)
    addStrLit(b, suffix)

proc addHeader*(b: var Builder; vendor = "", dialect = "") =
  b.put "(.nif24)\n"
  if vendor.len > 0:
    b.put "(.vendor "
    b.addStrLit vendor
    b.put ")\n"
  if dialect.len > 0:
    b.put "(.dialect "
    b.addStrLit dialect
    b.put ")\n"

proc addHeader26*(b: var Builder): int =
  ## Returns the patch position for the indexat overwrite.
  b.put "(.nif26)\n"
  b.put "(.indexat                  )\n"
  #                 ^ whitespace essential here for patching without reallocations!
  result = b.offs + len("(.indexat ")
  #b.put "(.indexat 1_000_000_000)\n"

proc patchIndexAt*(b: var Builder; patchPos: int; indexAt: int) =
  var s = "+"
  s.addInt indexAt
  for i in 0..<s.len:
    b.buffer[patchPos + i] = s[i]

proc offset*(b: Builder): int {.inline.} =
  ## Returns the current offset for index generation. The produced value might point to
  ## whitespace that must first be skipped before the desired element is reached but
  ## the nifreader will skip the whitespace automatically, so no harm is done.
  result = b.offs

when isMainModule:
  proc test(b: sink Builder) =
    b.addHeader "tester", "niftest"
    b.withTree "stmts":
      b.addLineInfo 4, 5, "mymodb.nim"
      b.withTree "call":
        b.addSymbolDef "oh.0.my.god"
        b.addLineInfo 1, 3, "mymod.nim"
        b.addSymbol "foo.3.mymod"
        b.addIntLit 3423
        b.addFloatLit 50.4
        # issue #1313
        b.addFloatLit -0.0

    if b.attachedToFile:
      b.close
    else:
      echo b.extract()

  proc main() =
    #var b = open(10)
    #test b

    var b2 = open"builder_example.nif"
    test b2

  main()
