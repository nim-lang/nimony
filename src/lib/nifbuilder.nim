#       Nif library
# (c) Copyright 2024 Andreas Rumpf
#
# See the file "license.txt", included in this
# distribution, for details about the copyright.

## Support code for generating NIF code.

import std / [assertions, syncio, formatfloat, math]
from std / strutils import endsWith
import vfs
export vfs.FileWriteMode

type
  Mode = enum
    UsesMem, UsesFile
  Builder* = object ## A builder can be in-memory or directly write into a file.
                    ## In the end either `extract` or `close` must be called.
                    ## File-mode builders accumulate into `buffer` and flush
                    ## once at `close()` via `vfsWrite`, so the destination
                    ## (real disk, in-memory cache, sandbox-rejected, …) is
                    ## decided by the active VFS relays at close time.
    buffer: string
    mode: Mode
    writeMode: FileWriteMode
    compact: bool
    filename: string
    nesting: int
    offs: int

proc `=copy`(dest: var Builder; src: Builder) {.error.}

proc open*(filename: string; compact = false; writeMode: FileWriteMode = AlwaysWrite): Builder =
  ## Opens a new builder attached to some output path. Writes are
  ## buffered in memory and flushed via `vfsWrite` at `close()`.
  ## With `writeMode = OnlyIfChanged` the close compares the buffered
  ## bytes to the existing file and skips the write (preserving mtime)
  ## when they match — useful for tools whose output should not bump
  ## downstream mtimes when nothing actually changed (e.g. nifler).
  Builder(buffer: "", mode: UsesFile, writeMode: writeMode,
          compact: compact, filename: filename)

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
    if b.writeMode == OnlyIfChanged and vfsExists(b.filename) and
        vfsRead(b.filename) == b.buffer:
      discard
    else:
      vfsWrite(b.filename, b.buffer)
  when not defined(showBroken):
    assert b.nesting == 0, "unpaired '(' or ')'"

proc putPending(b: var Builder; s: string) =
  b.buffer.add s
  b.offs += s.len

proc drainPending(b: var Builder) =
  ## No-op kept for source compatibility; both modes now buffer in
  ## memory until `close`.
  discard

proc put(b: var Builder; s: string) =
  b.buffer.add s
  b.offs += s.len

proc put(b: var Builder; s: char) =
  b.buffer.add s
  b.offs += 1

proc undoWhitespace(b: var Builder) =
  var i = b.buffer.len - 1
  while i >= 0 and b.buffer[i] in {' ', '\n'}:
    dec i
    b.offs -= 1
  b.buffer.setLen i+1


const
  ControlChars* = {'(', ')', '[', ']', '{', '}', '~', '#', '\'', '"', '\\', ':', '@'}

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
    L -= dottedSuffix.len
    inc L
  discard addSymbolImpl(b, s, L)

proc addSymbolDefRetIsGlobal*(b: var Builder; s: string; dottedSuffix = ""): bool =
  ## Returns true if the symbol is global.
  addSep b
  b.put ':'
  var L = s.len
  if dottedSuffix.len > 0 and s.endsWith(dottedSuffix):
    L -= dottedSuffix.len
    inc L
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
  b.put $i

proc addUIntLit*(b: var Builder; u: uint64) =
  addSep b
  b.put $u
  b.buffer.add 'u'
  b.offs += 1

proc attachLineInfo*(b: var Builder; col, line: int32; file = "")

proc addFloatLit*(b: var Builder; f: float; col: int32 = 0; line: int32 = 0; file = "") =
  ## Emit a float literal. Special values (`inf`, `nan`, `neginf`) are emitted
  ## as compounds; for those, `(col, line, file)` if non-zero is attached as a
  ## suffix on the inner tag (the only place a NIF27 line-info suffix can go).
  ## For finite values the caller is expected to call `attachLineInfo`
  ## separately; the args here are honored as a convenience.
  addSep b
  let hasInfo = col != 0 or line != 0 or file.len > 0
  case classify(f)
  of fcInf:
    b.put "(inf"
    if hasInfo: b.attachLineInfo(col, line, file)
    b.put ')'
  of fcNan:
    b.put "(nan"
    if hasInfo: b.attachLineInfo(col, line, file)
    b.put ')'
  of fcNegInf:
    b.put "(neginf"
    if hasInfo: b.attachLineInfo(col, line, file)
    b.put ')'
  of fcNegZero:
    b.put "-0.0"
    if hasInfo: b.attachLineInfo(col, line, file)
  of fcNormal, fcSubnormal, fcZero:
    let myLen = b.buffer.len
    b.buffer.addFloat f
    for i in myLen ..< b.buffer.len:
      if b.buffer[i] == 'e': b.buffer[i] = 'E'
    b.offs += b.buffer.len - myLen
    if hasInfo: b.attachLineInfo(col, line, file)


proc b62Char(d: int): char {.inline.} =
  if d < 10: char(ord('0') + d)
  elif d < 36: char(ord('A') + d - 10)
  else: char(ord('a') + d - 36)

proc addB62Unsigned(b: var Builder; n0: uint64) =
  ## Emit `n0` as base62 digits (most-significant first). Always emits at
  ## least one digit (even for zero) so the line-info parser sees a non-empty
  ## diff segment.
  if n0 == 0:
    b.put '0'
  else:
    var buf {.noinit.}: array[12, char]  # 62^12 > 2^64
    var i = 0
    var n = n0
    while n > 0'u64:
      buf[i] = b62Char(int(n mod 62'u64))
      n = n div 62'u64
      inc i
    while i > 0:
      dec i
      b.put buf[i]

proc addLineDiff(b: var Builder; x: int32; emitZero: bool) {.inline.} =
  ## Emit one base62 line-info diff. If `emitZero` is false and `x == 0`,
  ## emit nothing (the segment is implicitly zero between two commas).
  if x < 0:
    b.put '~'
    b.addB62Unsigned uint64(-int64(x))
  elif x > 0:
    b.addB62Unsigned uint64(x)
  elif emitZero:
    b.put '0'

proc attachLineInfo*(b: var Builder; col, line: int32; file = "") =
  ## Append a NIF27 line-information suffix to the most recently emitted atom
  ## or tag name. There must be no whitespace between the atom/tag and this
  ## call (do not call `addSep`, `addEmpty`, or any `add*Lit` between them).
  ## A no-op when all components are zero/empty.
  if col == 0 and line == 0 and file.len == 0:
    return
  drainPending b
  if col < 0:
    # Use the bare-`~` shorthand (no `@` introducer).
    b.put '~'
    b.addB62Unsigned uint64(-int64(col))
  else:
    b.put '@'
    if col > 0:
      b.addB62Unsigned uint64(col)
    # else: empty first diff segment, allowed by `B62Digit*` in the grammar.
  if line != 0 or file.len > 0:
    b.put ','
    b.addLineDiff line, emitZero = false
  if file.len > 0:
    b.put ','
    for c in file.items:
      if c.needsEscape:
        b.escape c
      else:
        b.put c

template addLineInfo*(b: var Builder; col, line: int32; file = "") =
  ## Backwards-compatible alias for `attachLineInfo`. Callers must invoke
  ## this **after** the atom or tag whose info they want to record — there
  ## is no buffering, no "pending" state.
  attachLineInfo(b, col, line, file)

proc attachComment*(b: var Builder; s: string) =
  ## Append a NIF27 comment suffix `#<s>#` to the most recently emitted atom
  ## or tag name (or directly after a preceding `attachLineInfo`). No
  ## whitespace allowed before the `#`.
  drainPending b
  b.put '#'
  for c in s.items:
    if c.needsEscape:
      b.escape c
    else:
      b.put c
  b.put '#'

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

proc addUIntLit*(b: var Builder; u: uint64; suffix: string;
                 col: int32 = 0; line: int32 = 0; file = "") =
  b.addTree "suf"
  if col != 0 or line != 0 or file.len > 0:
    b.attachLineInfo(col, line, file)
  b.addUIntLit(u)
  b.addStrLit(suffix)
  b.endTree()

proc addStrLit*(b: var Builder; s: string; suffix: string;
                col: int32 = 0; line: int32 = 0; file = "") =
  b.addTree "suf"
  if col != 0 or line != 0 or file.len > 0:
    b.attachLineInfo(col, line, file)
  b.addStrLit(s)
  b.addStrLit(suffix)
  b.endTree()

proc addHeader*(b: var Builder; vendor = "", dialect = "") =
  b.put "(.nif27)\n"
  if vendor.len > 0:
    b.put "(.vendor "
    b.addStrLit vendor
    b.put ")\n"
  if dialect.len > 0:
    b.put "(.dialect "
    b.addStrLit dialect
    b.put ")\n"

proc addHeader27*(b: var Builder): int =
  ## Returns the patch position for the indexat overwrite.
  b.put "(.nif27)\n"
  result = b.offs + len("(.indexat ")
  b.put "(.indexat                  )\n"
  #                 ^ whitespace essential here for patching without reallocations!

proc patchIndexAt*(b: var Builder; patchPos: int; indexAt: int) =
  var s = ""
  s.addInt indexAt
  for i in 0..<s.len:
    b.buffer[patchPos + i] = s[i]

proc offset*(b: Builder): int {.inline.} =
  ## Returns the current offset for index generation. The produced value might point to
  ## whitespace that must first be skipped before the desired element is reached but
  ## the nifreader will skip the whitespace automatically, so no harm is done.
  result = b.offs

when isMainModule and not defined(nimony):
  proc test(b: sink Builder) =
    b.addHeader "tester", "niftest"
    b.addTree "stmts"
    b.attachLineInfo 4, 5, "mymodb.nim"
    block:
      b.addTree "call"
      b.attachLineInfo 1, 3, "mymod.nim"
      b.addSymbolDef "oh.0.my.god"
      b.addSymbol "foo.3.mymod"
      b.addIntLit 3423
      b.addFloatLit 50.4
      # issue #1313
      b.addFloatLit -0.0
      b.endTree
    b.endTree

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
