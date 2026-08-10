#       Nif library
# (c) Copyright 2024 Andreas Rumpf
#
# See the file "license.txt", included in this
# distribution, for details about the copyright.

## Support code for generating NIF code.

when defined(nimony):
  # The write cursor (`Builder.raw`) is a raw payload pointer that is nil between
  # `finish` and the next `reserve`; the nil analysis cannot follow that through
  # `beginStore`. Same reason `bif` sets it.
  {.feature: "lenientnils".}

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
      ## Storage, NOT the result: `buffer.len` is the *capacity*, `offs` is how
      ## much of it is written. `finish` truncates it back to `offs`.
    raw: ptr UncheckedArray[char]
      ## Write cursor into `buffer`'s payload. Kept in the object because the
      ## alternative — `buffer.add c` per byte — cost 54 instructions per output
      ## byte natively (a call, a COW check, a capacity check and an inline-cache
      ## sync), which was the single biggest item in the `emit` benchmark. The
      ## pointer is stable across moves of the Builder because `reserve` keeps
      ## the string in its *long* (heap) representation; see `MinCapacity`.
    cap: int
    mode: Mode
    writeMode: FileWriteMode
    compact: bool
    filename: string
    nesting: int
    offs: int

const
  MinCapacity = 64
    ## Also the reason `raw` is safe: a string this long is heap-allocated, so
    ## its payload address does not move when the Builder object does.

proc `=copy`(dest: var Builder; src: Builder) {.error.}

# `beginStore`/`endStore` are the Nimony bulk-write API: resize without zeroing,
# then sync the SSO inline prefix cache. Host Nim has neither, so shim them
# privately (a `*`-exported shim would collide with `bif`'s copy in modules that
# import both).
when defined(nimony):
  proc grabBuf(s: var string; newLen: int): ptr UncheckedArray[char] {.inline.} =
    beginStore(s, newLen, 0)
  proc syncBuf(s: var string) {.inline.} = endStore(s)
else:
  proc grabBuf(s: var string; newLen: int): ptr UncheckedArray[char] {.inline.} =
    s.setLen(newLen)
    cast[ptr UncheckedArray[char]](addr s[0])
  proc syncBuf(s: var string) {.inline.} = discard
  proc readRawData(s: string): ptr UncheckedArray[char] {.inline.} =
    if s.len == 0: nil
    else: cast[ptr UncheckedArray[char]](addr s[0])

proc reserve(b: var Builder; extra: int) =
  ## Make room for `extra` more bytes. Grows geometrically and re-acquires the
  ## cursor, which the resize may have invalidated.
  if b.offs + extra > b.cap:
    var newCap = max(b.cap * 2, MinCapacity)
    while newCap < b.offs + extra: newCap = newCap * 2
    b.raw = grabBuf(b.buffer, newCap)
    b.cap = newCap

proc finish(b: var Builder) =
  ## Cut the storage back to what was actually written and re-sync the SSO
  ## prefix cache, so `buffer` is a valid `string` again. Must run before
  ## anything reads `buffer` as a string (`extract`, `close`).
  b.buffer.setLen b.offs
  syncBuf b.buffer
  b.cap = b.offs
  b.raw = nil

proc open*(filename: string; compact = false; writeMode: FileWriteMode = AlwaysWrite): Builder =
  ## Opens a new builder attached to some output path. Writes are
  ## buffered in memory and flushed via `vfsWrite` at `close()`.
  ## With `writeMode = OnlyIfChanged` the close compares the buffered
  ## bytes to the existing file and skips the write (preserving mtime)
  ## when they match — useful for tools whose output should not bump
  ## downstream mtimes when nothing actually changed (e.g. nifler).
  result = Builder(buffer: "", mode: UsesFile, writeMode: writeMode,
                   compact: compact, filename: filename)
  reserve(result, MinCapacity)

proc open*(sizeHint: int; compact = false): Builder =
  ## Opens a new builder with the intent to keep the produced
  ## code in memory.
  result = Builder(buffer: "", mode: UsesMem, compact: compact)
  reserve(result, max(sizeHint, MinCapacity))

proc attachedToFile*(b: Builder): bool {.inline.} = b.mode == UsesFile

proc extract*(b: sink Builder): string =
  ## Extracts the buffer from the builder.
  ## The builder should not be used afterwards.
  when not defined(showBroken):
    assert b.nesting == 0, "unpaired '(' or ')'" & $b.nesting
  assert b.mode == UsesMem, "cannot extract from a file"
  finish b
  result = move(b.buffer)

proc close*(b: var Builder) =
  if b.mode == UsesFile:
    finish b
    if b.writeMode == OnlyIfChanged and vfsExists(b.filename) and
        vfsRead(b.filename) == b.buffer:
      discard
    else:
      vfsWrite(b.filename, b.buffer)
  when not defined(showBroken):
    assert b.nesting == 0, "unpaired '(' or ')'"

proc put(b: var Builder; s: string) =
  let n = s.len
  if n > 0:
    reserve b, n
    copyMem(cast[pointer](addr b.raw[b.offs]), cast[pointer](readRawData(s)), n)
    b.offs += n

template put(b: var Builder; s: char) =
  ## A TEMPLATE, not an `{.inline.}` proc: at ~5.7M calls per emitted module
  ## this is the most executed routine in the builder, and a call costs 28
  ## instructions around a body of three. Nothing here evaluates `b` in a way
  ## that repeating it could matter — every call site passes the builder
  ## variable itself.
  if b.offs >= b.cap: reserve b, 1
  b.raw[b.offs] = s
  b.offs += 1

proc putPending(b: var Builder; s: string) {.inline.} = put(b, s)

proc drainPending(b: var Builder) =
  ## No-op kept for source compatibility; both modes now buffer in
  ## memory until `close`.
  discard

proc undoWhitespace(b: var Builder) =
  var i = b.offs - 1
  while i >= 0 and b.raw[i] in {' ', '\n'}:
    dec i
  b.offs = i+1


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
  ## Insert a token separator if the previous byte isn't already one. NIF27
  ## bare numbers (`123` instead of the legacy `+123`) carry no leading
  ## sign-byte to act as an implicit separator, so adjacent atoms at top
  ## level (e.g. `"foo"123u`) would otherwise glue together — emit a space
  ## even when nesting is zero.
  if b.offs == 0:
    discard
  elif b.raw[b.offs-1] in {'\n', ' ', '(', ')'}:
    discard "no separator required"
  else:
    b.put ' '

proc addNumber*(b: var Builder; s: string) =
  addSep b
  put b, s

#  ------------ Atoms ------------------------

proc addIdent*(b: var Builder; s: string) =
  addSep b
  let sLen = s.len
  if sLen > 0:
    let src = readRawData(s)
    let c0 = src[0]
    if c0 < ' ' or c0 in {'.', '0'..'9', '+', '-', '~'} or c0.needsEscape:
      b.escape c0
    else:
      b.put c0
    for i in 1..<sLen:
      let c = src[i]
      if c < ' ' or (c in ControlChars+{'.'}):
        b.escape c
      else:
        b.put c

proc addSymbolImpl(b: var Builder; s: string; len: int): int {.inline.} =
  ## Returns the number of dots in the symbol.
  result = 0
  if s.len > 0:
    let src = readRawData(s)
    let c0 = src[0]
    if c0 in {'.', '0'..'9', '+', '-', '~'} or c0.needsEscape:
      b.escape c0
    else:
      b.put c0
    for i in 1..<len:
      let c = src[i]
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
  for c in s:
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
  b.put 'u'

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
    # Format into a scratch string: the builder's storage is capacity-sized, so
    # `addFloat` cannot append to it directly.
    var tmp = ""
    tmp.addFloat f
    let myLen = b.offs
    b.put tmp
    for i in myLen ..< b.offs:
      if b.raw[i] == 'e': b.raw[i] = 'E'
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
    for c in file:
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
  for c in s:
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
    b.raw[patchPos + i] = s[i]

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
