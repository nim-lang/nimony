#       Nif library
# (c) Copyright 2024 Andreas Rumpf
#
# See the file "license.txt", included in this
# distribution, for details about the copyright.

## High performance ("zero copies") NIF file reader.

import std / [memfiles, parseutils, assertions]
import stringviews
import vfs
when defined(nimony):
  import std/syncio

  {.feature: "lenientnils".}

const
  ControlChars = {'(', ')', '[', ']', '{', '}', '~', '#', '\'', '"', ':', '@'}
  ControlCharsOrWhite = ControlChars + {' ', '\n', '\t', '\r'}
  HexChars = {'0'..'9', 'A'..'F'} # lowercase letters are not in the NIF spec!
  Digits = {'0'..'9'}
  B62Digits = {'0'..'9', 'A'..'Z', 'a'..'z'}

type
  NifKind* = enum
    UnknownToken, EofToken,
    DotToken, Ident, Symbol, SymbolDef,
    StringLit, CharLit, IntLit, UIntLit, FloatLit,
    ParLe, ParRi

  FilePos* = object
    col*, line*: int32

  TokenFlag = enum
    TokenHasEscapes, FilenameHasEscapes, TokenHasModuleSuffixExpansion

  ExpandedToken* = object
    tk*: NifKind
    flags: set[TokenFlag]
    kind*: uint16   # for clients to fill in ("known node kinds")
    data*: StringView
    pos*: FilePos
    filename*: StringView

  Reader* = object
    p: pchar
    eof: pointer # so that <= uses the correct comparison, not the cstring crap
    f: VfsBlob
    buf: string
    thisModule*: string
    line*: int32 # file position within the NIF file, not affected by line annotations
    indexAt: int  # position of the index
    unusedNameHint: StringView

proc `$`*(t: ExpandedToken): string =
  case t.tk
  of UnknownToken: result = "<unknown token>"
  of EofToken: result = "<eof>"
  of ParLe: result = "(" & $t.data
  of ParRi: result = ")"
  of DotToken: result = "."
  of Ident, Symbol, SymbolDef,
     StringLit, CharLit, IntLit, UIntLit, FloatLit:
    result = $t.tk & ":" & $t.data

template inc(p: pchar; diff = 1) =
  p = cast[pchar](cast[int](p) + diff)

template `+!`(p: pchar; diff: int): pchar =
  cast[pchar](cast[int](p) + diff)

template `-!`(a, b: pchar): int = cast[int](a) - cast[int](b)

template `^`(p: pchar): char = p[0]

when not defined(nimony):
  proc readRawData*(s: string): ptr UncheckedArray[char] {.inline.} =
    assert s.len > 0
    cast[ptr UncheckedArray[char]](addr s[0])

proc close*(r: var Reader) =
  ## Release the backend's read handle (mmap unmap, LMDB read txn
  ## close, …) via the explicit VfsBlob close API.
  closeBlob(r.f)

when not defined(nimony):
  {.pragma: untyped.}

template useCpuRegisters(body: untyped) {.untyped.} =
  var p {.inject.} = r.p # encourage the code generator to use a register for this.
  let eof {.inject.} = r.eof
  body
  r.p = p # store back

proc skipWhitespace(r: var Reader) =
  useCpuRegisters:
    while p < eof:
      case ^p
      of ' ', '\t', '\r':
        inc p
      of '\n':
        inc p
        inc r.line
      else:
        break

proc skipComment(r: var Reader) {.inline.} =
  useCpuRegisters:
    while p < eof:
      if ^p == '#':
        inc p
        break
      elif ^p == '\n':
        inc p
        inc r.line
      else:
        inc p

proc handleHex(p: pchar): char =
  var output = 0
  case p[0]
  of '0'..'9':
    output = output shl 4 or (ord(p[0]) - ord('0'))
  of 'A'..'F':
    output = output shl 4 or (ord(p[0]) - ord('A') + 10)
  else: discard
  case p[1]
  of '0'..'9':
    output = output shl 4 or (ord(p[1]) - ord('0'))
  of 'A'..'F':
    output = output shl 4 or (ord(p[1]) - ord('A') + 10)
  else: discard
  result = char(output)

proc decodeChar*(t: ExpandedToken): char =
  assert t.tk == CharLit
  result = ^t.data.p
  if result == '\\':
    var p = t.data.p
    inc p
    result = handleHex(p)

proc decodeStr*(r: Reader; t: ExpandedToken): string =
  if TokenHasEscapes in t.flags:
    result = ""
    var p = t.data.p
    let sentinel = p +! t.data.len
    while p < sentinel:
      if ^p == '\\':
        inc p
        result.add handleHex(p)
        inc p, 2
      else:
        result.add ^p
        inc p
    # Handle module suffix expansion after decoding escapes
    if TokenHasModuleSuffixExpansion in t.flags:
      assert r.thisModule.len > 0
      result.add r.thisModule
  elif TokenHasModuleSuffixExpansion in t.flags:
    assert r.thisModule.len > 0
    result = newString(t.data.len + r.thisModule.len)
    if t.data.len > 0:
      copyMem(beginStore(result, result.len), t.data.p, t.data.len)
      copyMem(beginStore(result, result.len, t.data.len), r.thisModule.readRawData, r.thisModule.len)
      endStore(result)
  else:
    result = newString(t.data.len)
    if t.data.len > 0:
      copyMem(beginStore(result, result.len), t.data.p, t.data.len)
      endStore(result)

proc decodeFilename*(t: ExpandedToken): string =
  if FilenameHasEscapes in t.flags:
    result = ""
    var p = t.filename.p
    let sentinel = p +! t.filename.len
    while p < sentinel:
      if ^p == '\\':
        inc p
        result.add handleHex(p)
        inc p, 2
      else:
        result.add ^p
        inc p
  else:
    result = newString(t.filename.len)
    copyMem(beginStore(result, result.len), t.filename.p, t.filename.len)
    endStore(result)

proc decodeFloat*(t: ExpandedToken): BiggestFloat =
  result = 0.0
  assert t.tk == FloatLit
  let res = parseutils.parseBiggestFloat(toOpenArray(t.data.p, 0, t.data.len-1), result)
  assert res == t.data.len

proc decodeUInt*(t: ExpandedToken): BiggestUInt =
  result = 0
  assert t.tk == UIntLit
  let res = parseutils.parseBiggestUInt(toOpenArray(t.data.p, 0, t.data.len-1), result)
  assert res == t.data.len

proc decodeInt*(t: ExpandedToken): BiggestInt =
  result = 0
  assert t.tk == IntLit
  let res = parseutils.parseBiggestInt(toOpenArray(t.data.p, 0, t.data.len-1), result)
  assert res == t.data.len

proc handleNumber(r: var Reader; result: var ExpandedToken) =
  useCpuRegisters:
    if p < eof and ^p in Digits:
      result.tk = IntLit # overwritten if we detect a float or unsigned
      while p < eof and ^p in Digits:
        inc p
        inc result.data.len

      if p < eof and ^p == '.':
        result.tk = FloatLit
        inc p
        inc result.data.len
        while p < eof and ^p in Digits:
          inc p
          inc result.data.len

      if p < eof and ^p == 'E':
        result.tk = FloatLit
        inc p
        inc result.data.len
        if p < eof:
          if ^p == '-' or ^p == '+':
            inc p
            inc result.data.len
        while p < eof and ^p in Digits:
          inc p
          inc result.data.len

      if p < eof and ^p == 'u':
        result.tk = UIntLit
        inc p
        # ignore the suffix 'u'

proc decodeB62(c: char): int {.inline.} =
  ## Decode a single base62 digit (0-9, A-Z, a-z) to its numeric value 0..61.
  ## Caller must guarantee `c` is in `B62Digits`.
  if c <= '9': result = ord(c) - ord('0')
  elif c <= 'Z': result = ord(c) - ord('A') + 10
  else: result = ord(c) - ord('a') + 36

proc handleLineInfo(r: var Reader; result: var ExpandedToken) =
  ## Parse the body of a line-info suffix. The leading `@`, if any, has already
  ## been consumed by the caller; a leading `~` (negative-first shorthand) has
  ## **not** been consumed.
  proc integerOutOfRangeError() {.noinline, noreturn.} =
    quit "Parsed integer outside of valid range"

  useCpuRegisters:
    var col = 0
    var negative = false
    if p < eof and ^p == '~':
      inc p
      negative = true
    while p < eof and ^p in B62Digits:
      let c = decodeB62(^p)
      if col >= (low(int) + c) div 62:
        col = col * 62 - c
      else:
        integerOutOfRangeError()
      inc p
    if not negative:
      if col == low(int):
        integerOutOfRangeError()
      col = -col

    var line = 0
    negative = false

    if p < eof and ^p == ',':
      inc p
      if p < eof and ^p == '~':
        inc p
        negative = true
      while p < eof and ^p in B62Digits:
        let c = decodeB62(^p)
        if line >= (low(int) + c) div 62:
          line = line * 62 - c
        else:
          integerOutOfRangeError()
        inc p
      if not negative:
        if line == low(int):
          integerOutOfRangeError()
        line = -line

    result.pos = FilePos(col: col.int32, line: line.int32)

    if p < eof and ^p == ',':
      inc p
      result.filename.p = p
      while p < eof:
        let ch = ^p
        if ch in ControlCharsOrWhite:
          break
        elif ch == '\\':
          result.flags.incl FilenameHasEscapes
        elif ch == '\n':
          inc r.line
        inc result.filename.len
        inc p

proc handleSuffix(r: var Reader; result: var ExpandedToken) {.inline.} =
  ## After consuming an atom or a tag name, parse the optional postfix
  ## `@<info>` (or `~<neg-info>` shorthand) followed by an optional `#comment#`.
  ## No whitespace is allowed between the atom/tag and the suffix introducer,
  ## nor between the line info and the comment.
  if r.p < r.eof:
    let ch = ^r.p
    if ch == '@':
      inc r.p
      handleLineInfo(r, result)
    elif ch == '~':
      handleLineInfo(r, result)
  if r.p < r.eof and ^r.p == '#':
    inc r.p           # consume the opening '#'
    skipComment r     # consumes through the closing '#'

proc next*(r: var Reader; result: var ExpandedToken) =
  result = default(ExpandedToken)
  skipWhitespace r
  if r.p >= r.eof:
    result.tk = EofToken
    return

  case ^r.p
  of '(':
    result.tk = ParLe
    useCpuRegisters:
      inc p
      result.data.p = p
      result.data.len = 0
      while p < eof and ^p notin ControlCharsOrWhite:
        inc result.data.len
        inc p
    handleSuffix(r, result)

  of ')':
    result.tk = ParRi
    result.data.p = r.p
    inc result.data.len
    inc r.p
  of '.':
    result.tk = DotToken
    result.data.p = r.p
    inc result.data.len
    inc r.p
    handleSuffix(r, result)
  of '"':
    useCpuRegisters:
      inc p
      result.tk = StringLit
      result.data.p = p
      result.data.len = 0
      while p < eof:
        let ch = ^p
        if ch == '"':
          inc p
          break
        elif ch == '\\':
          result.flags.incl TokenHasEscapes
        elif ch == '\n':
          inc r.line
        inc result.data.len
        inc p
    handleSuffix(r, result)
  of '\'':
    inc r.p
    result.data.p = r.p
    if ^r.p == '\\':
      result.flags.incl TokenHasEscapes
      inc r.p
      if r.p[0] in HexChars and r.p[1] in HexChars:
        inc r.p, 2
        if ^r.p == '\'':
          inc r.p
          result.tk = CharLit # now valid
    elif ^r.p in ControlChars:
      discard "keep it as UnknownToken"
    else:
      inc r.p
      if ^r.p == '\'':
        inc r.p
        result.tk = CharLit # only now valid
    if result.tk == CharLit:
      handleSuffix(r, result)

  of ':':
    useCpuRegisters:
      inc p
      result.data.p = p
      while p < eof and ^p notin ControlCharsOrWhite:
        if ^p == '\\': result.flags.incl TokenHasEscapes
        inc result.data.len
        inc p
    if result.data.len > 0:
      result.tk = SymbolDef
      if result.data[result.data.len-1] == '.':
        result.flags.incl TokenHasModuleSuffixExpansion
      handleSuffix(r, result)

  of '-':
    # negative number; '+' is no longer a number prefix in NIF27.
    result.data.p = r.p
    inc r.p
    inc result.data.len
    handleNumber r, result
    handleSuffix(r, result)

  of '0'..'9':
    # bare-digit number (NIF27): no sign prefix on positives.
    useCpuRegisters:
      result.data.p = p
      result.data.len = 0
    handleNumber r, result
    handleSuffix(r, result)

  else:
    useCpuRegisters:
      result.data.p = p
      var hasDot = false
      while p < eof and ^p notin ControlCharsOrWhite:
        if ^p == '\\': result.flags.incl TokenHasEscapes
        elif ^p == '.': hasDot = true
        inc result.data.len
        inc p

    if result.data.len > 0:
      if hasDot:
        result.tk = Symbol
        if result.data[result.data.len-1] == '.':
          result.flags.incl TokenHasModuleSuffixExpansion
      else:
        result.tk = Ident
      handleSuffix(r, result)
    else:
      # Stray control character we don't otherwise handle (e.g. `[`, `]`, `~`).
      # Consume it to avoid an infinite loop and surface it as UnknownToken so
      # callers can flag the malformed input.
      result.data.p = r.p
      result.data.len = 1
      inc r.p

proc next*(r: var Reader): ExpandedToken {.deprecated: "use the other next instead".} =
  result = default(ExpandedToken)
  next r, result

type
  DirectivesResult* = enum
    WrongHeader, WrongMeta, Success

proc startsWith*(r: Reader; prefix: string): bool =
  let prefixLen = prefix.len
  var i = 0
  var p = r.p
  while true:
    if i >= prefixLen: return true
    if p >= r.eof or ^p != prefix[i]: return false
    inc p
    inc i
  return false

proc readDirectives(r: var Reader) =
  var tok = default(ExpandedToken)
  while true:
    skipWhitespace r
    if r.startsWith("(."):
      next(r, tok)
      assert tok.tk == ParLe
      if tok.data == ".indexat":
        next(r, tok)
        if tok.tk == IntLit:
          r.indexAt = int decodeInt tok
      elif tok.data == ".unusedname":
        next(r, tok)
        if tok.tk == Symbol:
          r.unusedNameHint = tok.data
      # skip the rest of the directive:
      var nested = 0
      while true:
        next(r, tok)
        case tok.tk
        of ParLe: inc nested
        of ParRi:
          if nested == 0: break
          dec nested
        of EofToken: break
        else: discard
    else:
      break

proc extractModuleSuffix*(filename: string): string =
  result = ""
  var skip = false
  for c in filename:
    if c == '/' or c == '\\':
      result.setLen 0
      skip = false
    elif c == '.':
      skip = true
    elif not skip:
      result.add c

proc open*(filename: string): Reader =
  let f = try:
      vfsOpenMmap(filename)
    except:
      when defined(debug) and not defined(nimony): writeStackTrace()
      quit "[Error] cannot open: " & filename
  result = Reader(f: f, p: nil, thisModule: extractModuleSuffix(filename))
  result.p = cast[pchar](result.f.data)
  result.eof = result.p +! result.f.size
  readDirectives result

proc openFromBuffer*(buf: sink string; thisModule: sink string): Reader =
  result = Reader(buf: ensureMove buf, thisModule: ensureMove thisModule)
  result.p = readRawData result.buf
  result.eof = result.p +! result.buf.len
  result.f = initBlob(cast[pointer](result.p), result.buf.len)
  readDirectives result

proc processDirectives*(r: var Reader): DirectivesResult =
  result = Success

proc fileSize*(r: var Reader): int {.inline.} =
  r.f.size

proc offset*(r: var Reader): int {.inline.} =
  result = r.p -! cast[pchar](r.f.data)

proc jumpTo*(r: var Reader; offset: int) {.inline.} =
  r.p = cast[pchar](r.f.data) +! offset
  assert cast[pointer](r.p) >= r.f.data and r.p < r.eof

proc indexStartsAt*(r: Reader): int =
  r.indexAt

when isMainModule and not defined(nimony):
  #const test = r"(.nif27)(stmts :\5B\5D=)"
  const test = "nimcache/sysvq0asl.s.nif"
  var r = open(test)
  var tok = default(ExpandedToken)
  while true:
    r.next(tok)
    if tok.tk == EofToken: break
    echo r.decodeStr tok, " ", tok
