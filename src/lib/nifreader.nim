#       Nif library
# (c) Copyright 2024 Andreas Rumpf
#
# See the file "license.txt", included in this
# distribution, for details about the copyright.

## High performance ("zero copies") NIF file reader.

import std / [memfiles, tables, parseutils, assertions, hashes]
import stringviews
when defined(nimony):
  import std/syncio

const
  ControlChars = {'(', ')', '[', ']', '{', '}', '~', '#', '\'', '"', ':'}
  ControlCharsOrWhite = ControlChars + {' ', '\n', '\t', '\r'}
  HexChars = {'0'..'9', 'A'..'F'} # lowercase letters are not in the NIF spec!
  Digits = {'0'..'9'}

type
  NifKind* = enum
    UnknownToken, EofToken,
    DotToken, Ident, Symbol, SymbolDef,
    StringLit, CharLit, IntLit, UIntLit, FloatLit,
    ParLe, ParRi

  FilePos* = object
    col*, line*: int32

  TokenFlag = enum
    TokenHasEscapes, FilenameHasEscapes

  Token* = object
    tk*: NifKind
    flags: set[TokenFlag]
    kind*: uint16   # for clients to fill in ("known node kinds")
    s*: StringView
    pos*: FilePos
    filename*: StringView

  MetaInfo* = object
    dialect*: StringView
    vendor*: StringView
    platform*: StringView
    config*: StringView

  Reader* = object
    p: pchar
    eof: pointer # so that <= uses the correct comparison, not the cstring crap
    f: MemFile
    buf: string
    line*: int32 # file position within the NIF file, not affected by line annotations
    trackDefs*: bool
    isubs, ksubs: Table[StringView, (NifKind, StringView)]
    defs: Table[string, pchar]
    meta: MetaInfo

proc `$`*(t: Token): string =
  case t.tk
  of UnknownToken: result = "<unknown token>"
  of EofToken: result = "<eof>"
  of ParLe: result = "(" & $t.s
  of ParRi: result = ")"
  of DotToken: result = "."
  of Ident, Symbol, SymbolDef,
     StringLit, CharLit, IntLit, UIntLit, FloatLit:
    result = $t.tk & ":" & $t.s

template inc(p: pchar; diff = 1) =
  p = cast[pchar](cast[int](p) + diff)

template dec(p: pchar; diff = 1) =
  p = cast[pchar](cast[int](p) - diff)

template `+!`(p: pchar; diff: int): pchar =
  cast[pchar](cast[int](p) + diff)

template `-!`(a, b: pchar): int = cast[int](a) - cast[int](b)

template `^`(p: pchar): char = p[0]

when not defined(nimony):
  proc rawData*(s: var string): ptr UncheckedArray[char] {.inline.} =
    assert s.len > 0
    cast[ptr UncheckedArray[char]](addr s[0])

proc open*(filename: string): Reader =
  let f = try:
      memfiles.open(filename)
    except:
      when defined(debug) and not defined(nimony): writeStackTrace()
      quit "[Error] cannot open: " & filename
  result = Reader(f: f, p: nil)
  result.p = cast[pchar](result.f.mem)
  result.eof = result.p +! result.f.size

proc openFromBuffer*(buf: sink string): Reader =
  result = Reader(f: default(MemFile), buf: ensureMove buf)
  result.p = rawData result.buf
  result.eof = result.p +! result.buf.len
  result.f.mem = result.p
  result.f.size = result.buf.len

proc close*(r: var Reader) =
  try:
    memfiles.close(r.f)
  except:
    when defined(debug) and not defined(nimony): writeStackTrace()
    quit "[Error] cannot close"

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

proc decodeChar*(t: Token): char =
  assert t.tk == CharLit
  result = ^t.s.p
  if result == '\\':
    var p = t.s.p
    inc p
    result = handleHex(p)

proc decodeStr*(t: Token): string =
  if TokenHasEscapes in t.flags:
    result = ""
    var p = t.s.p
    let sentinel = p +! t.s.len
    while p < sentinel:
      if ^p == '\\':
        inc p
        result.add handleHex(p)
        inc p, 2
      else:
        result.add ^p
        inc p
  else:
    result = newString(t.s.len)
    if t.s.len > 0:
      copyMem(rawData result, t.s.p, t.s.len)

proc decodeFilename*(t: Token): string =
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
    copyMem(rawData result, t.filename.p, t.filename.len)

proc decodeFloat*(t: Token): BiggestFloat =
  result = 0.0
  assert t.tk == FloatLit
  let res = parseutils.parseBiggestFloat(toOpenArray(t.s.p, 0, t.s.len-1), result)
  assert res == t.s.len

proc decodeUInt*(t: Token): BiggestUInt =
  result = 0
  assert t.tk == UIntLit
  let res = parseutils.parseBiggestUInt(toOpenArray(t.s.p, 0, t.s.len-1), result)
  assert res == t.s.len

proc decodeInt*(t: Token): BiggestInt =
  result = 0
  assert t.tk == IntLit
  let res = parseutils.parseBiggestInt(toOpenArray(t.s.p, 0, t.s.len-1), result)
  assert res == t.s.len

proc handleNumber(r: var Reader; result: var Token) =
  useCpuRegisters:
    if p < eof and ^p in Digits:
      result.tk = IntLit # overwritten if we detect a float or unsigned
      while p < eof and ^p in Digits:
        inc p
        inc result.s.len

      if p < eof and ^p == '.':
        result.tk = FloatLit
        inc p
        inc result.s.len
        while p < eof and ^p in Digits:
          inc p
          inc result.s.len

      if p < eof and ^p == 'E':
        result.tk = FloatLit
        inc p
        inc result.s.len
        if p < eof:
          if ^p == '-' or ^p == '+':
            inc p
            inc result.s.len
        while p < eof and ^p in Digits:
          inc p
          inc result.s.len

      if p < eof and ^p == 'u':
        result.tk = UIntLit
        inc p
        # ignore the suffix 'u'

proc handleLineInfo(r: var Reader; result: var Token) =
  proc integerOutOfRangeError() {.noinline, noreturn.} =
    quit "Parsed integer outside of valid range"

  useCpuRegisters:
    var col = 0
    var negative = false
    if p < eof and ^p == '~':
      inc p
      negative = true
    while p < eof and ^p in Digits:
      let c = ord(^p) - ord('0')
      if col >= (low(int) + c) div 10:
        col = col * 10 - c
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
      while p < eof and ^p in Digits:
        let c = ord(^p) - ord('0')
        if line >= (low(int) + c) div 10:
          line = line * 10 - c
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

proc next*(r: var Reader): Token =
  # Returning a new Token is somewhat unusual but lets clients
  # create implicit trees on the stack.
  result = default(Token)
  skipWhitespace r
  if r.p >= r.eof:
    result.tk = EofToken
  else:
    if ^r.p in {'0'..'9', ',', '~'}:
      # we have node prefix
      handleLineInfo r, result
      skipWhitespace r

    if ^r.p == '#':
      # we have a node comment, just skip it:
      skipComment r
      skipWhitespace r

    case ^r.p
    of '(':
      result.tk = ParLe
      useCpuRegisters:
        inc p
        result.s.p = p
        result.s.len = 0
        while p < eof and ^p notin ControlCharsOrWhite:
          inc result.s.len
          inc p
      if r.ksubs.len > 0:
        let repl = r.ksubs.getOrDefault(result.s)
        if repl[0] != UnknownToken:
          result.s = repl[1]

    of ')':
      result.tk = ParRi
      result.s.p = r.p
      inc result.s.len
      inc r.p
    of '.':
      result.tk = DotToken
      result.s.p = r.p
      inc result.s.len
      inc r.p
    of '"':
      useCpuRegisters:
        inc p
        result.tk = StringLit
        result.s.p = p
        result.s.len = 0
        while p < eof:
          let ch = ^p
          if ch == '"':
            inc p
            break
          elif ch == '\\':
            result.flags.incl TokenHasEscapes
          elif ch == '\n':
            inc r.line
          inc result.s.len
          inc p
    of '\'':
      inc r.p
      result.s.p = r.p
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

    of ':':
      useCpuRegisters:
        var start = p
        inc p
        result.s.p = p
        while p < eof and ^p notin ControlCharsOrWhite:
          if ^p == '\\': result.flags.incl TokenHasEscapes
          inc result.s.len
          inc p
      if result.s.len > 0:
        result.tk = SymbolDef
        if r.isubs.len > 0:
          let repl = r.isubs.getOrDefault(result.s)
          if repl[0] == Symbol:
            result.s = repl[1]
          else:
            result.tk = UnknownToken # error
        if r.trackDefs:
          while start != r.f.mem:
            if ^start == '(':
              r.defs[decodeStr result] = start
              break
            dec start

    of '-', '+':
      result.s.p = r.p
      inc r.p
      inc result.s.len
      handleNumber r, result

    else:
      useCpuRegisters:
        result.s.p = p
        var hasDot = false
        while p < eof and ^p notin ControlCharsOrWhite:
          if ^p == '\\': result.flags.incl TokenHasEscapes
          elif ^p == '.': hasDot = true
          inc result.s.len
          inc p

      if result.s.len > 0:
        result.tk = if hasDot: Symbol else: Ident
        if r.isubs.len > 0:
          let repl = r.isubs.getOrDefault(result.s)
          if repl[0] != UnknownToken:
            result.tk = repl[0]
            result.s = repl[1]
  #if result.tk == UnknownToken:
  #  for i in 0 .. 100:
  #    stdout.write r.p[i]
  #  writeStackTrace()
  #  quit "huh? "

type
  RestorePoint* = object
    p: pchar
    line: int32

proc success*(r: RestorePoint): bool {.inline.} = r.p != nil

proc restore*(r: var Reader; rp: RestorePoint) {.inline.} =
  r.p = rp.p
  r.line = rp.line

proc savePos*(r: Reader): RestorePoint {.inline.} =
  result = RestorePoint(p: r.p, line: r.line)

proc jumpTo*(r: var Reader; def: string): RestorePoint =
  assert def.len > 0
  assert r.trackDefs
  #assert def[0] != ':' # not correct, could be an escaped ':'
  var p = r.defs.getOrDefault(def)
  result = RestorePoint(p: r.p, line: r.line)
  if p != nil:
    r.p = p
    r.line = -1'i32 # unknown
  else:
    while true:
      let t = next(r)
      if t.tk == SymbolDef:
        p = r.defs.getOrDefault(def)
        if p != nil:
          r.p = p
          r.line = -1'i32 # unknown
          return result
      elif t.tk == EofToken:
        break
    # not found, reset position:
    r.p = result.p
    r.line = result.line
    result.p = nil # not found

when false:
  proc setPosition*(r: var Reader; s: StringView) {.inline.} =
    assert r.p >= cast[pchar](r.f.mem) and r.p < r.eof
    r.p = s.p +! s.len
    r.line = -1'i32 # unknown

  proc span*(r: Reader; offset: int; s: StringView): int {.inline.} =
    assert s.p >= cast[pchar](r.f.mem) and s.p < r.eof
    result = (s.p -! cast[pchar](r.f.mem)) - offset

const
  Cookie = "(.nif24)"

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

proc processDirectives*(r: var Reader): DirectivesResult =
  template handleSubstitutionPair(r: var Reader; valid: set[NifKind]; subs: Table[StringView, (NifKind, StringView)]) =
    if r.p < r.eof and ^r.p in ControlCharsOrWhite:
      let key = next(r)
      if key.tk == Ident:
        let val = next(r)
        let closingPar = next(r)
        if closingPar.tk == ParRi and val.tk in valid:
          subs[key.s] = (val.tk, val.s)

  template handleMeta(r: var Reader; field: untyped) =
    let value = next(r)
    if value.tk == StringLit:
      field = value.s
    else:
      result = WrongMeta
    while true:
      var closePar = next(r)
      if closePar.tk in {ParRi, EofToken}: break

  if r.startsWith(Cookie):
    result = Success
    inc r.p, Cookie.len
    while true:
      skipWhitespace r
      if r.startsWith("(.k"):
        inc r.p, len("(.k")
        # extension: let node kinds have dots! `(atomic.inc ...)`
        handleSubstitutionPair r, {Ident, Symbol}, r.ksubs
      elif r.startsWith("(.i"):
        inc r.p, len("(.i")
        handleSubstitutionPair r, {Ident, Symbol, StringLit, CharLit, IntLit, UIntLit, FloatLit}, r.isubs
      elif r.startsWith("(."):
        let directive = next(r)
        assert directive.tk == ParLe
        if directive.s == ".vendor":
          handleMeta r, r.meta.vendor
        elif directive.s == ".platform":
          handleMeta r, r.meta.platform
        elif directive.s == ".dialect":
          handleMeta r, r.meta.dialect
        elif directive.s == ".config":
          handleMeta r, r.meta.config
        else:
          # skip unknown directive
          while true:
            var closePar = next(r)
            if closePar.tk in {ParRi, EofToken}: break
      else:
        break
  else:
    result = WrongHeader

proc fileSize*(r: var Reader): int {.inline.} =
  r.f.size

proc offset*(r: var Reader): int {.inline.} =
  result = r.p -! cast[pchar](r.f.mem)

proc jumpTo*(r: var Reader; offset: int) {.inline.} =
  r.p = cast[pchar](r.f.mem) +! offset
  assert r.p >= cast[pchar](r.f.mem) and r.p < r.eof

when isMainModule:
  const test = r"(.nif24)(stmts :\5B\5D=)"
  var r = openFromBuffer(test)
  while true:
    let tk = r.next()
    if tk.tk == EofToken: break
    echo decodeStr tk, " ", tk
