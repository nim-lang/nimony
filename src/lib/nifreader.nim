#       Nif library
# (c) Copyright 2024 Andreas Rumpf
#
# See the file "license.txt", included in this
# distribution, for details about the copyright.

## High performance ("zero copies") NIF file reader.

import std / [memfiles, parseutils, assertions]
import stringviews
import vfs
import nifcore  # the ONE token-kind enum, shared with the binary token model
export NifKind

const StringLit* = StrLit
  ## The reader's traditional name for the string-literal token kind.
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
  FilePos* = object
    col*, line*: int32

  SymbolPart* = enum
    ## Which component of a split symbol an `ExtendedSuffix` token carries.
    ## A NIF symbol is `<name>.<disamb>[.<dedup>].<module>` (`<module>` is
    ## absent for a local symbol), so these are exactly the fields of the
    ## symbol object the compiler wants instead of a string.
    SymDisamb   ## the `12` of `abc.12.Ikey.mod`
    SymDedup    ## the `Ikey`: the key a generic instantiation is deduplicated by
    SymModule   ## the `mod`: the module suffix

  TokenFlag = enum
    TokenHasEscapes, FilenameHasEscapes, TokenHasModuleSuffixExpansion,
    CommentHasEscapes

  ExpandedToken* = object
    tk*: NifKind
    flags: set[TokenFlag]
    part*: SymbolPart ## for `ExtendedSuffix`: which component of the preceding symbol this is
    suffixes*: uint8  ## for `Symbol`/`SymbolDef`: how many `ExtendedSuffix` tokens follow
    kind*: uint16   # for clients to fill in ("known node kinds")
    data*: StringView
    pos*: FilePos
    filename*: StringView
    comment*: StringView ## raw bytes between '#' and '#'; empty if absent

  Reader* = object
    p: pchar
    eof: pointer # so that <= uses the correct comparison, not the cstring crap
    f: VfsBlob
    buf: string
    thisModule*: string
    line*: int32 # file position within the NIF file, not affected by line annotations
    indexAt: int  # position of the index
    unusedNameHint: ExpandedToken
    splitSyms: bool
    # The components of the symbol that was returned last, minus its name, and
    # minus the components already handed out. `pendingTotal` is what the head
    # token announced in `suffixes`; `pendingLeft` counts down to zero.
    pending: StringView
    pendingLeft, pendingTotal: int32
    pendingFlags: set[TokenFlag]
    pendingHasDisamb, pendingHasModule: bool

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
  of ExtendedSuffix:
    # a symbol component, in split mode; see `splitSymbols`
    result = $t.part & ":" & $t.data
  of TagLit, LineInfoLit:
    # binary-only kinds; the textual reader never produces them
    result = "<" & $t.tk & ">"

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

  proc readRawDataStable*(s: var string; start = 0): ptr UncheckedArray[char] {.inline.} =
    ## Nim strings are always heap-backed, so the data pointer is already stable
    ## across moves — no SSO promotion needed; this is just `readRawData`.
    if s.len == 0: nil
    else: cast[ptr UncheckedArray[char]](addr s[start])

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

proc captureComment(r: var Reader; result: var ExpandedToken) {.inline.} =
  ## Consume the bytes up to (and including) the closing `#` and record the
  ## inner span on `result.comment`. Tracks the `CommentHasEscapes` flag so
  ## downstream decoders can take the fast path when no escapes were present.
  result.comment.p = r.p
  useCpuRegisters:
    let start = p
    while p < eof:
      if ^p == '#':
        result.comment.len = cast[int](p) - cast[int](start)
        inc p
        break
      elif ^p == '\n':
        inc p
        inc r.line
      else:
        if ^p == '\\':
          result.flags.incl CommentHasEscapes
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

proc decodeEscape(p: var pchar): char {.inline.} =
  ## Decode a single NIF27 escape body. Caller has already advanced past the
  ## leading `\`. On return, `p` points just past the escape (advanced by 1
  ## for a shortcut, 2 for a hex pair). The shortcut bodies (`n`, `t`, `r`,
  ## `|`, `^`) are deliberately chosen so that none of them is a hex digit
  ## or a NIF control character, which lets a one-byte look-ahead pick the
  ## form unambiguously.
  case ^p
  of 'n': result = '\x0A'; inc p
  of 't': result = '\x09'; inc p
  of 'r': result = '\x0D'; inc p
  of '|': result = '\x5C'; inc p
  of '^': result = '\x22'; inc p
  else:
    result = handleHex(p)
    inc p, 2

proc decodeChar*(t: ExpandedToken): char =
  assert t.tk == CharLit
  result = ^t.data.p
  if result == '\\':
    var p = t.data.p
    inc p
    result = decodeEscape(p)

proc decodeStr*(r: Reader; t: ExpandedToken): string =
  if TokenHasEscapes in t.flags:
    result = ""
    var p = t.data.p
    let sentinel = p +! t.data.len
    while p < sentinel:
      if ^p == '\\':
        inc p
        result.add decodeEscape(p)
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
    # The module suffix is copied even when there is nothing before it: in the
    # reader's split-symbol mode the expanding component is the module and
    # nothing else, so its `data` is empty.
    copyMem(beginStore(result, result.len, t.data.len), r.thisModule.readRawData, r.thisModule.len)
    endStore(result)
  else:
    result = newString(t.data.len)
    if t.data.len > 0:
      copyMem(beginStore(result, result.len), t.data.p, t.data.len)
      endStore(result)

proc needsDecoding*(t: ExpandedToken): bool {.inline.} =
  ## Whether `decodeStr` would do anything but copy `t.data`: an escape to
  ## expand, or a module suffix to append. When it is false the bytes ARE the
  ## value, so a client that interns them (`getOrInclFromView`) can skip
  ## building a string at all.
  TokenHasEscapes in t.flags or TokenHasModuleSuffixExpansion in t.flags

proc decodeComment*(t: ExpandedToken): string =
  ## Decode the captured `#…#` comment, expanding `\HH` escapes. Returns "" if
  ## no comment is attached.
  if t.comment.len == 0:
    return ""
  if CommentHasEscapes notin t.flags:
    result = newString(t.comment.len)
    copyMem(beginStore(result, result.len), t.comment.p, t.comment.len)
    endStore(result)
    return
  result = ""
  var p = t.comment.p
  let sentinel = p +! t.comment.len
  while p < sentinel:
    if ^p == '\\':
      inc p
      result.add decodeEscape(p)
    else:
      result.add ^p
      inc p

proc decodeFilename*(t: ExpandedToken): string =
  if FilenameHasEscapes in t.flags:
    result = ""
    var p = t.filename.p
    let sentinel = p +! t.filename.len
    while p < sentinel:
      if ^p == '\\':
        inc p
        result.add decodeEscape(p)
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

proc decodeDisamb*(t: ExpandedToken): int =
  ## The value of a `SymDisamb` component: `abc.12.mod` gives 12. Only the
  ## leading digits are read (a well-formed symbol has nothing else there).
  assert t.tk == ExtendedSuffix and t.part == SymDisamb
  result = 0
  var i = 0
  while i < t.data.len and t.data[i] in Digits:
    result = result * 10 + (ord(t.data[i]) - ord('0'))
    inc i

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
    captureComment r, result   # consumes through the closing '#'

proc splitSymbols*(r: var Reader; enable = true) {.inline.} =
  ## Turn the split-symbol parsing mode on or off (default: off).
  ##
  ## A NIF symbol is not a string but an object: `<name>.<disamb>.<module>`,
  ## or `<name>.<disamb>.<dedup>.<module>` for a generic instantiation, or
  ## just `<name>.<disamb>` for a local symbol. Handing the whole thing to a
  ## client as one string is what makes the rest of the compiler take it
  ## apart again, over and over.
  ##
  ## In split mode the reader takes it apart ONCE, while it still has the
  ## raw bytes: a `Symbol`/`SymbolDef` token carries only `<name>`, and its
  ## `suffixes` field says how many `ExtendedSuffix` tokens follow — one per
  ## remaining component, in order, each tagged with its `part`. So
  ## `abc.12.Ikey.mod` arrives as
  ##
  ##   Symbol "abc" (suffixes: 3), SymDisamb "12", SymDedup "Ikey", SymModule "mod"
  ##
  ## `ExtendedSuffix` is the binary model's "more bits for the token before
  ## me" kind; a symbol component is exactly that, so no new token kind is
  ## needed and every existing `case` over `NifKind` still compiles.
  ##
  ## An atom with no dot to split on at all -- an operator definition such as
  ## `[]=` -- is left whole: `suffixes` is 0 and the token is what it always
  ## was. A symbol whose disambiguator is not a number (arkham's legacy
  ## `_exit.sys.mod`) still yields its module; only the name/disambiguator
  ## boundary needs the digit, so everything but the module is the name.
  r.splitSyms = enable

proc symbolsAreSplit*(r: Reader): bool {.inline.} = r.splitSyms

proc hasEscapes(data: StringView; first, last: int): bool =
  var i = first
  while i <= last:
    if data[i] == '\\': return true
    inc i
  result = false

proc splitSymbol(r: var Reader; result: var ExpandedToken) =
  ## Cut the just-lexed symbol into its `<name>` and the components that
  ## follow it, which subsequent `next` calls hand out as `ExtendedSuffix`
  ## tokens. `result.data` is shortened to the name.
  ##
  ## Same grammar as `symparser.sliceSymbol`, applied to the RAW bytes, where
  ## an escaped dot (`\2E`) is still three bytes and so cannot be mistaken for
  ## a separator: the module is the last dot-separated component unless it
  ## STARTS with a digit (`p.0h107` is a local symbol, not one from a module
  ## called `0h107`), and the name ends at the last `.` followed by a digit,
  ## because a name may contain dots itself (`a.b.c.23`).
  let origLen = result.data.len
  var lastDot = -1
  var k = 0
  while k < origLen:
    if result.data[k] == '.': lastDot = k
    inc k

  var head = origLen
  var moduleStart = -1
  if lastDot >= 0:
    if lastDot+1 < origLen:
      if result.data[lastDot+1] notin Digits:
        moduleStart = lastDot+1
        head = lastDot
    else:
      # a trailing dot is the "my own module" shorthand: the module is there,
      # spelled as nothing at all
      moduleStart = lastDot+1
      head = lastDot

  var i = head - 2
  while i > 0:
    if result.data[i] == '.' and result.data[i+1] in Digits: break
    dec i

  var start = 0
  if i > 0:
    r.pendingHasDisamb = true
    start = i+1
    result.data.len = i
  elif moduleStart >= 0:
    # no numeric disambiguator (`_exit.sys.mod`): everything but the module is
    # the name, which is what `splitSymName` has always answered here
    r.pendingHasDisamb = false
    start = moduleStart
    result.data.len = head
  else:
    return # not a symbol at all; leave it whole

  r.pendingHasModule = moduleStart >= 0
  # `result.data.len` is the name by now; the components run from `start` to the
  # symbol's ORIGINAL end.
  r.pending = StringView(p: result.data.p +! start, len: origLen - start)
  var comps = 1'i32
  for j in 0 ..< r.pending.len:
    if r.pending[j] == '.': inc comps
  r.pendingTotal = comps
  r.pendingLeft = comps
  r.pendingFlags = result.flags
  result.suffixes = uint8(comps)
  # The name keeps only the flags that are about the name: a trailing dot
  # (module suffix expansion) belongs to the last component, and escapes
  # only to the components that actually contain a backslash.
  result.flags.excl TokenHasModuleSuffixExpansion
  if TokenHasEscapes in result.flags and not hasEscapes(result.data, 0, result.data.len-1):
    result.flags.excl TokenHasEscapes

proc nextSymbolPart(r: var Reader; result: var ExpandedToken) =
  ## Hand out the next component of the symbol returned earlier.
  result = default(ExpandedToken)
  result.tk = ExtendedSuffix
  result.part =
    if r.pendingLeft == r.pendingTotal and r.pendingHasDisamb: SymDisamb
    elif r.pendingLeft == 1 and r.pendingHasModule: SymModule
    else: SymDedup
  var n = 0
  while n < r.pending.len and r.pending[n] != '.': inc n
  result.data = StringView(p: r.pending.p, len: n)
  if TokenHasEscapes in r.pendingFlags and hasEscapes(result.data, 0, n-1):
    result.flags.incl TokenHasEscapes
  dec r.pendingLeft
  if r.pendingLeft == 0:
    # `abc.12.` means "the module I am being read from"; the empty last
    # component is the one that expands to it.
    if TokenHasModuleSuffixExpansion in r.pendingFlags:
      result.flags.incl TokenHasModuleSuffixExpansion
    r.pending = StringView(p: nil, len: 0)
  else:
    r.pending = StringView(p: r.pending.p +! (n+1), len: r.pending.len - (n+1))

proc next*(r: var Reader; result: var ExpandedToken) =
  if r.pendingLeft > 0:
    nextSymbolPart(r, result)
    return
  result = default(ExpandedToken)
  # In the unified NifKind, ordinal 0 is DotToken, not UnknownToken — the
  # classification branches below rely on `tk` starting out as "unknown"
  # (e.g. the char-literal branch leaves it untouched on a lex error).
  result.tk = UnknownToken
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
      if r.splitSyms: splitSymbol(r, result)
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
        if r.splitSyms: splitSymbol(r, result)
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
          r.unusedNameHint = tok
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
  ## The Reader keeps `buf` alive as the owner of the source bytes. `buf` may be
  ## a short SSO string whose chars live *inline* in the string object; a plain
  ## `readRawData` pointer into it would dangle the moment the Reader is moved
  ## into its caller (and into the wrapping Stream), since the inline bytes move
  ## with the object. `readRawDataStable` pins `buf` to its heap representation,
  ## whose payload address survives those moves, so the cached `r.p`/`r.eof`
  ## stay valid for as long as the Reader (and thus `buf`) is alive.
  result = Reader(buf: ensureMove buf, thisModule: ensureMove thisModule)
  let n = result.buf.len
  result.p = readRawDataStable(result.buf)
  result.eof = result.p +! n
  result.f = initBlob(cast[pointer](result.p), n)
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
  # `offset` is the position AFTER the whole symbol, so a jump abandons the
  # components of a split symbol that were not asked for.
  r.pendingLeft = 0
  r.pending = StringView(p: nil, len: 0)

proc indexStartsAt*(r: Reader): int =
  r.indexAt

proc firstUnusedName*(r: Reader): string =
  ## Returns the symbol supplied by the `.unusedname` directive, or `""`.
  result = ""
  if r.unusedNameHint.tk == Symbol:
    result = decodeStr(r, r.unusedNameHint)

when isMainModule and not defined(nimony):
  proc tokens(input: string; split: bool): seq[string] =
    ## Every token of `input`, rendered as "<what>:<decoded text>".
    var r = openFromBuffer(input, "ThisMod")
    r.splitSymbols split
    result = @[]
    var tok = default(ExpandedToken)
    while true:
      r.next(tok)
      case tok.tk
      of EofToken: break
      of Symbol: result.add "sym" & $tok.suffixes & ":" & r.decodeStr(tok)
      of SymbolDef: result.add "def" & $tok.suffixes & ":" & r.decodeStr(tok)
      of ExtendedSuffix:
        var s = $tok.part & ":" & r.decodeStr(tok)
        if tok.part == SymDisamb: s.add "=" & $decodeDisamb(tok)
        result.add s
      of ParLe: result.add "(" & $tok.data
      of ParRi: result.add ")"
      else: result.add $tok.tk & ":" & r.decodeStr(tok)
    close r

  # Off by default: a symbol is one token carrying the whole string, exactly
  # as every existing client expects.
  assert tokens("(stmts abc.12.Ikey.mod :def.1.mod tmp.14)", false) ==
    @["(stmts", "sym0:abc.12.Ikey.mod", "def0:def.1.mod", "sym0:tmp.14", ")"]

  # On: the reader takes the symbol apart and reports the components as
  # `ExtendedSuffix` tokens following the head.
  assert tokens("abc.12.Ikey.mod", true) ==
    @["sym3:abc", "SymDisamb:12=12", "SymDedup:Ikey", "SymModule:mod"]
  assert tokens("abc.12.mod", true) ==
    @["sym2:abc", "SymDisamb:12=12", "SymModule:mod"]
  # A local symbol has no module.
  assert tokens("tmp.14", true) == @["sym1:tmp", "SymDisamb:14=14"]
  # A symbol definition splits the same way.
  assert tokens(":def.1.mod", true) ==
    @["def2:def", "SymDisamb:1=1", "SymModule:mod"]
  # A trailing dot is the "my own module" shorthand: it is the last component
  # that expands, not the name.
  assert tokens("x.3.", true) ==
    @["sym2:x", "SymDisamb:3=3", "SymModule:ThisMod"]
  # Only the disambiguator's dot ends the name; the name may contain dots.
  assert tokens("a.b.c.23.mod", true) ==
    @["sym2:a.b.c", "SymDisamb:23=23", "SymModule:mod"]
  # An escaped dot is `\2E` in the raw bytes and so cannot be mistaken for a
  # separator; the escapes stay with the component that has them.
  assert tokens("a\\2Eb.5.mod", true) ==
    @["sym2:a.b", "SymDisamb:5=5", "SymModule:mod"]
  # An operator definition has no dot to split on and is left whole.
  assert tokens("foo.bar", true) == @["sym1:foo", "SymModule:bar"]
  assert tokens(":\\5B\\5D=", true) == @["def0:[]="]
  # A disambiguator the inliner minted is NOT a module, however non-numeric it
  # looks after its first character.
  assert tokens("p.0h107", true) == @["sym1:p", "SymDisamb:0h107=0"]
  # ...and one with no numeric part at all (arkham's legacy syproc names) still
  # yields its module; everything before it is the name.
  assert tokens("_exit.sys.sysvq0asl", true) ==
    @["sym1:_exit.sys", "SymModule:sysvq0asl"]
  # Line info and comments ride on the head, as before.
  assert tokens("abc.12.mod@3,4#hi#", true) ==
    @["sym2:abc", "SymDisamb:12=12", "SymModule:mod"]

  block: # a jump abandons the components nobody asked for
    var r = openFromBuffer("abc.12.mod tail.1", "ThisMod")
    r.splitSymbols()
    assert r.symbolsAreSplit
    var tok = default(ExpandedToken)
    let start = offset(r)
    r.next(tok)
    assert tok.tk == Symbol and tok.suffixes == 2'u8
    r.jumpTo start
    r.next(tok)
    assert r.decodeStr(tok) == "abc"
    r.next(tok)
    assert tok.tk == ExtendedSuffix and tok.part == SymDisamb
    close r

  echo "nifreader: OK"
