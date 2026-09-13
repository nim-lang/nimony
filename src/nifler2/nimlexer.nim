#
#
#           Nifler2: Nim to NIF
#        (c) Copyright 2026 Andreas Rumpf
#
#    See the file "license.txt", included in this
#    distribution, for details about the copyright.
#

## Nim's lexer for `nifler2`, built on `std/regex`'s `lex` construct.
##
## The token type, the token kinds and their spellings are the ones
## `compiler/lexer.nim` uses, because the two are meant to be differentially
## tested against each other: same input, same token sequence, or a bug.
## `src/nifler2/tools/gramcheck.nim` already assumes these names -- the
## grammar's `'if'` is `tkIf` and its `'{.'` is `tkCurlyDotLe`.
##
## What the generated automata do and what is hand-written:
##
## * **`lex`**: keywords and identifiers, every numeric literal form --
##   decimal, hex, octal, binary, and floats with an exponent -- and the type
##   suffix behind a number. Numbers are where a DFA earns its keep:
##   `getNumber` in Nim's lexer is a hundred lines of hand-rolled state, most
##   of it spelling out where `_` may appear.
## * **hand-written**: the indentation and spacing bookkeeping, comments
##   (including nested `#[ ]#`), string and character literals, and the
##   punctuation whose meaning depends on the character *after* it (`(.` is
##   one token, `(..` is two).
##
## Keywords are case-sensitive, like every other identifier in Nimony: `proc`
## is the keyword, `pRoC` and `p_roc` are identifiers. This is where the lexer
## deliberately parts ways with Nim's, whose keywords are style-insensitive.

import std / regex

type
  TokKind* = enum ## Nim's `TokType`, minus the tokens only `renderer.nim` uses
    tkInvalid = "tkInvalid", tkEof = "[EOF]",
    tkSymbol = "tkSymbol", # keywords:
    tkAddr = "addr", tkAnd = "and", tkAs = "as", tkAsm = "asm",
    tkBind = "bind", tkBlock = "block", tkBreak = "break", tkCase = "case",
    tkCast = "cast", tkConcept = "concept", tkConst = "const",
    tkContinue = "continue", tkConverter = "converter", tkDefer = "defer",
    tkDiscard = "discard", tkDistinct = "distinct", tkDiv = "div", tkDo = "do",
    tkElif = "elif", tkElse = "else", tkEnd = "end", tkEnum = "enum",
    tkExcept = "except", tkExport = "export", tkFinally = "finally",
    tkFor = "for", tkFrom = "from", tkFunc = "func", tkIf = "if",
    tkImport = "import", tkIn = "in", tkInclude = "include",
    tkInterface = "interface", tkIs = "is", tkIsnot = "isnot",
    tkIterator = "iterator", tkLet = "let", tkMacro = "macro",
    tkMethod = "method", tkMixin = "mixin", tkMod = "mod", tkNil = "nil",
    tkNot = "not", tkNotin = "notin", tkObject = "object", tkOf = "of",
    tkOr = "or", tkOut = "out", tkProc = "proc", tkPtr = "ptr",
    tkRaise = "raise", tkRef = "ref", tkReturn = "return", tkShl = "shl",
    tkShr = "shr", tkStatic = "static", tkTemplate = "template", tkTry = "try",
    tkTuple = "tuple", tkType = "type", tkUsing = "using", tkVar = "var",
    tkWhen = "when", tkWhile = "while", tkXor = "xor",
    tkYield = "yield", # end of keywords
    tkIntLit = "tkIntLit", tkInt8Lit = "tkInt8Lit", tkInt16Lit = "tkInt16Lit",
    tkInt32Lit = "tkInt32Lit", tkInt64Lit = "tkInt64Lit",
    tkUIntLit = "tkUIntLit", tkUInt8Lit = "tkUInt8Lit",
    tkUInt16Lit = "tkUInt16Lit", tkUInt32Lit = "tkUInt32Lit",
    tkUInt64Lit = "tkUInt64Lit",
    tkFloatLit = "tkFloatLit", tkFloat32Lit = "tkFloat32Lit",
    tkFloat64Lit = "tkFloat64Lit", tkFloat128Lit = "tkFloat128Lit",
    tkStrLit = "tkStrLit", tkRStrLit = "tkRStrLit",
    tkTripleStrLit = "tkTripleStrLit", tkGStrLit = "tkGStrLit",
    tkGTripleStrLit = "tkGTripleStrLit", tkCharLit = "tkCharLit",
    tkCustomLit = "tkCustomLit",
    tkParLe = "(", tkParRi = ")", tkBracketLe = "[", tkBracketRi = "]",
    tkCurlyLe = "{", tkCurlyRi = "}",
    tkBracketDotLe = "[.", tkBracketDotRi = ".]",
    tkCurlyDotLe = "{.", tkCurlyDotRi = ".}",
    tkParDotLe = "(.", tkParDotRi = ".)",
    tkComma = ",", tkSemiColon = ";",
    tkColon = ":", tkColonColon = "::", tkEquals = "=",
    tkDot = ".", tkDotDot = "..", tkBracketLeColon = "[:",
    tkOpr = "tkOpr", tkComment = "tkComment", tkAccent = "`"

  TokSpacing* = enum ## whitespace *around* a token, which the parser needs to
                     ## tell `f(x)` from `f (x)` and `-x` from `a - x`
    tsLeading, tsTrailing, tsEof

  Token* = object
    kind*: TokKind
    s*: string            ## identifier, operator, or the *decoded* literal
    indent*: int32        ## column when first on its line, else -1
    spacing*: set[TokSpacing]
    line*, col*: int32
    base*: int32          ## 2, 8, 10 or 16 for the integer literals
    suffixPos*: int32     ## `tkCustomLit`: index of the `'` inside `s`
    iNumber*: int64       ## value of an integer literal
      ## Floats keep their text in `s` and are converted by whoever needs the
      ## value; integers cannot, because `tkIntLit` is promoted to
      ## `tkInt64Lit` exactly when the value leaves `int32`'s range.

  Lexer* = object
    buf*: string
    pos*: int
    filename*: string
    lineNumber*: int32
    lineStart*: int       ## buffer offset of the current line's first char
    currLineIndent*: int32
    indentAhead*: int32   ## set by a doc comment for the token behind it
    errors*: seq[string]

const
  KeywordLow* = tkAddr
  KeywordHigh* = tkYield

  OpChars* = {'+', '-', '*', '/', '\\', '<', '>', '!', '?', '^', '.',
              '|', '=', '%', '&', '$', '@', '~', ':'}
  SymChars* = {'a'..'z', 'A'..'Z', '0'..'9', '\x80'..'\xFF'}
  SymStartChars* = {'a'..'z', 'A'..'Z', '\x80'..'\xFF'}
  UnicodeOperatorStartChars* = {'\xC2', '\xC3', '\xE2'}
    ## `∙ ∘ × ★ ☆ ⊗ ⊘ ⊙ ⊛ ⊠ ⊡ ∩ ∧ ⊓ ⟑ ⟇ ⩓ ⩔ ■ □ ± ⊕ ⊖ ⊞ ⊟ ∪ ∨ ⊔` all start
    ## with one of these three bytes.
  UnaryMinusWhitelist = {' ', '\t', '\n', '\r', ',', ';', '(', '[', '{'}

  MulPred = 9
  PlusPred = 8

# ---------------------------------------------------------------------------
# Buffer access
#
# Nim's lexer reads from a sentinel-terminated buffer, so it may peek one or
# two characters past the token without a bounds check. `ch` gives us the same
# freedom over a plain string: everything past the end reads as '\0', which is
# also what the real lexer's end-of-file sentinel is.
# ---------------------------------------------------------------------------

proc str(c: char): string {.inline.} =
  result = ""
  result.add c

proc charAt(buf: string; i: int): char {.inline.} =
  if i >= 0 and i < buf.len: buf[i] else: '\0'

proc ch*(L: Lexer; i: int): char {.inline.} = charAt(L.buf, i)

proc errorAt(L: var Lexer; line, col: int; msg: string) =
  L.errors.add L.filename & "(" & $line & ", " & $(col + 1) & ") Error: " & msg

proc error(L: var Lexer; pos: int; msg: string) =
  errorAt L, L.lineNumber, pos - L.lineStart, msg

proc litNumText(L: Lexer; start: int): string =
  ## The number as `lexMessageLitNum` quotes it: everything literal-ish from
  ## `start`, which is behind a leading `-`.
  const LiteralishChars = {'A'..'Z', 'a'..'z', '0'..'9', '_', '.', '\''}
  result = ""
  var pos = start
  while L.ch(pos) in LiteralishChars:
    result.add L.ch(pos)
    inc pos
  if L.ch(pos) in {'+', '-'} and L.ch(pos-1) in {'e', 'E'}:
    result.add L.ch(pos)
    inc pos
    while L.ch(pos) in LiteralishChars:
      result.add L.ch(pos)
      inc pos
  if L.ch(pos) in LiteralishChars:
    result.add L.ch(pos)
    inc pos
    while L.ch(pos) in {'0'..'9'}:
      result.add L.ch(pos)
      inc pos

proc handleCRLF(L: var Lexer; pos: int): int =
  result = pos
  if L.ch(result) == '\r':
    inc result
    if L.ch(result) == '\n': inc result
  elif L.ch(result) == '\n':
    inc result
  inc L.lineNumber
  L.lineStart = result

# ---------------------------------------------------------------------------
# Identifiers and keywords
# ---------------------------------------------------------------------------

proc nimIdentNormalize*(s: string): string =
  ## Nim's identifier equality made explicit: the first character counts as
  ## written, the rest is lowercased and underscores drop out. The lexer does
  ## not use it; the differential tools compare identifiers with it, because
  ## Nim's identifier cache cannot report the spelling it saw.
  result = ""
  if s.len > 0: result.add s[0]
  var i = 1
  while i < s.len:
    let c = s[i]
    if c == '_': discard
    elif c >= 'A' and c <= 'Z': result.add chr(ord(c) - ord('A') + ord('a'))
    else: result.add c
    inc i

# ---------------------------------------------------------------------------
# Unicode operators
# ---------------------------------------------------------------------------

type
  UnicodeOprPred = enum uopNone, uopMul, uopAdd

proc unicodeOprLen*(buf: string; pos: int): (int, UnicodeOprPred) =
  ## Length and precedence class of the unicode operator at `pos`, or
  ## `(0, uopNone)`. A byte in `UnicodeOperatorStartChars` that does not begin
  ## one of these is an ordinary identifier character, which is the whole
  ## reason this has to be consulted while scanning a symbol.
  result = (0, uopNone)
  case charAt(buf, pos)
  of '\xE2':
    let b1 = charAt(buf, pos+1)
    let b2 = charAt(buf, pos+2)
    if b1 == '\x88':
      if b2 == '\x98' or b2 == '\x99' or b2 == '\xA7' or b2 == '\xA9':
        result = (3, uopMul)          # ∘ ∙ ∧ ∩
      elif b2 == '\xA8' or b2 == '\xAA':
        result = (3, uopAdd)          # ∨ ∪
    elif b1 == '\x8A':
      if b2 == '\x93' or b2 == '\x97' or b2 == '\x98' or b2 == '\x99' or
         b2 == '\x9B' or b2 == '\xA0' or b2 == '\xA1':
        result = (3, uopMul)          # ⊓ ⊗ ⊘ ⊙ ⊛ ⊠ ⊡
      elif b2 == '\x94' or b2 == '\x95' or b2 == '\x96' or b2 == '\x9E' or
           b2 == '\x9F':
        result = (3, uopAdd)          # ⊔ ⊕ ⊖ ⊞ ⊟
    elif b1 == '\x96':
      if b2 == '\xA0' or b2 == '\xA1': result = (3, uopMul)   # ■ □
    elif b1 == '\x98':
      if b2 == '\x85' or b2 == '\x86': result = (3, uopMul)   # ★ ☆
    elif b1 == '\x9F':
      if b2 == '\x87' or b2 == '\x91': result = (3, uopMul)   # ⟇ ⟑
    elif b1 == '\xA9':
      if b2 == '\x93' or b2 == '\x94': result = (3, uopMul)   # ⩓ ⩔
  of '\xC2':
    if charAt(buf, pos+1) == '\xB1': result = (2, uopAdd)     # ±
  of '\xC3':
    if charAt(buf, pos+1) == '\x97': result = (2, uopMul)     # ×
  else: discard

# ---------------------------------------------------------------------------
# Operator classification -- lexer-level in Nim, and the parser runtime needs
# all of it.
# ---------------------------------------------------------------------------

proc getPrecedence*(tok: Token): int =
  ## Nim computes an operator's precedence from its spelling, which is why the
  ## grammar's `binary(...)` takes this as a parameter rather than baking a
  ## precedence table into the generated parser.
  case tok.kind
  of tkOpr:
    let s = tok.s
    if s.len == 0: return -10
    let last = s[s.len-1]
    if s.len > 1 and last == '>' and
       (s[s.len-2] == '-' or s[s.len-2] == '~' or s[s.len-2] == '='):
      return 0                              # arrow-like
    let asgn = last == '='
    case s[0]
    of '$', '^': result = (if asgn: 1 else: 10)
    of '*', '%', '/', '\\': result = (if asgn: 1 else: MulPred)
    of '~': result = 8
    of '+', '-', '|': result = (if asgn: 1 else: PlusPred)
    of '&': result = (if asgn: 1 else: 7)
    of '=', '<', '>', '!': result = 5
    of '.': result = (if asgn: 1 else: 6)
    of '?': result = 2
    of '\xC2', '\xC3', '\xE2':
      if asgn:
        result = 1
      else:
        let (n, pred) = unicodeOprLen(s, 0)
        if n != 0: result = (if pred == uopMul: MulPred else: PlusPred)
        else: result = 2
    else: result = (if asgn: 1 else: 2)
  of tkDiv, tkMod, tkShl, tkShr: result = 9
  of tkDotDot: result = 6
  of tkIn, tkNotin, tkIs, tkIsnot, tkOf, tkAs, tkFrom: result = 5
  of tkAnd: result = 4
  of tkOr, tkXor, tkPtr, tkRef: result = 3
  else: result = -10

proc isRightAssoc*(tok: Token): bool {.inline.} =
  ## Only `^`-like operators associate to the right, exactly as in
  ## `parser.nim`'s `isRightAssociative`.
  tok.kind == tkOpr and tok.s.len > 0 and tok.s[0] == '^' 

proc isUnary*(tok: Token): bool {.inline.} =
  ## Space in front and none behind: `-x` is a prefix operator, `a - x` is not.
  tok.kind in {tkOpr, tkDotDot} and tok.spacing == {tsLeading}

proc isDotLike*(tok: Token): bool {.inline.} =
  tok.kind == tkOpr and tok.s.len > 1 and tok.s[0] == '.' and tok.s[1] != '.'

proc isSigilLike*(tok: Token): bool {.inline.} =
  tok.kind == tkOpr and tok.s.len > 0 and tok.s[0] == '@'

# ---------------------------------------------------------------------------
# The generated automata
# ---------------------------------------------------------------------------

proc numberSuffix(L: var Lexer; tok: var Token; start: int; isFloat: bool) =
  ## Second stage, as in `getNumber`: the first automaton matched the number,
  ## this one reads the `'i8` / `u32` / `'myLit` behind it. They are two
  ## automata and not one because the number must be the *longest* number:
  ## `0x1f32` is a hex literal, not `0x1` with the suffix `f32`, and a single
  ## automaton's maximal munch cannot tell those apart.
  tok.kind = if isFloat: tkFloatLit else: tkIntLit
  let numEnd = L.pos
  tok.s = L.buf.substr(start, numEnd-1)
  var pos = numEnd
  var kind = tok.kind
  # The built-in suffixes come first so that they win against the custom
  # literal and the bad suffix on a tie; a longer spelling (`'i8x`) does not
  # tie and is not built in.
  lex L.buf, pos:
  of r"'? [fF] (32)?": kind = tkFloat32Lit
  of r"'? ([dD] | [fF] 64)": kind = tkFloat64Lit
  of r"'? [fF] 128": kind = tkFloat128Lit
  of r"'? [iI] 8": kind = tkInt8Lit
  of r"'? [iI] 16": kind = tkInt16Lit
  of r"'? [iI] 32": kind = tkInt32Lit
  of r"'? [iI] 64": kind = tkInt64Lit
  of r"'? [uU]": kind = tkUIntLit
  of r"'? [uU] 8": kind = tkUInt8Lit
  of r"'? [uU] 16": kind = tkUInt16Lit
  of r"'? [uU] 32": kind = tkUInt32Lit
  of r"'? [uU] 64": kind = tkUInt64Lit
  of r"' [a-zA-Z0-9\0128-\0255] [a-zA-Z0-9_\0128-\0255]*":
    kind = tkCustomLit
  of r"[fFdDiIuU] [a-zA-Z0-9_\0128-\0255]*":
    error L, numEnd, "invalid number suffix: '" & L.buf.substr(numEnd, pos-1) & "'"
  else:
    if L.ch(pos) == '\'':
      error L, pos + 1, "invalid number suffix"
  L.pos = pos
  if kind == tkCustomLit:
    tok.suffixPos = int32(numEnd - start)
    tok.s = L.buf.substr(start, pos-1)
  tok.kind = kind

proc normalizeBasePrefix(tok: var Token) =
  ## `getNumber` writes the base prefix into the literal in lower case
  ## whatever the source said, so `0X10` and `0x10` are one token and not two
  ## spellings of it.
  if tok.base == 10: return
  let i = if tok.s.len > 0 and tok.s[0] == '-': 2 else: 1
  if i < tok.s.len and tok.s[i] >= 'A' and tok.s[i] <= 'Z':
    tok.s[i] = chr(ord(tok.s[i]) - ord('A') + ord('a'))

proc hexVal(c: char): int {.inline.} =
  if c >= '0' and c <= '9': ord(c) - ord('0')
  elif c >= 'a' and c <= 'f': ord(c) - ord('a') + 10
  elif c >= 'A' and c <= 'F': ord(c) - ord('A') + 10
  else: 0

proc signExtend(x: uint64; bits: int): int64 =
  ## `x`'s low `bits` bits, read as a two's complement number. `0x80'i8` is
  ## -128 and not an error, which is the rule for every non-decimal literal.
  let m = 1'u64 shl (bits - 1)
  let masked = x and ((m shl 1) - 1'u64)
  result = cast[int64](masked xor m) - cast[int64](m)

proc numberValue(L: var Lexer; tok: var Token; start: int) =
  ## The value matters here and not only in the parser: `0xffffffff` is a
  ## `tkInt64Lit` and `0xffff` is a `tkIntLit`, so the token *kind* depends on
  ## it. Floats are left as text.
  if tok.kind == tkCustomLit or
     (tok.kind >= tkFloatLit and tok.kind <= tkFloat128Lit):
    return
  var i = 0
  var negative = false
  if i < tok.s.len and tok.s[i] == '-':
    negative = true
    inc i
  var xi = 0'u64
  if tok.base != 10:
    let shiftBy = if tok.base == 16: 4 elif tok.base == 8: 3 else: 1
    inc i, 2                            # past `0x` / `0o` / `0c` / `0b`
    while i < tok.s.len:
      if tok.s[i] != '_': xi = (xi shl shiftBy) or uint64(hexVal(tok.s[i]))
      inc i
    case tok.kind
    of tkInt8Lit: tok.iNumber = signExtend(xi, 8)
    of tkInt16Lit: tok.iNumber = signExtend(xi, 16)
    of tkInt32Lit: tok.iNumber = signExtend(xi, 32)
    of tkUInt8Lit: tok.iNumber = cast[int64](xi and 0xff'u64)
    of tkUInt16Lit: tok.iNumber = cast[int64](xi and 0xffff'u64)
    of tkUInt32Lit: tok.iNumber = cast[int64](xi and 0xffffffff'u64)
    else: tok.iNumber = cast[int64](xi)
  else:
    var overflow = false
    while i < tok.s.len:
      let c = tok.s[i]
      if c != '_':
        let d = uint64(ord(c) - ord('0'))
        if xi > (0xffffffffffffffff'u64 - d) div 10'u64: overflow = true
        xi = xi * 10'u64 + d
      inc i
    # A decimal literal is parsed, not reinterpreted, so it really can be out
    # of range -- and when it is, Nim abandons the whole third stage, which
    # means the token keeps the kind it already had. `9223372036854775808`
    # stays a `tkIntLit`; it does not become a `tkInt64Lit`.
    let limit =
      if tok.kind == tkUIntLit or tok.kind == tkUInt64Lit:
        0xffffffffffffffff'u64
      elif negative: 0x8000000000000000'u64
      else: 0x7fffffffffffffff'u64
    if overflow or xi > limit:
      # Nim's third stage has rewound to the start of the digits
      error L, start, "number out of range: '" & litNumText(L, start) & "'"
      return
    tok.iNumber = cast[int64](xi)
    if negative: tok.iNumber = -tok.iNumber
    let tooWide =
      case tok.kind
      of tkInt8Lit: tok.iNumber > 127'i64 or tok.iNumber < -128'i64
      of tkInt16Lit: tok.iNumber > 32767'i64 or tok.iNumber < -32768'i64
      of tkInt32Lit: tok.iNumber > 2147483647'i64 or
                     tok.iNumber < -2147483648'i64
      of tkUInt8Lit: tok.iNumber > 255'i64 or tok.iNumber < 0'i64
      of tkUInt16Lit: tok.iNumber > 65535'i64 or tok.iNumber < 0'i64
      of tkUInt32Lit: tok.iNumber > 4294967295'i64 or tok.iNumber < 0'i64
      else: false
    if tooWide: error L, start, "number out of range: '" & litNumText(L, start) & "'"
  if tok.base != 10 and negative: tok.iNumber = -tok.iNumber
  # "Promote int literal to int64? Not always necessary, but more consistent"
  if tok.kind == tkIntLit and
     (tok.iNumber > 2147483647'i64 or tok.iNumber < -2147483648'i64):
    tok.kind = tkInt64Lit

proc scanNumber(L: var Lexer; tok: var Token) =
  ## One automaton for every numeric literal Nim has. The patterns spell out
  ## `_`'s rule -- between digits, never doubled, never trailing -- rather
  ## than checking for it afterwards, which is the whole hundred lines of
  ## `matchUnderscoreChars` and its callers gone.
  let start = L.pos
  var pos = L.pos
  var base = 10'i32
  var isFloat = false
  block:
    let d = if L.ch(start) == '-': start + 1 else: start
    if L.ch(d) == '0' and L.ch(d+1) == 'O':
      error L, d + 1, litNumText(L, d) &
        " is an invalid int literal; For octal literals use the '0o' prefix."
  lex L.buf, pos:
  # The digits are optional so that `0x` with nothing behind it is one bad
  # number rather than `0` followed by the identifier `x`, which is what Nim
  # reports and therefore what the differential test expects.
  of r"-? 0 [xX] ([0-9a-fA-F](_?[0-9a-fA-F])*)?": base = 16'i32
  of r"-? 0 [ocC] ([0-7](_?[0-7])*)?": base = 8'i32  # `0c` deprecated, `0O` invalid
  of r"-? 0 [bB] ([01](_?[01])*)?": base = 2'i32
  of r"-? [0-9](_?[0-9])* (\. [0-9](_?[0-9])*)? [eE][+-]? [0-9](_?[0-9])*",
     r"-? [0-9](_?[0-9])* \. [0-9](_?[0-9])*":
    isFloat = true
  of r"-? [0-9](_?[0-9])*": discard
  else:
    tok.kind = tkInvalid
    tok.s = str(L.ch(pos))
    error L, pos, "invalid number"
    inc pos
    L.pos = pos
    return
  L.pos = pos
  tok.base = base
  let digits = if L.buf[start] == '-': start + 1 else: start
  if base != 10 and pos - digits <= 2:
    error L, pos, "invalid number: '" & litNumText(L, digits) & "'"
  numberSuffix L, tok, start, isFloat
  normalizeBasePrefix tok
  numberValue L, tok, digits
  if L.ch(L.pos) in SymChars + {'_'} and unicodeOprLen(L.buf, L.pos)[0] == 0:
    error L, L.pos, "invalid token: no whitespace between number and identifier"

proc symbolKind(s: string; pos: var int): TokKind =
  ## The keyword or identifier starting at `pos`, which is moved behind it;
  ## `tkInvalid`, with `pos` unmoved, when there is none. A keyword ties with
  ## the identifier pattern and wins because it comes first; `iffy` is longer
  ## than `if` and so is an identifier.
  result = tkInvalid
  lex s, pos:
  of "addr": result = tkAddr
  of "and": result = tkAnd
  of "as": result = tkAs
  of "asm": result = tkAsm
  of "bind": result = tkBind
  of "block": result = tkBlock
  of "break": result = tkBreak
  of "case": result = tkCase
  of "cast": result = tkCast
  of "concept": result = tkConcept
  of "const": result = tkConst
  of "continue": result = tkContinue
  of "converter": result = tkConverter
  of "defer": result = tkDefer
  of "discard": result = tkDiscard
  of "distinct": result = tkDistinct
  of "div": result = tkDiv
  of "do": result = tkDo
  of "elif": result = tkElif
  of "else": result = tkElse
  of "end": result = tkEnd
  of "enum": result = tkEnum
  of "except": result = tkExcept
  of "export": result = tkExport
  of "finally": result = tkFinally
  of "for": result = tkFor
  of "from": result = tkFrom
  of "func": result = tkFunc
  of "if": result = tkIf
  of "import": result = tkImport
  of "in": result = tkIn
  of "include": result = tkInclude
  of "interface": result = tkInterface
  of "is": result = tkIs
  of "isnot": result = tkIsnot
  of "iterator": result = tkIterator
  of "let": result = tkLet
  of "macro": result = tkMacro
  of "method": result = tkMethod
  of "mixin": result = tkMixin
  of "mod": result = tkMod
  of "nil": result = tkNil
  of "not": result = tkNot
  of "notin": result = tkNotin
  of "object": result = tkObject
  of "of": result = tkOf
  of "or": result = tkOr
  of "out": result = tkOut
  of "proc": result = tkProc
  of "ptr": result = tkPtr
  of "raise": result = tkRaise
  of "ref": result = tkRef
  of "return": result = tkReturn
  of "shl": result = tkShl
  of "shr": result = tkShr
  of "static": result = tkStatic
  of "template": result = tkTemplate
  of "try": result = tkTry
  of "tuple": result = tkTuple
  of "type": result = tkType
  of "using": result = tkUsing
  of "var": result = tkVar
  of "when": result = tkWhen
  of "while": result = tkWhile
  of "xor": result = tkXor
  of "yield": result = tkYield
  of r"[a-zA-Z\0128-\0255](_?[a-zA-Z0-9\0128-\0255])*": result = tkSymbol

proc scanSymbol(L: var Lexer; tok: var Token) =
  let start = L.pos
  var pos = L.pos
  var kind = symbolKind(L.buf, pos)
  if kind == tkInvalid:
    tok.kind = tkInvalid
    tok.s = str(L.ch(pos))
    inc pos
    L.pos = pos
    return
  # A byte in `UnicodeOperatorStartChars` that begins a unicode operator ends
  # the identifier; the automaton has no way to know that, so cut here -- and
  # ask again what the shorter text is, since `and∙` starts with a keyword.
  var i = start
  while i < pos:
    if L.buf[i] in UnicodeOperatorStartChars and
       unicodeOprLen(L.buf, i)[0] != 0:
      pos = i
      let cut = L.buf.substr(start, pos-1)
      var p = 0
      kind = symbolKind(cut, p)
      if p != cut.len: kind = tkSymbol
      break
    inc i
  if pos == start:
    tok.kind = tkInvalid
    tok.s = str(L.ch(pos))
    inc pos
    L.pos = pos
    return
  if L.ch(pos) == '_':
    error L, start, "invalid token: trailing underscore"
  L.pos = pos
  tok.s = L.buf.substr(start, pos-1)
  tok.kind = kind

# ---------------------------------------------------------------------------
# Comments
# ---------------------------------------------------------------------------

proc skipMultiLineComment(L: var Lexer; tok: var Token; start: int;
                          isDoc: bool) =
  var pos = start
  var toStrip = 0
  if isDoc:
    toStrip = pos - L.lineStart
    while L.ch(pos) == ' ':
      inc pos
      inc toStrip
    while L.ch(pos) == '\r' or L.ch(pos) == '\n':
      pos = handleCRLF(L, pos)
      toStrip = 0
      while L.ch(pos) == ' ':
        inc pos
        inc toStrip
  var nesting = 0
  while true:
    let c = L.ch(pos)
    if c == '#':
      if isDoc:
        if L.ch(pos+1) == '#' and L.ch(pos+2) == '[': inc nesting
        tok.s.add '#'
      elif L.ch(pos+1) == '[':
        inc nesting
      inc pos
    elif c == ']':
      if isDoc:
        if L.ch(pos+1) == '#' and L.ch(pos+2) == '#':
          if nesting == 0:
            inc pos, 3
            break
          dec nesting
        tok.s.add ']'
      elif L.ch(pos+1) == '#':
        if nesting == 0:
          inc pos, 2
          break
        dec nesting
      inc pos
    elif c == '\r' or c == '\n':
      pos = handleCRLF(L, pos)
      if isDoc:
        tok.s.add '\n'
        var n = toStrip
        while L.ch(pos) == ' ' and n > 0:
          inc pos
          dec n
    elif c == '\0':
      error L, pos, "end of multiline comment expected"
      break
    else:
      if isDoc: tok.s.add c
      inc pos
  L.pos = pos

proc scanComment(L: var Lexer; tok: var Token) =
  ## Only doc comments get here: `skip` eats the ordinary ones. Consecutive
  ## `##` lines become **one** token, and the indentation of the line after
  ## the block is remembered in `indentAhead` for the token that follows.
  var pos = L.pos
  tok.kind = tkComment
  if L.ch(pos+2) == '[':
    skipMultiLineComment(L, tok, pos+3, true)
    return
  inc pos, 2
  var toStrip = 0
  var stripInit = false
  while true:
    if not stripInit:
      while L.ch(pos) == ' ':
        inc pos
        inc toStrip
      if L.ch(pos) == '\r' or L.ch(pos) == '\n': toStrip = 0
      else: stripInit = true
    while L.ch(pos) != '\r' and L.ch(pos) != '\n' and L.ch(pos) != '\0':
      tok.s.add L.buf[pos]
      inc pos
    if L.ch(pos) == '\0': break
    pos = handleCRLF(L, pos)
    var indent = 0
    while L.ch(pos) == ' ':
      inc pos
      inc indent
    if L.ch(pos) == '#' and L.ch(pos+1) == '#':
      tok.s.add '\n'
      inc pos, 2
      if stripInit:
        var n = toStrip
        while L.ch(pos) == ' ' and n > 0:
          inc pos
          dec n
    else:
      if L.ch(pos) > ' ': L.indentAhead = int32(indent)
      break
  L.pos = pos

# ---------------------------------------------------------------------------
# String and character literals
# ---------------------------------------------------------------------------

proc handleHexChar(L: var Lexer; xi: var int; position: range[0..4]) =
  let c = L.ch(L.pos)
  if c >= '0' and c <= '9': xi = (xi shl 4) or (ord(c) - ord('0'))
  elif c >= 'a' and c <= 'f': xi = (xi shl 4) or (ord(c) - ord('a') + 10)
  elif c >= 'A' and c <= 'F': xi = (xi shl 4) or (ord(c) - ord('A') + 10)
  else:
    if position <= 1: error L, L.pos, "expected a hex digit"
    return
  inc L.pos

proc addUnicodeCodePoint(s: var string; i: int) =
  ## UTF-8 encode; the only place `nimlexer` produces multi-byte output of its
  ## own rather than copying input.
  if i <= 127:
    s.add chr(i)
  elif i <= 0x07FF:
    s.add chr((i shr 6) or 0b1100_0000)
    s.add chr((i and 0b0011_1111) or 0b1000_0000)
  elif i <= 0xFFFF:
    s.add chr((i shr 12) or 0b1110_0000)
    s.add chr(((i shr 6) and 0b0011_1111) or 0b1000_0000)
    s.add chr((i and 0b0011_1111) or 0b1000_0000)
  else:
    s.add chr((i shr 18) or 0b1111_0000)
    s.add chr(((i shr 12) and 0b0011_1111) or 0b1000_0000)
    s.add chr(((i shr 6) and 0b0011_1111) or 0b1000_0000)
    s.add chr((i and 0b0011_1111) or 0b1000_0000)

proc getEscapedChar(L: var Lexer; tok: var Token) =
  inc L.pos                   # skip '\'
  let c = L.ch(L.pos)
  case c
  of 'n', 'N': tok.s.add '\n'; inc L.pos
  of 'p', 'P':
    if tok.kind == tkCharLit:
      error L, L.pos, "\\p not allowed in character literal"
    tok.s.add '\n'
    inc L.pos
  of 'r', 'R', 'c', 'C': tok.s.add '\r'; inc L.pos
  of 'l', 'L': tok.s.add '\n'; inc L.pos
  of 'f', 'F': tok.s.add '\f'; inc L.pos
  of 'e', 'E': tok.s.add '\e'; inc L.pos
  of 'a', 'A': tok.s.add '\a'; inc L.pos
  of 'b', 'B': tok.s.add '\b'; inc L.pos
  of 'v', 'V': tok.s.add '\v'; inc L.pos
  of 't', 'T': tok.s.add '\t'; inc L.pos
  of '\'', '\"': tok.s.add c; inc L.pos
  of '\\': tok.s.add '\\'; inc L.pos
  of 'x', 'X':
    inc L.pos
    var xi = 0
    handleHexChar L, xi, 1
    handleHexChar L, xi, 2
    tok.s.add chr(xi)
  of 'u', 'U':
    if tok.kind == tkCharLit:
      error L, L.pos, "\\u not allowed in character literal"
    inc L.pos
    var xi = 0
    if L.ch(L.pos) == '{':
      inc L.pos
      let start = L.pos
      while L.ch(L.pos) != '}' and L.ch(L.pos) != '\0':
        handleHexChar L, xi, 0
      if start == L.pos: error L, L.pos, "Unicode codepoint cannot be empty"
      if L.ch(L.pos) == '}': inc L.pos
      if xi > 0x10FFFF:
        error L, L.pos, "Unicode codepoint must be lower than 0x10FFFF"
    else:
      handleHexChar L, xi, 1
      handleHexChar L, xi, 2
      handleHexChar L, xi, 3
      handleHexChar L, xi, 4
    addUnicodeCodePoint tok.s, xi
  of '0'..'9':
    var xi = 0
    while L.ch(L.pos) >= '0' and L.ch(L.pos) <= '9':
      xi = xi * 10 + (ord(L.ch(L.pos)) - ord('0'))
      inc L.pos
    if xi <= 255: tok.s.add chr(xi)
    else: error L, L.pos, "invalid character constant"
  else:
    error L, L.pos, "invalid character constant"
    inc L.pos

type
  StringMode = enum smNormal, smRaw, smGeneralized

proc getString(L: var Lexer; tok: var Token; mode: StringMode) =
  var pos = L.pos
  let line = L.lineNumber
  inc pos                     # skip the opening quote
  if L.ch(pos) == '\"' and L.ch(pos+1) == '\"':
    tok.kind = tkTripleStrLit
    inc pos, 2
    # a newline directly after `"""` is not part of the string
    if L.ch(pos) == ' ' or L.ch(pos) == '\t':
      var newpos = pos + 1
      while L.ch(newpos) == ' ' or L.ch(newpos) == '\t': inc newpos
      if L.ch(newpos) == '\r' or L.ch(newpos) == '\n': pos = newpos
    if L.ch(pos) == '\r' or L.ch(pos) == '\n':
      pos = handleCRLF(L, pos)
    while true:
      let c = L.ch(pos)
      if c == '\"':
        if L.ch(pos+1) == '\"' and L.ch(pos+2) == '\"' and L.ch(pos+3) != '\"':
          pos = pos + 3
          break
        tok.s.add '\"'
        inc pos
      elif c == '\r' or c == '\n':
        pos = handleCRLF(L, pos)
        tok.s.add '\n'
      elif c == '\0':
        errorAt L, line, 0, "closing \"\"\" expected, but end of file reached"
        break
      else:
        tok.s.add c
        inc pos
    L.pos = pos
  else:
    tok.kind = if mode == smNormal: tkStrLit else: tkRStrLit
    while true:
      let c = L.ch(pos)
      if c == '\"':
        if mode != smNormal and L.ch(pos+1) == '\"':
          inc pos, 2
          tok.s.add '\"'
        else:
          inc pos
          break
      elif c == '\r' or c == '\n' or c == '\0':
        # at `L.pos`: the opening quote, or behind the last escape sequence
        error L, L.pos, "closing \" expected"
        break
      elif c == '\\' and mode == smNormal:
        L.pos = pos
        getEscapedChar L, tok
        pos = L.pos
      else:
        tok.s.add c
        inc pos
    L.pos = pos

proc getCharacter(L: var Lexer; tok: var Token) =
  let startPos = L.pos
  tok.kind = tkCharLit
  inc L.pos                   # skip '
  let c = L.ch(L.pos)
  if c < ' ' or c == '\'':
    error L, L.pos, "invalid character literal"
    tok.s = str(c)
  elif c == '\\':
    getEscapedChar L, tok
  else:
    tok.s = str(c)
    inc L.pos
  if L.ch(L.pos) == '\'':
    inc L.pos
  elif startPos > 0 and L.ch(startPos-1) == '`':
    # `'` as an operator name inside backticks
    tok.s = "'"
    L.pos = startPos + 1
  else:
    error L, L.pos, "missing closing ' for character literal"

# ---------------------------------------------------------------------------
# Operators
# ---------------------------------------------------------------------------

proc endOperator(L: var Lexer; tok: var Token; start, stop: int) =
  ## The five operator spellings that are punctuation rather than operators.
  tok.s = L.buf.substr(start, stop)
  if tok.s == ":": tok.kind = tkColon
  elif tok.s == "::": tok.kind = tkColonColon
  elif tok.s == "=": tok.kind = tkEquals
  elif tok.s == ".": tok.kind = tkDot
  elif tok.s == "..": tok.kind = tkDotDot
  else: tok.kind = tkOpr
  L.pos = stop + 1

proc getOperator(L: var Lexer; tok: var Token) =
  let start = L.pos
  var pos = L.pos
  while true:
    let c = L.ch(pos)
    if c in OpChars:
      inc pos
    elif c in UnicodeOperatorStartChars:
      let n = unicodeOprLen(L.buf, pos)[0]
      if n == 0: break
      inc pos, n
    else:
      break
  endOperator L, tok, start, pos-1
  # An operator's trailing spacing decides whether it is unary, so it is read
  # here without moving `L.pos`: the next token still gets its leading space.
  tok.spacing = tok.spacing - {tsTrailing, tsEof}
  var trailing = false
  while L.ch(pos) == ' ':
    inc pos
    trailing = true
  let c = L.ch(pos)
  if c == '\r' or c == '\n' or c == '\0': tok.spacing.incl tsEof
  elif trailing: tok.spacing.incl tsTrailing

# ---------------------------------------------------------------------------
# Whitespace, and the indentation it produces
# ---------------------------------------------------------------------------

proc skip(L: var Lexer; tok: var Token) =
  ## Everything that is not a token. `tok.indent` comes out of here: it is the
  ## column of the first token on a line and -1 for every other token, which
  ## is the single input the generated parser's `IndClass` is computed from.
  var pos = L.pos
  tok.spacing.excl tsLeading
  while true:
    let c = L.ch(pos)
    if c == ' ':
      inc pos
      tok.spacing.incl tsLeading
    elif c == '\t':
      error L, pos, "tabs are not allowed, use spaces instead"
      inc pos
    elif c == '\r' or c == '\n':
      pos = handleCRLF(L, pos)
      var indent = 0
      while true:
        if L.ch(pos) == ' ':
          inc pos
          inc indent
        elif L.ch(pos) == '#' and L.ch(pos+1) == '[':
          L.pos = pos
          skipMultiLineComment(L, tok, pos+2, false)
          pos = L.pos
        else:
          break
      tok.spacing.excl tsLeading
      if L.ch(pos) > ' ' and (L.ch(pos) != '#' or L.ch(pos+1) == '#'):
        tok.indent = int32(indent)
        L.currLineIndent = int32(indent)
        break
    elif c == '#':
      if L.ch(pos+1) == '#': break      # a doc comment is a token
      if L.ch(pos+1) == '[':
        L.pos = pos
        skipMultiLineComment(L, tok, pos+2, false)
        pos = L.pos
      else:
        while L.ch(pos) != '\r' and L.ch(pos) != '\n' and L.ch(pos) != '\0':
          inc pos
    else:
      break                             # end of file leaves the loop too
  L.pos = pos

# ---------------------------------------------------------------------------
# The main dispatch
# ---------------------------------------------------------------------------

proc openLexer*(src: string; filename = ""): Lexer =
  result = Lexer(buf: src, pos: 0, filename: filename, lineNumber: 1,
                 lineStart: 0, currLineIndent: 0, indentAhead: -1, errors: @[])
  # `nimlexbase` skips a UTF-8 BOM before the lexer ever sees the file; here
  # the whole source is one string, so it is skipped on the way in.
  if src.len >= 3 and src[0] == '\xEF' and src[1] == '\xBB' and src[2] == '\xBF':
    result.pos = 3
    result.lineStart = 3

proc next*(L: var Lexer; tok: var Token) =
  ## One token. Mirrors `rawGetTok` in `compiler/lexer.nim` branch for branch,
  ## because that is what the differential test compares against.
  let carry = tok.spacing * {tsLeading}
  tok = Token(kind: tkInvalid, s: "", indent: -1, spacing: carry,
              line: 0, col: 0, base: 10, suffixPos: -1)
  if L.indentAhead >= 0:
    tok.indent = L.indentAhead
    L.currLineIndent = L.indentAhead
    L.indentAhead = -1
  skip L, tok
  let c = L.ch(L.pos)
  tok.line = L.lineNumber
  tok.col = int32(L.pos - L.lineStart)

  if c in SymStartChars - {'r', 'R'} - UnicodeOperatorStartChars:
    scanSymbol L, tok
  elif c in UnicodeOperatorStartChars:
    if unicodeOprLen(L.buf, L.pos)[0] != 0: getOperator L, tok
    else: scanSymbol L, tok
  elif c == '#':
    scanComment L, tok
  elif c == '*':
    # `*:` is two tokens in `var v*: int`, and one operator in `a *: b`
    if L.ch(L.pos+1) == ':' and L.ch(L.pos+2) notin OpChars:
      endOperator L, tok, L.pos, L.pos
    else:
      getOperator L, tok
  elif c == ',':
    tok.kind = tkComma
    inc L.pos
  elif c == 'r' or c == 'R':
    if L.ch(L.pos+1) == '\"':
      inc L.pos
      getString L, tok, smRaw
    else:
      scanSymbol L, tok
  elif c == '(':
    inc L.pos
    if L.ch(L.pos) == '.' and L.ch(L.pos+1) != '.':
      tok.kind = tkParDotLe
      inc L.pos
    else:
      tok.kind = tkParLe
  elif c == ')':
    tok.kind = tkParRi
    inc L.pos
  elif c == '[':
    inc L.pos
    if L.ch(L.pos) == '.' and L.ch(L.pos+1) != '.':
      tok.kind = tkBracketDotLe
      inc L.pos
    elif L.ch(L.pos) == ':':
      tok.kind = tkBracketLeColon
      inc L.pos
    else:
      tok.kind = tkBracketLe
  elif c == ']':
    tok.kind = tkBracketRi
    inc L.pos
  elif c == '.':
    if L.ch(L.pos+1) == ']':
      tok.kind = tkBracketDotRi
      inc L.pos, 2
    elif L.ch(L.pos+1) == '}':
      tok.kind = tkCurlyDotRi
      inc L.pos, 2
    elif L.ch(L.pos+1) == ')':
      tok.kind = tkParDotRi
      inc L.pos, 2
    else:
      getOperator L, tok
  elif c == '{':
    inc L.pos
    if L.ch(L.pos) == '.' and L.ch(L.pos+1) != '.':
      tok.kind = tkCurlyDotLe
      inc L.pos
    else:
      tok.kind = tkCurlyLe
  elif c == '}':
    tok.kind = tkCurlyRi
    inc L.pos
  elif c == ';':
    tok.kind = tkSemiColon
    inc L.pos
  elif c == '`':
    tok.kind = tkAccent
    inc L.pos
  elif c == '_':
    inc L.pos
    if L.ch(L.pos) notin SymChars + {'_'}:
      tok.kind = tkSymbol
      tok.s = "_"
    else:
      tok.kind = tkInvalid
      tok.s = "_"
      error L, L.pos, "invalid token: _"
  elif c == '\"':
    # `foo"bar"` is a generalized string literal, `foo "bar"` is a command
    let mode = if L.pos > 0 and L.ch(L.pos-1) in SymChars: smGeneralized
               else: smNormal
    getString L, tok, mode
    if mode == smGeneralized:
      if tok.kind == tkRStrLit: tok.kind = tkGStrLit
      elif tok.kind == tkTripleStrLit: tok.kind = tkGTripleStrLit
  elif c == '\'':
    getCharacter L, tok
  elif c >= '0' and c <= '9':
    scanNumber L, tok
  elif c == '-':
    if L.ch(L.pos+1) >= '0' and L.ch(L.pos+1) <= '9' and
       (L.pos == 0 or L.ch(L.pos-1) in UnaryMinusWhitelist):
      scanNumber L, tok
    else:
      getOperator L, tok
  elif c in OpChars:
    getOperator L, tok
  elif c == '\0':
    tok.kind = tkEof
    tok.indent = 0
  else:
    tok.kind = tkInvalid
    tok.s = str(c)
    error L, L.pos, "invalid token: " & str(c) & " (\\" & $ord(c) & ")"
    inc L.pos

iterator tokens*(src: string; filename = ""): Token {.sideEffect.} =
  ## Every token of `src`, `tkEof` last.
  var L = openLexer(src, filename)
  var tok = Token(kind: tkInvalid, s: "", indent: -1, spacing: {},
                  line: 0, col: 0, base: 10, suffixPos: -1)
  while true:
    next L, tok
    yield tok
    if tok.kind == tkEof: break
