## Runtime the generated parsers are written against: a token stream that
## carries indentation, and an output buffer that supports inserting an
## opening token at a saved position (`wrap`), which is what lets the
## generator left-factor freely and still build the tree the unfactored
## grammar describes.
##
## This is the miniature version used by `tmini`: a `seq` of open/close/leaf
## records instead of a NIF buffer, and its own three-screen lexer, so the
## generated parser can be tested without the Nim grammar's dependencies. The
## real one is `src/nifler2/parserrt.nim`.

import std / [strutils, syncio]

type
  TokKind* = enum
    tkEof = "[EOF]", tkSymbol = "tkSymbol", tkIntLit = "tkIntLit",
    tkIf = "if", tkWhile = "while",
    tkColon = ":", tkEquals = "=", tkSemiColon = ";",
    tkComma = ",", tkDot = ".",
    tkParLe = "(", tkParRi = ")", tkBracketLe = "[", tkBracketRi = "]",
    tkOpr = "tkOpr"

  IndClass* = enum
    icNoInd, icLt, icEq, icGt

  Mark* = object        # `src/nifler2/parserrt.nim` also carries the line info
    pos*: int
    info*: int           # this runtime has no positions; the field keeps
                         # the generated `mark.info` references compiling

  Token* = object
    kind*: TokKind
    s*: string
    indent*: int32          # column when first on its line, else -1
    spaceBefore*: bool      # whitespace immediately before it
    line*, col*: int32

  BufKind = enum bOpen, bClose, bLeaf
  BufTok = object
    kind: BufKind
    text: string

  Parser* = object
    toks*: seq[Token]
    pos*: int
    currInd*: int32
    indStack*: seq[int32]
    buf*: seq[BufTok]
    errors*: seq[string]

proc tok*(p: Parser): Token = p.toks[p.pos]

# --------------------------------------------------------------- lexing

const OpChars = {'+', '-', '*', '/', '<', '>', '=', '^', '&', '%', '!', '~', '|'}

proc lex*(src: string): seq[Token] =
  result = @[]
  var i = 0
  var line = 1'i32
  var lineStart = 0
  var atLineStart = true
  var sawSpace = false
  while i < src.len:
    let c = src[i]
    if c == '\n':
      inc line
      inc i
      lineStart = i
      atLineStart = true
      sawSpace = true
      continue
    if c in {' ', '\t', '\r'}:
      inc i
      sawSpace = true
      continue
    if c == '#':
      while i < src.len and src[i] != '\n': inc i
      continue
    let col = int32(i - lineStart)
    var t = Token(indent: (if atLineStart: col else: -1'i32),
                  spaceBefore: sawSpace, line: line, col: col)
    atLineStart = false
    sawSpace = false
    if c in {'a'..'z', 'A'..'Z', '_'}:
      let start = i
      while i < src.len and src[i] in {'a'..'z', 'A'..'Z', '0'..'9', '_'}: inc i
      t.s = src.substr(start, i-1)
      t.kind = case t.s
        of "if": tkIf
        of "while": tkWhile
        else: tkSymbol
    elif c in {'0'..'9'}:
      let start = i
      while i < src.len and src[i] in {'0'..'9'}: inc i
      t.s = src.substr(start, i-1)
      t.kind = tkIntLit
    elif c in OpChars:
      let start = i
      while i < src.len and src[i] in OpChars: inc i
      t.s = src.substr(start, i-1)
      t.kind = if t.s == "=": tkEquals else: tkOpr
    else:
      t.s = $c
      inc i
      t.kind = case c
        of ':': tkColon
        of ';': tkSemiColon
        of ',': tkComma
        of '.': tkDot
        of '(': tkParLe
        of ')': tkParRi
        of '[': tkBracketLe
        of ']': tkBracketRi
        else: tkEof
    result.add t
  result.add Token(kind: tkEof, s: "", indent: 0, line: line, col: 0)

proc openParser*(src: string): Parser =
  Parser(toks: lex(src), pos: 0, currInd: 0, indStack: @[], buf: @[], errors: @[])

# --------------------------------------------------------------- primitives

proc error*(p: var Parser; msg: string) =
  let t = p.tok
  p.errors.add "(" & $t.line & "," & $t.col & ") " & msg &
               ", got '" & (if t.s.len > 0: t.s else: $t.kind) & "'"

proc getTok*(p: var Parser) =
  if p.pos < p.toks.high: inc p.pos

proc ruleError*(p: var Parser; rule: string; misplaced: bool) =
  error p, (if misplaced: "invalid indentation in " else: "expected ") & rule

proc indentError*(p: var Parser) =
  error p, "invalid indentation"

proc indClass*(p: Parser): IndClass =
  let ind = p.tok.indent
  if ind < 0: icNoInd
  elif ind < p.currInd: icLt
  elif ind == p.currInd: icEq
  else: icGt

proc afterOperator*(p: var Parser) =
  if indClass(p) == icLt: indentError p

proc checkInd*(p: var Parser; allowed: set[IndClass]) =
  if indClass(p) notin allowed:
    error p, "invalid indentation (" & $indClass(p) & " not in " & $allowed & ")"

proc pushInd*(p: var Parser) =
  checkInd p, {icGt}
  p.indStack.add p.currInd
  p.currInd = p.tok.indent

proc popInd*(p: var Parser) =
  p.currInd = p.indStack.pop()

proc expect*(p: var Parser; k: TokKind) =
  if p.tok.kind == k: getTok p
  else: error p, "expected '" & $k & "'"

proc mark*(p: Parser): Mark = Mark(pos: p.buf.len)

# ---- what the generated code needs for `binary(...)` and `&predicates`

proc noSpaceBefore*(p: Parser): bool =
  ## `f(x)` is a call, `f (x)` a command -- the same rule Nim uses.
  not p.tok.spaceBefore

proc getPrecedence*(p: Parser): int =
  ## Precedence from the operator's spelling, as Nim does it. Anything that is
  ## not an operator scores below every `limit`, which is what stops the loop.
  if p.tok.kind != tkOpr: return -1000
  case p.tok.s[0]
  of '$', '^': 10
  of '*', '/', '%': 9
  of '+', '-', '~', '|': 8
  of '&': 7
  of '.': 6
  of '=', '<', '>', '!': 5
  else: 4

proc isRightAssoc*(p: Parser): bool =
  p.tok.kind == tkOpr and p.tok.s[0] == '^'

proc insertLeafAt*(p: var Parser; m: Mark; text: string) =
  p.buf.insert(BufTok(kind: bLeaf, text: text), m.pos)

proc emitLeaf*(p: var Parser) =
  p.buf.add BufTok(kind: bLeaf, text: p.tok.s)
  getTok p

proc openTag*(p: var Parser; tag: string) =
  p.buf.add BufTok(kind: bOpen, text: tag)

proc closeTag*(p: var Parser) =
  p.buf.add BufTok(kind: bClose, text: "")

proc wrap*(p: var Parser; m: Mark; tag: string) =
  ## Insert the opening token at the mark and close at the end. The whole
  ## point of the design: the tag is decided after the fact.
  p.buf.insert(BufTok(kind: bOpen, text: tag), m.pos)
  p.buf.add BufTok(kind: bClose, text: "")

proc wrapAt*(p: var Parser; m: Mark; tag: string; info: int) =
  ## Positions are not modelled here, so this is `wrap`.
  wrap p, m, tag

proc info*(p: Parser): int {.inline.} = 0

proc discardUnused*(m: Mark) = discard

proc render*(p: Parser): string =
  result = ""
  var needSpace = false
  for t in p.buf:
    case t.kind
    of bOpen:
      if needSpace: result.add ' '
      result.add '('
      result.add t.text
      needSpace = true
    of bClose:
      result.add ')'
      needSpace = true
    of bLeaf:
      if needSpace: result.add ' '
      result.add t.text
      needSpace = true
