#
#
#           Nifler2: Nim to NIF
#        (c) Copyright 2026 Andreas Rumpf
#
#    See the file "license.txt", included in this
#    distribution, for details about the copyright.
#

## The runtime the generated parsers are written against.
##
## Two halves, and the generator knows nothing about either beyond the names:
##
## * **The token stream.** `nimlexer` produces it; everything the grammar's
##   semantic predicates ask about a token (`noSpaceBefore`, `isUnary`,
##   `dotLikeOps`, ...) is answered here, from `p.tok`. The indentation class
##   is the second half of the LL(1) decision domain, so `indClass`,
##   `checkInd`, `pushInd` and `popInd` are as much a part of the interface as
##   `expect` is.
##
## * **The output buffer.** A `nifcore.TokenBuf`, built *out of order*: a
##   `mark` records a position, and `wrap` retroactively inserts the opening
##   tag there. That is what lets the generator left-factor freely and still
##   build the tree the unfactored grammar describes -- and it is why the
##   output cannot be a streaming `nifbuilder`.
##
## `wrap` is the whole trick, so it is worth saying how it avoids duplicating
## nifcore's jump arithmetic: a wrapped node always ends at the *current* end
## of the buffer (marks nest, they never interleave), so after splicing the
## head token in at the mark, `reopenLastTree` + `addParRi` is exactly the
## situation nifcore's own `closeTag` is written for -- including the
## `ExtendedSuffix` it splices in when a body overflows the 19-bit jump field.

import std / [parseutils, syncio]
import ".." / lib / nifpools
import nimlexer
export nimlexer
export nifpools

type
  IndClass* = enum ## the indentation half of the LL(1) decision domain
    icNoInd,  ## not the first token on its line
    icLt,     ## first on its line, left of the current indentation
    icEq,     ## first on its line, at the current indentation
    icGt      ## first on its line, right of the current indentation

  PrimaryMode* = enum ## `parser.nim`'s `PrimaryMode`, threaded by the grammar
    pmNormal, pmTypeDesc, pmTypeDef, pmTrySimple

  Mark* = object ## where a node *would* start, if the rule turns out to build one
    pos*: int              ## token index into `Parser.dest`
    info*: NifLineInfo     ## position of the token that was current at `mark`

  Parser* = object
    lex*: Lexer
    tok*: Token
    dest*: TokenBuf
    head: TokenBuf         ## scratch for the one head `wrap`/`insertLeafAt` splice
    file: FileId
    currInd*: int32
    indStack: seq[int32]
    errors*: seq[string]
    inPragma*: int         ## `{.` ... `.}` nesting; a pragma has no indentation
    section*: string       ## the tag a declaration fans out into: var/let/param/...

proc openParser*(src, filename: string): Parser =
  result = Parser(lex: openLexer(src, filename),
                  tok: Token(kind: tkInvalid, s: "", indent: -1, spacing: {},
                             line: 0, col: 0, base: 10, suffixPos: -1,
                             iNumber: 0),
                  dest: createTokenBuf(src.len div 3 + 16),
                  head: createTokenBuf(4),
                  file: pool.filenames.getOrIncl(filename),
                  currInd: 0, indStack: @[], errors: @[],
                  inPragma: 0, section: "var")
  next result.lex, result.tok

proc info*(p: Parser): NifLineInfo {.inline.} =
  NifLineInfo(file: p.file, line: p.tok.line, col: p.tok.col)

# --------------------------------------------------------------- diagnostics

proc error*(p: var Parser; msg: string) =
  ## The first syntax error ends the parse. Recovery would mean bailing out of
  ## an arbitrarily deep recursion, and the generated code cannot: there are no
  ## exceptions in this runtime and no notation for a recovery production. It
  ## also has to end the parse rather than merely record it, because `expect`
  ## does not consume the token it did not match -- so a repetition whose body
  ## fails makes no progress and would spin.
  p.errors.add p.lex.filename & "(" & $p.tok.line & ", " & $p.tok.col & ") " &
               msg & ", got '" &
               (if p.tok.s.len > 0: p.tok.s else: $p.tok.kind) & "'"
  quit p.errors[0]

proc getTok*(p: var Parser) =
  if p.tok.kind != tkEof: next p.lex, p.tok

proc expect*(p: var Parser; k: TokKind) =
  if p.tok.kind == k: getTok p
  else: error p, "expected '" & $k & "'"

proc expect*(p: var Parser; k: TokKind; s: string) =
  ## The spelling matters for the operators the grammar names literally
  ## (`'->'` in `paramListArrow`).
  if p.tok.kind == k and p.tok.s == s: getTok p
  else: error p, "expected '" & s & "'"

# --------------------------------------------------------------- indentation

proc emitLeaf*(p: var Parser)

proc expectLeaf*(p: var Parser; k: TokKind) =
  ## `@'not'` in the grammar: the terminal is content, not punctuation.
  if p.tok.kind == k: emitLeaf p
  else: error p, "expected '" & $k & "'"

proc expectLeaf*(p: var Parser; k: TokKind; s: string) =
  if p.tok.kind == k and p.tok.s == s: emitLeaf p
  else: error p, "expected '" & s & "'"

proc indClass*(p: Parser): IndClass =
  if p.tok.indent < 0 or p.inPragma > 0: icNoInd
  elif p.tok.indent < p.currInd: icLt
  elif p.tok.indent == p.currInd: icEq
  else: icGt

proc indSetStr(s: set[IndClass]): string =
  result = "{"
  if icNoInd in s: result.add "NO_IND "
  if icLt in s: result.add "IND{<} "
  if icEq in s: result.add "IND{=} "
  if icGt in s: result.add "IND{>} "
  result.add "}"

proc indName(c: IndClass): string =
  case c
  of icNoInd: "NO_IND"
  of icLt: "IND{<}"
  of icEq: "IND{=}"
  of icGt: "IND{>}"

proc checkInd*(p: var Parser; allowed: set[IndClass]) =
  if indClass(p) notin allowed:
    error p, "invalid indentation: " & indName(indClass(p)) & " not in " &
             indSetStr(allowed)

proc pushInd*(p: var Parser) =
  checkInd p, {icGt}
  p.indStack.add p.currInd
  p.currInd = p.tok.indent

proc pushIndAny*(p: var Parser) =
  ## `withInd` without the `realInd` assertion: the indentation becomes the
  ## current token's whatever that is, including -1 for a token that is not
  ## first on its line. `semiStmtList` is the one place `parser.nim` does
  ## this, and it is why `(\n  when a: x\n  elif b: y)` has its `elif` at the
  ## same indentation as its `when` rather than at the enclosing block's.
  p.indStack.add p.currInd
  p.currInd = p.tok.indent

proc popInd*(p: var Parser) =
  if p.indStack.len > 0:
    p.currInd = p.indStack[p.indStack.len-1]
    p.indStack.setLen p.indStack.len - 1

# --------------------------------------------------------------- predicates
#
# The grammar's `&name` and `&name(args)` items. Four of the six come straight
# from `nimlexer` -- they are properties of a token, which is where Nim's
# lexer computes them too -- and are lifted to the parser here only because
# the generated code spells them `name(p)`.

proc noSpaceBefore*(p: Parser): bool {.inline.} =
  ## `f(x)` is a call, `f (x)` a command.
  tsLeading notin p.tok.spacing

proc isUnary*(p: Parser): bool {.inline.} = isUnary(p.tok)
proc isSigilLike*(p: Parser): bool {.inline.} = isSigilLike(p.tok)

proc dotLikeOps*(p: Parser): bool {.inline.} =
  ## `nimPreviewDotLikeOps`: `a.?b` parses as a field access rather than as an
  ## infix operator. The switch is gone in Nim 2, so the answer is constant.
  isDotLike(p.tok)

proc inTypeDesc*(p: Parser; mode: PrimaryMode): bool {.inline.} =
  ## `parser.nim`'s `if mode == pmTypeDesc` in `commandParam`.
  mode == pmTypeDesc

proc parIsTuple*(p: Parser; mode: PrimaryMode): bool {.inline.} =
  ## `parser.nim`'s `identOrLiteral` takes `'('` to the comma-separated
  ## `exprColonEqExprList` in a type, and to `parsePar` -- which also accepts a
  ## statement list, an assignment and a `do` block -- everywhere else. Both
  ## productions exist in the grammar; this is the discriminator that says
  ## which one `'('` opens.
  mode in {pmTypeDesc, pmTypeDef}

proc inOrOut*(p: Parser): bool {.inline.} =
  ## `parseGenericParam`'s `of tkIn, tkOut:` -- the variance markers of
  ## `MyPtr[out T]`. The grammar spells the operand as `KEYW` so that the
  ## keyword is *emitted* as the prefix operator's name, which a terminal
  ## would not be; this narrows `KEYW` back to the two that are meant.
  p.tok.kind in {tkIn, tkOut}

proc pragmaOnPrimary*(p: Parser; mode: PrimaryMode): bool {.inline.} =
  ## `simpleExprAux`'s `if p.tok.tokType == tkCurlyDotLe and (p.tok.indent < 0
  ## or realInd(p)) and mode == pmNormal`. The indentation half is spelled in
  ## the grammar; this is the mode half. `pmTrySimple` counts because
  ## `simpleExprAux` rewrites it to `pmNormal` right after `primary` returns,
  ## before it looks for the pragma.
  mode in {pmNormal, pmTrySimple}

proc isTypedefOperand*(p: Parser; mode: PrimaryMode): bool {.inline.} =
  ## `parseTypeDescKAux`'s `isTypedef`: after `ref`/`ptr`/`distinct` in a type
  ## *definition*, an `object` or `tuple` operand brings a whole declaration
  ## with it -- `type T = ref object` and its indented field list -- while
  ## anything else is just a `primary`. It is a two-token decision in
  ## `parser.nim` and a one-token one here, because by the time the operand is
  ## dispatched the keyword is already consumed.
  mode == pmTypeDef and p.tok.kind in {tkObject, tkTuple}

proc typeOperandFollows*(p: Parser): bool {.inline.} =
  ## `parseTypeDescKAux`'s `not isOperator(p.tok) and isExprStart(p)`: what
  ## makes `ptr` in `SomeInteger | ptr | pointer` a *bare* `ptr` rather than
  ## the prefix of `(prefix | pointer)`. The `isExprStart` half is the
  ## alternative's FIRST set and is already in the generated condition; the
  ## operator half is not expressible there, because an operator can also
  ## start a prefix expression.
  p.tok.kind notin {tkOpr, tkDiv, tkMod, tkShl, tkShr, tkIn, tkNotin, tkIs,
                    tkIsnot, tkNot, tkOf, tkAs, tkFrom, tkDotDot, tkAnd,
                    tkOr, tkXor}

proc commandStart*(p: Parser): bool {.inline.} =
  ## `parser.nim`'s guard on `primarySuffix`'s command branch. The token set is
  ## the one its `case` lists, which is narrower than FIRST(commandParam):
  ## `not`, `if`, `addr` and friends can start an expression but not a
  ## command, so `ref int not nil` is `(infix not (ref int) nil)` rather than
  ## a command whose argument is `not nil`. An *infix* operator is not the
  ## start of a command either -- `import std / os` is `(infix / std os)` --
  ## and inside a pragma nothing is, because `{.push hints:off.}` must not
  ## become `{.push(hints:off).}`.
  p.inPragma == 0 and
    p.tok.kind in {tkSymbol, tkAccent, tkIntLit..tkCustomLit, tkNil, tkCast,
                   tkOpr, tkDotDot, tkVar, tkOut, tkStatic, tkType, tkEnum,
                   tkTuple, tkObject, tkProc, tkParLe, tkBracketLe, tkCurlyLe} and
    (isUnary(p.tok) or p.tok.kind notin {tkOpr, tkDotDot})

proc commandAllowed*(p: Parser; mode: PrimaryMode): bool {.inline.} =
  ## `parser.nim`'s `commandExpr` returns its operand untouched when the mode
  ## is `pmTrySimple`, so in that mode a command is not a suffix at all. That
  ## is what leaves `echo a, b` for `exprStmt` to parse as one command with two
  ## parameters, instead of `(cmd echo a)` followed by a stray comma.
  mode != pmTrySimple

proc suffixStart*(p: Parser): bool {.inline.} =
  ## `parser.nim`'s `primarySuffix` loop guard, which the grammar cannot spell:
  ## a suffix continues on the same line, and a `.` may also open a
  ## continuation line that is indented at least as far as the line the primary
  ## started on. That last indentation is a *parameter* of `primarySuffix`
  ## there; here it is the enclosing block's, which differs only for a primary
  ## that already spans lines.
  p.tok.indent < 0 or (p.tok.kind == tkDot and p.tok.indent >= p.currInd)

proc getPrecedence*(p: Parser): int {.inline.} =
  ## `parseOperators` loops while `opPrec >= limit and p.tok.indent < 0 and
  ## not isUnary(p.tok)`. The unary test lives here rather than in the
  ## generator's `binary(...)`, because "a unary operator has no infix
  ## precedence" is a fact about Nim's operators and not about precedence
  ## climbing. Without it `echo $kind, "a"` parsed as `(infix $ echo kind)`
  ## and then stalled on the comma.
  if isUnary(p.tok): -10 else: getPrecedence(p.tok)
proc isRightAssoc*(p: Parser): bool {.inline.} = isRightAssoc(p.tok)

# --------------------------------------------------------------- the buffer

proc mark*(p: Parser): Mark {.inline.} =
  Mark(pos: p.dest.len, info: p.info)

proc discardUnused*(m: Mark) {.inline.} = discard
  ## A mark the rule turned out not to need. The generator emits one per
  ## alternative because it cannot know, before it has seen the whole
  ## alternative, whether a tag will claim it.

proc splice(p: var Parser; pos: int) =
  ## Move `p.head`'s tokens into `p.dest` at `pos`. Growing through `add`
  ## rather than `growRawUninit` is deliberate: the latter sets capacity to
  ## *exactly* the new length, so a one-token splice per node would realloc
  ## the whole buffer on every node.
  let n = p.head.len
  let oldLen = p.dest.len
  for i in 0 ..< n: p.dest.add p.head[i]
  var src = oldLen - 1
  while src >= pos:
    p.dest[src + n] = p.dest[src]
    dec src
  for i in 0 ..< n:
    p.dest[pos + i] = p.head[i]

proc wrap*(p: var Parser; m: Mark; tag: string) =
  ## Retroactively make everything from `m` a `(tag ...)` node.
  p.head.shrink 0
  addParLe(p.head, registerTag(tag), m.info)
  splice p, m.pos
  # The node ends at the end of the buffer -- which is precisely the situation
  # `reopenLastTree` exists for, so nifcore computes the jump, overflow and all.
  reopenLastTree(p.dest, m.pos)
  addParRi p.dest

proc insertLeafAt*(p: var Parser; m: Mark; text: string)

proc insertTokAt*(p: var Parser; m: Mark; k: TokKind) =
  ## `^tag[ @'not' ... ]`: consume the operator and insert it at the anchor,
  ## in front of the operand that is already on the buffer.
  if p.tok.kind == k:
    insertLeafAt p, m, (if p.tok.s.len > 0: p.tok.s else: $p.tok.kind)
    getTok p
  else:
    error p, "expected '" & $k & "'"

proc insertLeafAt*(p: var Parser; m: Mark; text: string) =
  ## `binary(...)`'s operator: `a + b` is `(infix + a b)`, so the operator has
  ## to land *before* the left operand, which was emitted before it was read.
  p.head.shrink 0
  addIdent(p.head, text, p.info)
  splice p, m.pos

proc fanOut*(m: Mark; section: string) =
  ## GAP. `declColonEquals` parses `a, b: T = v` and NIF has no such node: it
  ## wants one `(var a T v) (var b T v)` per name. Doing that here needs the
  ## *number of names*, and the generated code does not pass it -- the buffer
  ## alone cannot tell a trailing name from a type from a value. Giving the
  ## notation a way to say "this item repeats, count it" is the missing piece;
  ## until then the declaration stays as parsed and `p.section` is unused.
  discard

proc hexVal(c: char): uint64 {.inline.} =
  if c <= '9': uint64(ord(c) - ord('0'))
  elif c <= 'F': uint64(ord(c) - ord('A') + 10)
  else: uint64(ord(c) - ord('a') + 10)

proc floatValue(tok: Token): float64 =
  ## Nim's lexer leaves a float literal as text, so the conversion happens
  ## here. A base-16/8/2 literal that carries a float suffix is not a number
  ## in that base at all -- `0x3F800000'f32` names the *bit pattern* -- which
  ## is why the two cases do not share a parser.
  if tok.base == 10:
    var f = 0.0
    discard parseBiggestFloat(tok.s, f)
    result = f
  else:
    let shiftBy = if tok.base == 16: 4 elif tok.base == 8: 3 else: 1
    var xi = 0'u64
    var i = 2                            # past `0x` / `0o` / `0c` / `0b`
    while i < tok.s.len:
      if tok.s[i] != '_': xi = (xi shl shiftBy) or hexVal(tok.s[i])
      inc i
    if tok.kind == tkFloat32Lit:
      result = float64(cast[float32](uint32(xi and 0xffffffff'u64)))
    else:
      result = cast[float64](xi)

proc emitLeaf*(p: var Parser) =
  ## The terminal the grammar matched, as a NIF atom. Which atom is decided by
  ## the token kind alone, so the generator never has to say (it emits the
  ## terminal's name as a comment only).
  let info = p.info
  case p.tok.kind
  of tkIntLit, tkInt64Lit:
    addIntLit(p.dest, p.tok.iNumber, info)
  of tkInt8Lit, tkInt16Lit, tkInt32Lit:
    p.dest.buildTree registerTag("suf"), info:
      addIntLit(p.dest, p.tok.iNumber, info)
      addStrLit(p.dest, (if p.tok.kind == tkInt8Lit: "i8"
                         elif p.tok.kind == tkInt16Lit: "i16" else: "i32"), info)
  of tkUIntLit, tkUInt64Lit:
    addUIntLit(p.dest, cast[uint64](p.tok.iNumber), info)
  of tkUInt8Lit, tkUInt16Lit, tkUInt32Lit:
    p.dest.buildTree registerTag("suf"), info:
      addUIntLit(p.dest, cast[uint64](p.tok.iNumber), info)
      addStrLit(p.dest, (if p.tok.kind == tkUInt8Lit: "u8"
                         elif p.tok.kind == tkUInt16Lit: "u16" else: "u32"), info)
  of tkFloatLit, tkFloat64Lit:
    addFloatLit(p.dest, floatValue(p.tok), info)
  of tkFloat32Lit, tkFloat128Lit:
    p.dest.buildTree registerTag("suf"), info:
      addFloatLit(p.dest, floatValue(p.tok), info)
      addStrLit(p.dest, (if p.tok.kind == tkFloat32Lit: "f32" else: "f128"), info)
  of tkStrLit:
    addStrLit(p.dest, p.tok.s, info)
  of tkRStrLit, tkTripleStrLit:
    p.dest.buildTree registerTag("suf"), info:
      addStrLit(p.dest, p.tok.s, info)
      addStrLit(p.dest, (if p.tok.kind == tkRStrLit: "R" else: "T"), info)
  of tkCharLit:
    addCharLit(p.dest, (if p.tok.s.len > 0: p.tok.s[0] else: '\0'), info)
  of tkCustomLit:
    p.dest.buildTree registerTag("suf"), info:
      addStrLit(p.dest, p.tok.s.substr(0, int(p.tok.suffixPos) - 1), info)
      addStrLit(p.dest, p.tok.s.substr(int(p.tok.suffixPos) + 1), info)
  of tkComment:
    addStrLit(p.dest, p.tok.s, info)
  else:
    # Identifiers, operators and every keyword used as a name. `tkSymbol` is
    # the common case; the rest reach here through `symbolOrKeyword`, `OPR`
    # and the `quoted[...]` production.
    addIdent(p.dest, (if p.tok.s.len > 0: p.tok.s else: $p.tok.kind), info)
  getTok p
