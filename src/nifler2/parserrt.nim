{.feature: "lenientnils".}

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
import linkedtok
export linkedtok
from ".." / lib / nifcore import createTokenBuf
import ".." / models / nifler_tags

export nifler_tags
import nimlexer
export nimlexer
export nifpools

template grammar*(rules: varargs[untyped]) {.plugin: "deps/parsegen".}
  ## The grammar notation of `doc/internals/parser_generator.md`, turned into
  ## one `pRule` proc per rule at compile time.

type
  IndClass* = enum ## the indentation half of the LL(1) decision domain
    icNoInd,  ## not the first token on its line
    icLt,     ## first on its line, left of the current indentation
    icEq,     ## first on its line, at the current indentation
    icGt      ## first on its line, right of the current indentation

  PrimaryMode* = enum ## `parser.nim`'s `PrimaryMode`, threaded by the grammar
    pmNormal, pmTypeDesc, pmTypeDef, pmTrySimple

  OptSigs* = object
    ## How often the two optional grammar parts that a layout rewrite depends
    ## on have been *built*. A layout compares its mark's snapshot against the
    ## current counts: unchanged means the part did not match inside the rule,
    ## the rewrite is the identity, and the tokens can stay exactly where the
    ## rule wrote them -- no scratch copy, no cursor, no rewrite.
    ##
    ## Both are bumped where the thing is created, and both creators are in
    ## this module, so a count can never be missed. A nested match bumps a
    ## count the enclosing rule did not cause; that only costs the enclosing
    ## layout its old path, which is why over-reporting is harmless and
    ## under-reporting would not be.
    posMarkers*: int       ## `posMarker`: the `[:` of `x.y[:T](...)`, and `do`
    kvs*: int              ## a `kv` node: what makes a call an `oconstr` and
                           ## a curly a table constructor

  Mark* = object ## where a node *would* start, if the rule turns out to build one
    prev*: Node            ## the sibling that was last when the rule began;
                           ## `nil` when nothing had been written yet
    info*: NifLineInfo     ## position of the token that was current at `mark`
    sigs*: OptSigs         ## `OptSigs` as the rule began

  Parser* = object
    lex*: Lexer
    tok*: Token
    arena*: Arena          ## every cell of the tree; freed in one go
    first*, last*: Node    ## the chain of siblings being built
    file: FileId
    currInd*: int32
    indStack: seq[int32]
    inPragma*: int         ## `{.` ... `.}` nesting; a pragma has no indentation
    prevKind*: TokKind     ## the token before `tok`; `tkInvalid` at the start
    filterFailed*: bool    ## a source filter reported an error
    prevEndLine, prevEndCol: int ## where that token ended
    inSemiStmtList*: int   ## `( stmt; stmt )` nesting, as in parser.nim
    sigs*: OptSigs         ## see `OptSigs`
    sections: seq[NiflerKind] ## the tag a declaration fans out into: var/let/param/...
    lastSection: NiflerKind ## the section opened most recently, popped or not
    infos: seq[NifLineInfo] ## see `pushInfo`
    wrapFields: seq[bool]  ## whether a multi-name field is wrapped in `stmts`
    pool*: Pool            ## the literals pool the cells intern into
    tags*: TagPool         ## the tag pool a flattened buffer is built with
    failed*: bool          ## a syntax error ended the parse; see `errorAt`
    errLine*, errCol*: int ## where, `errCol` 0-based
    errMsg*: string        ## what

proc openParser*(src, filename: string; pool: Pool; tags: TagPool): Parser =
  ## `pool` and `tags` are the output's: nifler2 writes through `nifpools`'
  ## globals, a plugin parses into its own. `tags` is not stored -- a cell
  ## holds a `TagId` and the tag pool is only consulted when the tree is
  ## written out.
  result = Parser(lex: openLexer(src, filename),
                  tok: Token(kind: tkInvalid, s: "", indent: -1, spacing: {},
                             line: 0, col: 0, base: 10, suffixPos: -1,
                             iNumber: 0),
                  arena: initArena(), first: nil, last: nil,
                  file: pool.filenames.getOrIncl(filename),
                  currInd: 0, indStack: @[],
                  inPragma: 0, sections: @[], lastSection: VarL, wrapFields: @[],
                  pool: pool, tags: tags,
                  failed: false, errLine: 0, errCol: 0, errMsg: "")
  next result.lex, result.tok

proc close*(p: var Parser) =
  ## Releases the whole tree. One `dealloc` per arena block -- the cells have
  ## no hooks and the chain is not walked.
  destroy p.arena
  p.first = nil
  p.last = nil

proc finish*(p: var Parser): TokenBuf =
  ## The parsed tree as packed tokens, for everything that reads a `TokenBuf`.
  result = nifcore.createTokenBuf(1024, p.pool, p.tags)
  flatten(result, p.first, p.pool)

proc info*(p: Parser): NifLineInfo {.inline.} =
  NifLineInfo(file: p.file, line: p.tok.line, col: p.tok.col)

# --------------------------------------------------------------- diagnostics
#
# The messages, and where they point, are `compiler/parser.nim`'s: nifler's
# users see them, and `tools/errsweep.sh` compares the two. Nim positions a
# message either at the current token (`parMessage`) or at the lexer's
# position, which is the end of that token (`lexMessage`, used by `eat`).

const
  errInvalidIndentation* = "invalid indentation"
  NestableStmts = {tkIf, tkWhile, tkCase, tkTry, tkFor, tkBlock, tkAsm, tkProc,
                   tkFunc, tkIterator, tkMacro, tkType, tkConst, tkWhen, tkVar}

proc prettyTok*(t: Token): string =
  ## `prettyTok` in `compiler/lexer.nim`.
  case t.kind
  of KeywordLow..KeywordHigh: "keyword " & $t.kind
  of tkIntLit..tkInt64Lit: $t.iNumber
  of tkUIntLit..tkUInt64Lit:
    # Nim's `iNumber` is a `BiggestInt` for these too
    $t.iNumber
  of tkParLe..tkColon, tkEof, tkAccent: $t.kind
  of tkBracketLeColon: ""
  of tkColonColon, tkEquals, tkDot, tkDotDot:
    if t.s.len > 0: t.s else: $t.kind
  else: t.s

proc errorAt*(p: var Parser; line, col: int; msg: string) =
  ## The first syntax error ends the parse. Recovery would mean bailing out of
  ## an arbitrarily deep recursion, and the generated code cannot: there are no
  ## exceptions in this runtime and no notation for a recovery production. So
  ## the error is recorded and the token stream ends *here*: from now on the
  ## current token is an end of file that `getTok` never moves past. That is
  ## what unwinds the recursion -- every repetition's continuation test fails
  ## on it, and a mandatory item that does not match reports into an error
  ## that is already recorded. It has to end the stream rather than merely
  ## record the error, because `expect` does not consume the token it did not
  ## match, so a repetition whose body fails would make no progress and spin.
  ## A rule cut short leaves fewer trees than its layout expects, which is why
  ## the layouts check `failed`.
  if not p.failed:
    p.failed = true
    p.errLine = line
    p.errCol = col
    p.errMsg = msg
    p.tok.kind = tkEof
    p.tok.indent = 0
    p.tok.s = ""

proc reportFailure*(p: Parser) =
  ## nifler's output for a syntax error, and its exit code. The lexer's
  ## messages come first, which is where Nim, one token ahead as well, prints
  ## them; the stream ended at the error, so they are the ones up to it.
  for e in p.lex.errors: echo e
  echo p.lex.filename, "(", p.errLine, ", ", p.errCol + 1, ") Error: ", p.errMsg
  quit 1

proc error*(p: var Parser; msg: string) =
  ## `parMessage`: at the current token.
  errorAt p, p.tok.line, p.tok.col, msg

proc lexError*(p: var Parser; msg: string) =
  ## `lexMessage`: at the lexer's position, the end of the current token.
  errorAt p, p.lex.lineNumber, p.lex.pos - p.lex.lineStart, msg

proc expectedTok(p: var Parser; spelling: string) =
  lexError p, "expected: '" & spelling & "', but got: '" & prettyTok(p.tok) & "'"

proc identExpected*(p: var Parser) =
  error p, "identifier expected, but got '" & prettyTok(p.tok) & "'"

proc exprExpected*(p: var Parser) =
  error p, "expression expected, but found '" & prettyTok(p.tok) & "'"

proc ruleError*(p: var Parser; rule: string; misplaced: bool) =
  ## Nothing in `rule` starts with the current token. parser.nim has no such
  ## single place: the message is whichever check of the hand-written
  ## procedure the token trips first. For a token that could start the rule
  ## at another indentation (`misplaced`) that is usually an indentation
  ## check, and otherwise `identOrLiteral`'s `errExprExpected`.
  case rule
  of "section_variable", "section_constant", "section_typeDef", "symbol",
     "plainSymbol", "identVis", "identVisDot", "identWithPragma",
     "identWithPragmaDot", "qualifiedIdent", "forHead",
     "symbolOrKeyword":
    identExpected p
  of "complexOrSimpleStmt":
    # `'type' section(typeDef)` and `'type' typeof[...]` factor into one
    # dispatch after `type`; parser.nim's `parseSection` owns that token
    if p.prevKind == tkType: identExpected p
    else: exprExpected p
  of "declColonEquals", "identColonEquals", "declColonEqualsDot":
    error p, "':' or '=' expected, but got '" & prettyTok(p.tok) & "'"
  of "castExpr":
    expectedTok p, "("
  of "genericParamName":
    # a keyword other than `in`/`out` ends `parseGenericParamList`'s loop
    expectedTok p, "]"
  of "par":
    # `parsePar` ends in `optPar(p); eat(p, tkParRi)`
    if misplaced or indClass(p) == icLt: indentError p
    elif p.prevKind == tkParLe: exprExpected p
    else: expectedTok p, ")"
  of "pragma":
    error p, "expected '.}'"
  of "stmt":
    if indClass(p) == icGt:
      # `parseStmt`'s block loop parses anything but these as a statement
      if p.tok.kind in {tkCurlyRi, tkParRi, tkCurlyDotRi, tkBracketRi, tkElse, tkElif}:
        indentError p
      else:
        exprExpected p
    elif p.tok.kind in NestableStmts:
      error p, "nestable statement requires indentation"
    elif p.tok.indent >= 0 and p.inSemiStmtList == 0:
      error p, errInvalidIndentation
    else:
      exprExpected p
  else:
    if misplaced: indentError p
    else: exprExpected p

proc strictListEnd*(p: var Parser) =
  ## See `strictListEnd` in the grammar.
  if indClass(p) == icEq and p.tok.kind != tkEof: identExpected p

proc strictListStart*(p: var Parser) =
  ## An indented token that cannot start the list at all.
  if indClass(p) == icGt and p.tok.kind != tkEof: identExpected p

proc enumListEnd*(p: var Parser) =
  ## `parseEnum` loops on `validInd` and calls `parseSymbol` on whatever is
  ## there.
  if indClass(p) in {icNoInd, icGt} and p.tok.kind != tkEof: identExpected p

proc paramStart*(p: var Parser) =
  ## `parseParamList`'s messages for a token that starts no parameter.
  if p.tok.kind in {tkSymbol, tkAccent, tkParRi}: discard
  elif p.tok.kind == tkVar:
    error p, "the syntax is 'parameter: var T', not 'var parameter: T'"
  elif p.tok.kind in KeywordLow..KeywordHigh:
    error p, "'" & $p.tok.kind & "' is a keyword and cannot be used as a parameter name"
  else:
    error p, "expected closing ')'"

proc accentEnd*(p: var Parser) =
  if p.tok.kind != tkAccent: identExpected p

proc listEnd*(p: var Parser; close: TokKind) =
  ## See `listEnd` in the grammar. A pragma ends in `.}` or `}`.
  if p.tok.kind != close and p.tok.kind != tkEof and
      not (close == tkCurlyDotRi and p.tok.kind == tkCurlyRi):
    exprExpected p

proc missingEquals*(p: var Parser) =
  ## A routine without a body followed by an indented line.
  if indClass(p) == icGt and p.tok.kind notin {tkComment, tkEof}:
    error p, "invalid indentation, maybe you forgot a '=' at " & p.lex.filename &
             "(" & $p.prevEndLine & ", " & $(p.prevEndCol + 1) & ") ?"

proc requireFields*(p: var Parser; m: Mark) =
  if p.last == m.prev: identExpected p      # nothing written since the mark

proc funcType*(p: var Parser) =
  if p.prevKind != tkFunc:
    error p, "func keyword is not allowed in type descriptions, use proc with {.noSideEffect.} pragma instead"

proc stmtListEnd*(p: var Parser) =
  ## `parseStmt`'s block loop ends on these; any other token at the block's
  ## indentation is handed to `complexOrSimpleStmt`, which finds nothing.
  if indClass(p) == icEq and p.tok.kind notin {tkCurlyRi, tkParRi, tkCurlyDotRi,
      tkBracketRi, tkElse, tkElif, tkEof}:
    exprExpected p

proc requireExcept*(p: var Parser; m: Mark) =
  if p.last == m.prev: error p, "expected 'except'"

proc noIndHere*(p: var Parser) =
  if p.tok.indent >= 0: error p, errInvalidIndentation

proc blockNameEnd*(p: var Parser) =
  if p.tok.kind != tkColon: identExpected p

proc tupleEnd*(p: var Parser) =
  if p.prevKind == tkComma and p.tok.kind notin {tkParRi, tkEof}: exprExpected p

proc indentError*(p: var Parser) =
  error p, errInvalidIndentation

proc getTok*(p: var Parser) =
  if p.tok.kind != tkEof:
    p.prevKind = p.tok.kind
    p.prevEndLine = p.lex.lineNumber
    p.prevEndCol = p.lex.pos - p.lex.lineStart
    next p.lex, p.tok

proc expect*(p: var Parser; k: TokKind) =
  if p.tok.kind == k: getTok p
  else: expectedTok p, $k

proc expect*(p: var Parser; k: TokKind; s: string) =
  ## The spelling matters for the operators the grammar names literally
  ## (`'->'` in `paramListArrow`).
  if p.tok.kind == k and p.tok.s == s: getTok p
  else: expectedTok p, s

# --------------------------------------------------------------- indentation

proc emitLeaf*(p: var Parser)

proc expectLeaf*(p: var Parser; k: TokKind) =
  ## `@'not'` in the grammar: the terminal is content, not punctuation.
  if p.tok.kind == k: emitLeaf p
  else: expectedTok p, $k

proc expectLeaf*(p: var Parser; k: TokKind; s: string) =
  if p.tok.kind == k and p.tok.s == s: emitLeaf p
  else: expectedTok p, s

proc indClass*(p: Parser): IndClass =
  if p.tok.indent < 0 or p.inPragma > 0: icNoInd
  elif p.tok.indent < p.currInd: icLt
  elif p.tok.indent == p.currInd: icEq
  else: icGt

proc checkInd*(p: var Parser; allowed: set[IndClass]) =
  if indClass(p) notin allowed:
    error p, errInvalidIndentation

proc afterOperator*(p: var Parser) =
  ## `simpleExprAux` after it consumed a binary operator: `flexComment` and
  ## `optPar`.
  if p.tok.kind == tkComment and indClass(p) in {icNoInd, icGt}: getTok p
  if indClass(p) == icLt: indentError p

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
  ## `nimPreviewDotLikeOps`: `a.?b` would parse as a field access rather than
  ## as an infix operator. nifler does not define it, so a dot-like operator
  ## is an ordinary infix operator -- `a.?b.c` is `(infix .? a (dot b c))` --
  ## and the answer is `false`.
  false

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

# --------------------------------------------------------------- the tree
#
# The rule builds a chain of siblings; `p.first` is its head and `p.last` its
# tail. A `Mark` remembers the sibling that was last when the rule began, so
# "everything since the mark" is the chain hanging off `m.prev.next` -- or off
# `p.first` when the rule began with nothing written. Every rewrite below is
# some re-linking of that chain: nothing is moved, copied or re-read.

proc mark*(p: Parser): Mark {.inline.} =
  Mark(prev: p.last, info: p.info, sigs: p.sigs)

proc discardUnused*(m: Mark) {.inline.} = discard
  ## A mark the rule turned out not to need. The generator emits one per
  ## alternative because it cannot know, before it has seen the whole
  ## alternative, whether a tag will claim it.

proc since(p: Parser; m: Mark): Node {.inline.} =
  ## The first node written since `m`, `nil` if none was.
  if m.prev == nil: p.first else: m.prev.next

proc setSince(p: var Parser; m: Mark; n: Node) {.inline.} =
  if m.prev == nil: p.first = n else: m.prev.next = n

proc append*(p: var Parser; n: Node) =
  ## Links `n` as the last sibling. A cell carries its own `next`, so it lives
  ## in one chain only: appending unhooks it from wherever it was.
  n.next = nil
  if p.last == nil: p.first = n else: p.last.next = n
  p.last = n

proc cutTo(p: var Parser; m: Mark) =
  ## Drops everything written since `m`. The cells stay in the arena and are
  ## never looked at again; there is nothing to free and nothing to move.
  setSince(p, m, nil)
  p.last = m.prev

proc newNode(p: var Parser; payload: NifToken; info: NifLineInfo): Node {.inline.} =
  alloc(p.arena, payload, info)

proc tagId*(k: NiflerKind): TagId {.inline.} =
  ## The master tag pool is seeded in `TagEnum` order, so a tag's id is its
  ## ordinal and no string is ever hashed to find it.
  TagId(ord(k))

proc identNode(p: var Parser; s: string; info: NifLineInfo): Node =
  newNode(p, identToken(p.pool.strings.getOrIncl(s)), info)

proc emitIdent(p: var Parser; s: string; info: NifLineInfo) =
  p.append identNode(p, s, info)

proc emitStr(p: var Parser; s: string; info: NifLineInfo) =
  p.append newNode(p, strLitToken(p.pool.strings.getOrIncl(s)), info)

proc wrapAt*(p: var Parser; m: Mark; tag: NiflerKind; info: NifLineInfo) =
  ## Makes everything since `m` the children of a new `(tag ...)` node. The
  ## chain is not touched: it becomes the new node's `down`, and the node takes
  ## its place in the enclosing chain. Constant time, whatever it holds.
  # Tags nifler invents for the dialect are not nodes of Nim's AST and carry
  # no position: bridge.nim writes them with `addTree` and nothing else.
  let pos = if tag == RangesL or tag == UnpackflatL or tag == UnpacktupL: NoLineInfo
            else: info
  if tag == KvL: inc p.sigs.kvs
  let node = newNode(p, tagLitToken(tagId(tag)), pos)
  node.down = since(p, m)
  setSince(p, m, node)
  p.last = node

proc wrap*(p: var Parser; m: Mark; tag: NiflerKind) =
  ## Retroactively make everything from `m` a `(tag ...)` node.
  wrapAt p, m, tag, m.info

proc openNode(p: Parser): Mark {.inline.} =
  ## The other spelling of `wrapAt`, for a layout that *builds* a node rather
  ## than claiming one the rule wrote: everything appended between this and
  ## the matching `closeNode` becomes the node's children.
  Mark(prev: p.last, info: NoLineInfo, sigs: p.sigs)

proc closeNode(p: var Parser; m: Mark; tag: NiflerKind; info: NifLineInfo) {.inline.} =
  wrapAt p, m, tag, info

proc insertLeafAt*(p: var Parser; m: Mark; text: string)

proc insertTokAt*(p: var Parser; m: Mark; k: TokKind) =
  ## `^tag[ @'not' ... ]`: consume the operator and insert it at the anchor,
  ## in front of the operand that is already there.
  if p.tok.kind == k:
    insertLeafAt p, m, (if p.tok.s.len > 0: p.tok.s else: $p.tok.kind)
    getTok p
  else:
    expectedTok p, $k

proc insertLeafAt*(p: var Parser; m: Mark; text: string) =
  ## `binary(...)`'s operator: `a + b` is `(infix + a b)`, so the operator has
  ## to land *before* the left operand, which was parsed before it was read.
  ## Two links, where a flat buffer had to move the whole operand out of the way.
  let node = identNode(p, text, p.info)
  node.next = since(p, m)
  setSince(p, m, node)
  if node.next == nil: p.last = node

# --------------------------------------------------------------- layouts
#
# nifler's `nim-parsed` dialect is not the shape of the source: every absent
# child is a `.`, a declaration with several names is one node per name, and
# `for`/tuple unpacking put the iterated value in front of the variables. The
# grammar parses in source order and writes a `.` wherever a child is absent
# (`X?.`); these procs then rearrange what the rule just wrote. They all work
# the same way: `takeTail` unhooks the trees written since the mark and hands
# them over as a list, and the layout links them back in nifler's order.

proc emitEmpty*(p: var Parser) =
  ## `.` in the grammar: an absent child.
  p.append newNode(p, dotToken(), NoLineInfo)

proc setExportMarker*(p: var Parser; m: Mark) =
  ## `exportMarker`: nifler writes `x` in the export slot, whatever the
  ## operator was.
  cutTo p, m
  # The operator's position stays on the marker: a declaration node is
  # positioned at its `nkPostfix`, which parser.nim creates there. The writer
  # does not write it -- bridge.nim emits the marker with `addRaw " x"`.
  emitIdent p, "x", m.info

proc pushSection*(p: var Parser; tag: NiflerKind) =
  p.sections.add tag
  p.lastSection = tag

proc pushLastSection*(p: var Parser) =
  ## `using`: nifler's section is one variable that nothing restores, and
  ## `nkUsingStmt` does not set it -- a `using` gets the tag of whatever
  ## section was opened last in the module. Nothing downstream reads that tag,
  ## but a byte-identical tree has to have it.
  p.sections.add p.lastSection
proc popSection*(p: var Parser) = p.sections.setLen p.sections.len - 1
proc section(p: Parser): NiflerKind =
  if p.sections.len > 0: p.sections[^1] else: VarL

proc pushFieldWrap*(p: var Parser; wrap: bool) = p.wrapFields.add wrap
proc popFieldWrap*(p: var Parser) = p.wrapFields.setLen p.wrapFields.len - 1

proc takeTail(p: var Parser; m: Mark): seq[Node] =
  ## The trees written since `m`, unhooked from the chain and listed so a
  ## layout can index them. The cells are the ones the rule built: no copy is
  ## made and none is needed, because a layout links each of them back exactly
  ## once. `fanOut` and `fanOutKv` are the exceptions and say so with
  ## `copyTree`.
  let head = since(p, m)
  cutTo p, m
  result = newSeqOfCap[Node](16)
  var n = head
  while n != nil:
    result.add n
    n = n.next

proc nameNodeInfo(name, exp, pragmas: Node): NifLineInfo =
  ## The position of parser.nim's name node, which is what a declaration is
  ## positioned at: `nkPragmaExpr` (created at the `{.`) if there is a pragma,
  ## else `nkPostfix` (created at the export operator), else the name.
  if not pragmas.isEmpty: pragmas.info
  elif not exp.isEmpty: exp.info
  else: name.info

proc wrapLikeFirst*(p: var Parser; m: Mark; tag: NiflerKind) =
  ## A node positioned at its first child: `newTree(nkCommand, a.info, a)`.
  let f = since(p, m)
  wrapAt p, m, tag, (if f == nil: m.info else: f.info)

proc fanOut*(p: var Parser; m: Mark) =
  ## `name x pragmas` repeated, then `type value`: one `(section name x
  ## pragmas type value)` per name. The count is exact because every slot is
  ## written, `.` or not -- which is what the placeholders buy.
  let kids = takeTail(p, m)
  if p.failed: return
  let names = (kids.len - 2) div 3
  let tag = p.section
  let wrapped = tag == FldL and names > 1 and
                p.wrapFields.len > 0 and p.wrapFields[^1]
  let outer = openNode(p)
  for i in 0 ..< names:
    let one = openNode(p)
    for j in 0 .. 2: p.append kids[3*i + j]
    # The type and the value belong to every name, and a cell can sit in one
    # chain only, so all but the last name get a copy of them.
    let last = i == names - 1
    p.append (if last: kids[^2] else: copyTree(p.arena, kids[^2]))
    p.append (if last: kids[^1] else: copyTree(p.arena, kids[^1]))
    closeNode p, one, tag, nameNodeInfo(kids[3*i], kids[3*i+1], kids[3*i+2])
  if wrapped: closeNode p, outer, StmtsL, NoLineInfo  # not a node of Nim's AST
  else: discardUnused outer

proc fanOutKv*(p: var Parser; m: Mark) =
  ## A tuple field list: names, then `type value`. nifler keeps `(kv name
  ## type)` and drops the default.
  let kids = takeTail(p, m)
  if p.failed: return
  if kids.len > 2: p.sigs.kvs = p.sigs.kvs + kids.len - 2
  for i in 0 ..< kids.len - 2:
    let one = openNode(p)
    let info = kids[i].info
    p.append kids[i]
    let last = i == kids.len - 3
    p.append (if last: kids[^2] else: copyTree(p.arena, kids[^2]))
    closeNode p, one, KvL, info

proc joinIdents*(p: var Parser; m: Mark) =
  ## Inside backquotes `parseSymbol` glues a run of operator and bracket
  ## tokens into one identifier: `` `[]=` `` is `(quoted []=)`, not three
  ## children, while `` `=copy` `` is `(quoted = copy)`.
  let kids = takeTail(p, m)
  if p.failed: return
  var text = ""
  for k in kids: text.add p.pool.strings[strId(k)]
  emitIdent p, text, m.info

proc inSemiStmtList*(p: Parser): bool {.inline.} =
  ## `parseStmt`'s `if p.inSemiStmtList > 0: result = simpleStmt(p)`: inside
  ## a parenthesised statement list a one-line body is the bare statement,
  ## not a `stmts` -- `(if a: b else: c)` is `(elif a b)`. The counter is not
  ## reset by what nests inside, so neither is this.
  p.inSemiStmtList > 0

proc stmtListExprLayout*(p: var Parser; m: Mark) =
  ## `nkStmtListExpr`: nifler writes all statements but the last in a `stmts`
  ## and the last one after it, `(expr (stmts a b) c)`.
  let kids = takeTail(p, m)
  if p.failed: return
  let outer = openNode(p)
  let inner = openNode(p)
  for i in 0 ..< kids.len - 1: p.append kids[i]
  closeNode p, inner, StmtsL, m.info
  if kids.len > 0: p.append kids[^1]
  else: emitEmpty p
  closeNode p, outer, ExprL, m.info

proc addParams(p: var Parser; c: Node; info: NifLineInfo)

proc rhsMode*(mode: PrimaryMode): PrimaryMode {.inline.} =
  ## The mode of an operator's right operand. `simpleExprAux` turns
  ## `pmTrySimple` into `pmNormal` after the first primary, and
  ## `parseOperators` turns `pmTypeDef` into `pmTypeDesc` -- so
  ## `x == Handle 0` is `(infix == x (cmd Handle 0))`.
  case mode
  of pmTrySimple: pmNormal
  of pmTypeDef: pmTypeDesc
  else: mode

proc literalAsIdent*(p: var Parser; m: Mark) =
  ## Inside backquotes `parseSymbol` makes a literal token an identifier of
  ## its text: `` `'big` `` is `(quoted ' big)`.
  let kids = takeTail(p, m)
  if p.failed: return
  let k = kids[0]
  case kind(k)
  of CharLit:
    var t = ""
    t.add charVal(k)
    emitIdent p, t, m.info
  of IntLit: emitIdent p, $intVal(k), m.info
  of UIntLit: emitIdent p, $uintVal(k), m.info
  of FloatLit: emitIdent p, $floatVal(k), m.info
  of StrLit: emitIdent p, p.pool.strings[strId(k)], m.info
  else: p.append k

proc posMarker*(p: var Parser) =
  ## A `.` that only carries the current position to a layout, which removes
  ## it again.
  inc p.sigs.posMarkers
  p.append newNode(p, dotToken(), p.info)

proc dotLayout*(p: var Parser; m: Mark) =
  ## `dotExpr`'s rewrite of `x.y[:z](args)` into `y[z](x, args)`: the dot
  ## node's children are `x y (at z...) args...` when the `[:` was there.
  ## The rule wrapped one `(dot ...)` tree, so with no `[:` there is nothing
  ## to move and the tree stands as written.
  if p.sigs.posMarkers == m.sigs.posMarkers: return
  let kids = takeTail(p, m)
  if p.failed: return
  let node = kids[0]
  var parts: seq[Node] = newSeqOfCap[Node](8)
  var c = node.down
  while c != nil:
    parts.add c
    c = c.next
  if parts.len <= 2:
    p.append node
    return
  # `dotExpr` builds the call with `p.parLineInfo` right after the `]` --
  # the position `posMarker` recorded, as `parts[3]`
  let call = openNode(p)
  let at = openNode(p)
  p.append parts[1]
  var z = parts[2].down
  while z != nil:
    let nxt = z.next
    p.append z
    z = nxt
  closeNode p, at, AtL, parts[2].info
  p.append parts[0]
  for i in 4 ..< parts.len: p.append parts[i]
  closeNode p, call, CallL, parts[3].info

proc curlyOrTable*(p: var Parser; m: Mark) =
  ## `setOrTableConstr` retags `nkCurly` as `nkTableConstr` as soon as one
  ## element is `key: value`.
  ## No `kv` was built inside: not a table, so the `(curly ...)` stands.
  if p.sigs.kvs == m.sigs.kvs: return
  let kids = takeTail(p, m)
  if p.failed: return
  let node = kids[0]
  var isTable = false
  var c = node.down
  while c != nil:
    if kind(c) == TagLit and tag(c) == tagId(KvL): isTable = true
    c = c.next
  # Only the tag changes; the children stay exactly where they are.
  if isTable: node.payload = tagLitToken(tagId(TabconstrL))
  p.append node

proc callOrObjConstr*(p: var Parser; m: Mark) =
  ## `primarySuffix`'s `(`: a call whose first argument is `name: value` is
  ## an object constructor, `Foo(a: 1)` is `(oconstr Foo (kv a 1))`.
  ## No `kv` was built inside: not an object constructor, so the `(call ...)`
  ## stands. A `kv` nested deeper than the first argument only costs the test
  ## below, which then finds none in that slot.
  if p.sigs.kvs == m.sigs.kvs: return
  let kids = takeTail(p, m)
  if p.failed: return
  let node = kids[0]
  let callee = node.down
  if callee != nil and callee.next != nil and kind(callee.next) == TagLit and
      tag(callee.next) == tagId(KvL):
    node.payload = tagLitToken(tagId(OconstrL))
  p.append node

proc attachBlocks*(p: var Parser; m: Mark) =
  ## `postExprBlocks`: a trailing `:` or `do` block belongs *to* the
  ## expression in front of it. `makeCall` keeps a node that is already a call
  ## -- `foo x:` stays a `cmd` -- and wraps anything else, and every block is
  ## appended as a child: `c.into:` is `(call (dot c into) (stmts ...))`.
  ## The rule that calls this has parsed the operand first, so the operand is
  ## the first tree since `m` and the blocks are the rest.
  let kids = takeTail(p, m)
  if p.failed: return
  if kids.len == 1:
    p.append kids[0]
    return
  let op = kids[0]
  var isCall = false
  if kind(op) == TagLit:
    let t = tag(op)
    isCall = t == tagId(CallL) or t == tagId(CmdL) or t == tagId(InfixL) or
             t == tagId(PrefixL) or t == tagId(CallstrlitL)
  if isCall:
    # The operand already IS the call, so the blocks just join its children:
    # `kids[1..]` are still chained to each other, so one link does it.
    var tail = op.down
    if tail == nil:
      op.down = kids[1]
    else:
      while tail.next != nil: tail = tail.next
      tail.next = kids[1]
    p.append op
  else:
    let call = openNode(p)
    let info = op.info
    p.append op
    for i in 1 ..< kids.len: p.append kids[i]
    closeNode p, call, CallL, info

proc doLayout*(p: var Parser; m: Mark; atBody: bool) =
  ## A `do` block, parsed as `params ret pragmas body`. Without a signature or
  ## pragmas it is just its body; otherwise `(do params ret body)` -- nifler
  ## writes the formal parameters as `(params ...)` plus the return type, and
  ## drops the pragmas.
  # kids: position after `do`, params, ret, pragmas, body, position after
  # the body (both from `posMarker`)
  let kids = takeTail(p, m)
  if p.failed: return
  if kids[1].isEmpty and kids[2].isEmpty and kids[3].isEmpty:
    p.append kids[4]
  else:
    # `postExprBlocks` builds the first `do` with `stmtList.info`, the loop
    # over further blocks with the position of the `do` keyword. The formal
    # parameters are created where their list starts -- unless there is no
    # list at all, only pragmas, in which case an empty one is made up after
    # the body has been parsed.
    let info = if atBody: kids[4].info else: m.info
    let paramsInfo = if kids[1].isEmpty and kids[2].isEmpty: kids[5].info
                     else: kids[0].info
    let d = openNode(p)
    addParams p, kids[1], paramsInfo
    p.append kids[2]
    p.append kids[4]
    closeNode p, d, DoL, info

proc routineBodyAllowed*(p: Parser; mode: PrimaryMode): bool {.inline.} =
  ## `parseProcExpr(p, mode != pmTypeDesc, ...)`: in a type `proc (): int = x`
  ## has no body -- the `= x` is the declaration's default value.
  mode != pmTypeDesc

proc emptyDiscriminator*(p: var Parser) =
  ## `parseObjectCase` without a discriminator: an `nkIdentDefs` of empty
  ## nodes. Its name is the empty node, whose position has no file, so
  ## bridge.nim writes it absolute -- as `~1,,???`.
  let unknown = NifLineInfo(file: p.pool.filenames.getOrIncl("???"), line: 0, col: -1)
  let f = openNode(p)
  for i in 0 .. 4: emitEmpty p
  closeNode p, f, FldL, unknown

proc wrapNoInfo*(p: var Parser; m: Mark; tag: NiflerKind) =
  ## A node bridge.nim writes with `addTree` alone.
  wrapAt p, m, tag, NoLineInfo

proc pragmaBlock*(p: var Parser; m: Mark) =
  ## `parseStmtPragma`: a pragma followed by a block is an `nkPragmaBlock`,
  ## `(pragmax pragmas body)`, positioned at the pragma.
  let kids = takeTail(p, m)
  if p.failed: return
  if kids.len == 1:
    p.append kids[0]
  else:
    let info = kids[0].info
    let x = openNode(p)
    for k in kids: p.append k
    closeNode p, x, PragmaxL, info

proc inheritLayout*(p: var Parser; m: Mark) =
  ## `nkOfInherit`, created at `of`: one type is written as itself, several
  ## as `(par A B)`.
  let kids = takeTail(p, m)
  if p.failed: return
  if kids.len == 1:
    p.append kids[0]
  else:
    let x = openNode(p)
    for k in kids: p.append k
    closeNode p, x, ParL, m.info

proc wrapSection*(p: var Parser; m: Mark) =
  ## A declaration that is already in slot form, tagged with the section: the
  ## `let`s of a tuple unpacking, which bridge.nim writes without a position.
  wrapAt p, m, p.section, NoLineInfo

proc pushInfo*(p: var Parser) =
  ## The position of the current token, for a node built after its keyword
  ## is consumed -- `parseRoutine` creates the routine node at `proc`.
  p.infos.add p.info
proc popInfo*(p: var Parser) = p.infos.setLen p.infos.len - 1

proc moveLastToMark*(p: var Parser; m: Mark) =
  ## `for x in it` and `let (a, b) = v`: nifler writes the iterated or
  ## unpacked value first.
  let kids = takeTail(p, m)
  if p.failed: return
  p.append kids[^1]
  for i in 0 ..< kids.len - 1: p.append kids[i]

proc addParams(p: var Parser; c: Node; info: NifLineInfo) =
  ## `parseParamList` always builds an `nkFormalParams`, created at the token
  ## where the list would start.
  if c.isEmpty:
    let x = openNode(p)
    closeNode p, x, ParamsL, info
  else:
    p.append c

proc procLayout*(p: var Parser; m: Mark; keyword: NiflerKind) =
  ## An anonymous routine, parsed as `params ret pragmas body` with `.` for
  ## each one that is absent. With a body it is a lambda,
  ## `(proc . . . . params ret pragmas . body)`, and `(params)` is always there.
  ## Without one it is a type, and a type keeps what the source had: nothing
  ## at all is `(proctype)`, no signature is a single `.` in place of params
  ## and result.
  let kids = takeTail(p, m)
  if p.failed: return
  let params = kids[0]
  let ret = kids[1]
  let pragmas = kids[2]
  let body = kids[3]
  if not body.isEmpty:
    let x = openNode(p)
    for i in 0 .. 3: emitEmpty p
    addParams p, params, m.info
    p.append ret
    p.append pragmas
    emitEmpty p
    p.append body
    closeNode p, x, keyword, m.info
  else:
    let t = if keyword == IteratorL: ItertypeL else: ProctypeL
    let x = openNode(p)
    let hasSig = not params.isEmpty or not ret.isEmpty
    if hasSig or not pragmas.isEmpty:
      for i in 0 .. 3: emitEmpty p
      if hasSig:
        addParams p, params, m.info
        p.append ret
      else:
        emitEmpty p
      p.append pragmas
      emitEmpty p
      emitEmpty p
    closeNode p, x, t, m.info

proc routineLayout*(p: var Parser; m: Mark; keyword: NiflerKind) =
  ## A named routine, parsed as `name x pattern typevars params ret pragmas
  ## body`: `(keyword name x pattern typevars params ret pragmas . body)`,
  ## with the effects slot nifler reserves and `(params)` always present.
  let kids = takeTail(p, m)
  if p.failed: return
  let info = p.infos[^1]
  let x = openNode(p)
  for i in 0 .. 3: p.append kids[i]
  addParams p, kids[4], NoLineInfo
  p.append kids[5]
  p.append kids[6]
  emitEmpty p
  p.append kids[7]
  closeNode p, x, keyword, info
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

proc emitInt(p: var Parser; v: int64; info: NifLineInfo) =
  p.append allocWide(p.arena, IntLit, cast[uint64](v), info)
proc emitUInt(p: var Parser; v: uint64; info: NifLineInfo) =
  p.append allocWide(p.arena, UIntLit, v, info)
proc emitFloat(p: var Parser; v: float64; info: NifLineInfo) =
  p.append allocWide(p.arena, FloatLit, cast[uint64](v), info)

proc emitLeaf*(p: var Parser) =
  ## The terminal the grammar matched, as a NIF atom. Which atom is decided by
  ## the token kind alone, so the generator never has to say (it emits the
  ## terminal's name as a comment only).
  let info = p.info
  case p.tok.kind
  # Only the unsuffixed kinds are bare atoms in nifler; every sized kind,
  # the 64-bit ones included, is `(suf value "i64")`. `tkInt64Lit` is also
  # what a plain literal becomes once it leaves int32's range, exactly as in
  # compiler/lexer.nim, so a large plain literal is written with a suffix too.
  of tkIntLit:
    emitInt p, p.tok.iNumber, info
  of tkInt8Lit, tkInt16Lit, tkInt32Lit, tkInt64Lit:
    let x = openNode(p)
    emitInt p, p.tok.iNumber, info
    emitStr p, (case p.tok.kind
                of tkInt8Lit: "i8"
                of tkInt16Lit: "i16"
                of tkInt32Lit: "i32"
                else: "i64"), info
    closeNode p, x, SufL, info
  of tkUIntLit:
    emitUInt p, cast[uint64](p.tok.iNumber), info
  of tkUInt8Lit, tkUInt16Lit, tkUInt32Lit, tkUInt64Lit:
    let x = openNode(p)
    emitUInt p, cast[uint64](p.tok.iNumber), info
    emitStr p, (case p.tok.kind
                of tkUInt8Lit: "u8"
                of tkUInt16Lit: "u16"
                of tkUInt32Lit: "u32"
                else: "u64"), info
    closeNode p, x, SufL, info
  of tkFloatLit:
    emitFloat p, floatValue(p.tok), info
  of tkFloat32Lit, tkFloat64Lit, tkFloat128Lit:
    let x = openNode(p)
    emitFloat p, floatValue(p.tok), info
    emitStr p, (case p.tok.kind
                of tkFloat32Lit: "f32"
                of tkFloat64Lit: "f64"
                else: "f128"), info
    closeNode p, x, SufL, info
  of tkStrLit:
    emitStr p, p.tok.s, info
  of tkRStrLit, tkTripleStrLit, tkGStrLit, tkGTripleStrLit:
    # a generalized string literal is raw: `parseGStrLit` makes the argument
    # an `nkRStrLit` (or `nkTripleStrLit`)
    let x = openNode(p)
    emitStr p, p.tok.s, info
    emitStr p, (if p.tok.kind == tkRStrLit or p.tok.kind == tkGStrLit: "R" else: "T"), info
    closeNode p, x, SufL, info
  of tkCharLit:
    p.append newNode(p, charToken(if p.tok.s.len > 0: p.tok.s[0] else: '\0'), info)
  of tkCustomLit:
    # `identOrLiteral` turns `-1'big` into a call of the suffix operator:
    # `nkDotExpr(nkRStrLit("-1"), ident("'big"))`, the apostrophe included.
    let outer = openNode(p)
    let inner = openNode(p)
    emitStr p, p.tok.s.substr(0, int(p.tok.suffixPos) - 1), info
    emitStr p, "R", info
    closeNode p, inner, SufL, info
    emitIdent p, p.tok.s.substr(int(p.tok.suffixPos)), info
    closeNode p, outer, DotL, info
  of tkComment:
    # nifler attaches a comment to its node and writes none of it (unless
    # `--docs`); a `commentStmt` is just `(comment)`. Emitting the text as a
    # string would take a slot that belongs to something else.
    discard
  else:
    # Identifiers, operators and every keyword used as a name. `tkSymbol` is
    # the common case; the rest reach here through `symbolOrKeyword`, `OPR`
    # and the `quoted[...]` production.
    emitIdent p, (if p.tok.s.len > 0: p.tok.s else: $p.tok.kind), info
  getTok p
