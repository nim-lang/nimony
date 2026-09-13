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
    inSemiStmtList*: int   ## `( stmt; stmt )` nesting, as in parser.nim
    sections: seq[string]  ## the tag a declaration fans out into: var/let/param/...
    lastSection: string    ## the section opened most recently, popped or not
    tail: TokenBuf         ## scratch for the layout rewrites
    wrapFields: seq[bool]  ## whether a multi-name field is wrapped in `stmts`

proc openParser*(src, filename: string): Parser =
  result = Parser(lex: openLexer(src, filename),
                  tok: Token(kind: tkInvalid, s: "", indent: -1, spacing: {},
                             line: 0, col: 0, base: 10, suffixPos: -1,
                             iNumber: 0),
                  dest: createTokenBuf(src.len div 3 + 16),
                  head: createTokenBuf(4),
                  tail: createTokenBuf(64),
                  file: pool.filenames.getOrIncl(filename),
                  currInd: 0, indStack: @[], errors: @[],
                  inPragma: 0, sections: @[], lastSection: "var", wrapFields: @[])
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

# --------------------------------------------------------------- layouts
#
# nifler's `nim-parsed` dialect is not the shape of the source: every absent
# child is a `.`, a declaration with several names is one node per name, and
# `for`/tuple unpacking put the iterated value in front of the variables. The
# grammar parses in source order and writes a `.` wherever a child is absent
# (`X?.`); these procs then rearrange what the rule just wrote. They all work
# the same way: the trees written since the mark are complete (marks nest), so
# they are copied to a scratch buffer, cut from `dest`, and written back in
# nifler's order.

proc emitEmpty*(p: var Parser) =
  ## `.` in the grammar: an absent child.
  addDotToken(p.dest, NoLineInfo)

proc setExportMarker*(p: var Parser; m: Mark) =
  ## `exportMarker`: nifler writes `x` in the export slot, whatever the
  ## operator was.
  p.dest.shrink m.pos
  addIdent(p.dest, "x", m.info)

proc pushSection*(p: var Parser; tag: string) =
  p.sections.add tag
  p.lastSection = tag

proc pushLastSection*(p: var Parser) =
  ## `using`: nifler's section is one variable that nothing restores, and
  ## `nkUsingStmt` does not set it -- a `using` gets the tag of whatever
  ## section was opened last in the module. Nothing downstream reads that tag,
  ## but a byte-identical tree has to have it.
  p.sections.add p.lastSection
proc popSection*(p: var Parser) = p.sections.setLen p.sections.len - 1
proc section(p: Parser): string =
  if p.sections.len > 0: p.sections[^1] else: "var"

proc pushFieldWrap*(p: var Parser; wrap: bool) = p.wrapFields.add wrap
proc popFieldWrap*(p: var Parser) = p.wrapFields.setLen p.wrapFields.len - 1

proc takeTail(p: var Parser; m: Mark): seq[Cursor] =
  ## The trees written since `m`, as cursors into `p.tail`; `dest` is cut back
  ## to the mark.
  p.tail.shrink 0
  addParLe(p.tail, registerTag("tail"))
  for i in m.pos ..< p.dest.len: p.tail.add p.dest[i]
  addParRi p.tail
  p.dest.shrink m.pos
  result = @[]
  var c = beginRead(p.tail)
  var k = childCursor(c)
  while k.hasMore:
    result.add k
    skip k

proc isEmpty(c: Cursor): bool {.inline.} = c.kind == DotToken

proc fanOut*(p: var Parser; m: Mark) =
  ## `name x pragmas` repeated, then `type value`: one `(section name x
  ## pragmas type value)` per name. The count is exact because every slot is
  ## written, `.` or not -- which is what the placeholders buy.
  let kids = takeTail(p, m)
  let names = (kids.len - 2) div 3
  let tag = registerTag(p.section)
  let wrap = p.section == "fld" and names > 1 and
             p.wrapFields.len > 0 and p.wrapFields[^1]
  if wrap: addParLe(p.dest, registerTag("stmts"), m.info)
  for i in 0 ..< names:
    addParLe(p.dest, tag, kids[3*i].info)
    for j in 0 .. 2: p.dest.addSubtree kids[3*i + j]
    p.dest.addSubtree kids[^2]
    p.dest.addSubtree kids[^1]
    addParRi p.dest
  if wrap: addParRi p.dest

proc fanOutKv*(p: var Parser; m: Mark) =
  ## A tuple field list: names, then `type value`. nifler keeps `(kv name
  ## type)` and drops the default.
  let kids = takeTail(p, m)
  let tag = registerTag("kv")
  for i in 0 ..< kids.len - 2:
    addParLe(p.dest, tag, kids[i].info)
    p.dest.addSubtree kids[i]
    p.dest.addSubtree kids[^2]
    addParRi p.dest

proc joinIdents*(p: var Parser; m: Mark) =
  ## Inside backquotes `parseSymbol` glues a run of operator and bracket
  ## tokens into one identifier: `` `[]=` `` is `(quoted []=)`, not three
  ## children, while `` `=copy` `` is `(quoted = copy)`.
  let kids = takeTail(p, m)
  var text = ""
  for k in kids: text.add strVal(k)
  addIdent(p.dest, text, m.info)

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
  addParLe(p.dest, registerTag("expr"), m.info)
  addParLe(p.dest, registerTag("stmts"), m.info)
  for i in 0 ..< kids.len - 1: p.dest.addSubtree kids[i]
  addParRi p.dest
  if kids.len > 0: p.dest.addSubtree kids[^1]
  else: emitEmpty p
  addParRi p.dest

proc routineBodyAllowed*(p: Parser; mode: PrimaryMode): bool {.inline.} =
  ## `parseProcExpr(p, mode != pmTypeDesc, ...)`: in a type `proc (): int = x`
  ## has no body -- the `= x` is the declaration's default value.
  mode != pmTypeDesc

proc wrapSection*(p: var Parser; m: Mark) =
  ## A declaration that is already in slot form, tagged with the section.
  wrap p, m, p.section

proc moveLastToMark*(p: var Parser; m: Mark) =
  ## `for x in it` and `let (a, b) = v`: nifler writes the iterated or
  ## unpacked value first.
  let kids = takeTail(p, m)
  p.dest.addSubtree kids[^1]
  for i in 0 ..< kids.len - 1: p.dest.addSubtree kids[i]

proc addParams(p: var Parser; c: Cursor) =
  if c.isEmpty:
    addParLe(p.dest, registerTag("params"), NoLineInfo)
    addParRi p.dest
  else:
    p.dest.addSubtree c

proc procLayout*(p: var Parser; m: Mark; keyword: string) =
  ## An anonymous routine, parsed as `params ret pragmas body` with `.` for
  ## each one that is absent. With a body it is a lambda,
  ## `(proc . . . . params ret pragmas . body)`, and `(params)` is always there.
  ## Without one it is a type, and a type keeps what the source had: nothing
  ## at all is `(proctype)`, no signature is a single `.` in place of params
  ## and result.
  let kids = takeTail(p, m)
  let (params, ret, pragmas, body) = (kids[0], kids[1], kids[2], kids[3])
  if not body.isEmpty:
    addParLe(p.dest, registerTag(keyword), m.info)
    for i in 0 .. 3: emitEmpty p
    addParams p, params
    p.dest.addSubtree ret
    p.dest.addSubtree pragmas
    emitEmpty p
    p.dest.addSubtree body
    addParRi p.dest
  else:
    addParLe(p.dest, registerTag(if keyword == "iterator": "itertype"
                                 else: "proctype"), m.info)
    let hasSig = not params.isEmpty or not ret.isEmpty
    if hasSig or not pragmas.isEmpty:
      for i in 0 .. 3: emitEmpty p
      if hasSig:
        addParams p, params
        p.dest.addSubtree ret
      else:
        emitEmpty p
      p.dest.addSubtree pragmas
      emitEmpty p
      emitEmpty p
    addParRi p.dest

proc routineLayout*(p: var Parser; m: Mark; keyword: string) =
  ## A named routine, parsed as `name x pattern typevars params ret pragmas
  ## body`: `(keyword name x pattern typevars params ret pragmas . body)`,
  ## with the effects slot nifler reserves and `(params)` always present.
  let kids = takeTail(p, m)
  addParLe(p.dest, registerTag(keyword), m.info)
  for i in 0 .. 3: p.dest.addSubtree kids[i]
  addParams p, kids[4]
  p.dest.addSubtree kids[5]
  p.dest.addSubtree kids[6]
  emitEmpty p
  p.dest.addSubtree kids[7]
  addParRi p.dest

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
    # nifler attaches a comment to its node and writes none of it (unless
    # `--docs`); a `commentStmt` is just `(comment)`. Emitting the text as a
    # string would take a slot that belongs to something else.
    discard
  else:
    # Identifiers, operators and every keyword used as a name. `tkSymbol` is
    # the common case; the rest reach here through `symbolOrKeyword`, `OPR`
    # and the `quoted[...]` production.
    addIdent(p.dest, (if p.tok.s.len > 0: p.tok.s else: $p.tok.kind), info)
  getTok p
