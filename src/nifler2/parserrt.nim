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
    infos: seq[NifLineInfo] ## see `pushInfo`
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

proc wrapAt*(p: var Parser; m: Mark; tag: string; info: NifLineInfo) =
  ## Retroactively make everything from `m` a `(tag ...)` node positioned at
  ## `info`.
  p.head.shrink 0
  # Tags nifler invents for the dialect are not nodes of Nim's AST and carry
  # no position: bridge.nim writes them with `addTree` and nothing else.
  let pos = if tag in ["ranges", "unpackflat", "unpacktup"]: NoLineInfo else: info
  addParLe(p.head, registerTag(tag), pos)
  splice p, m.pos
  # The node ends at the end of the buffer -- which is precisely the situation
  # `reopenLastTree` exists for, so nifcore computes the jump, overflow and all.
  reopenLastTree(p.dest, m.pos)
  addParRi p.dest

proc wrap*(p: var Parser; m: Mark; tag: string) =
  ## Retroactively make everything from `m` a `(tag ...)` node.
  wrapAt p, m, tag, m.info

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
  # The operator's position stays on the marker: a declaration node is
  # positioned at its `nkPostfix`, which parser.nim creates there. The writer
  # does not write it -- bridge.nim emits the marker with `addRaw " x"`.
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

proc nameNodeInfo(name, exp, pragmas: Cursor): NifLineInfo =
  ## The position of parser.nim's name node, which is what a declaration is
  ## positioned at: `nkPragmaExpr` (created at the `{.`) if there is a pragma,
  ## else `nkPostfix` (created at the export operator), else the name.
  if not pragmas.isEmpty: pragmas.info
  elif not exp.isEmpty: exp.info
  else: name.info

proc infoAt(p: var Parser; pos: int): NifLineInfo =
  var c = cursorAt(p.dest, pos)
  result = rawLineInfo(c)
  endRead c

proc wrapLikeFirst*(p: var Parser; m: Mark; tag: string) =
  ## A node positioned at its first child: `newTree(nkCommand, a.info, a)`.
  wrapAt p, m, tag, infoAt(p, m.pos)

proc fanOut*(p: var Parser; m: Mark) =
  ## `name x pragmas` repeated, then `type value`: one `(section name x
  ## pragmas type value)` per name. The count is exact because every slot is
  ## written, `.` or not -- which is what the placeholders buy.
  let kids = takeTail(p, m)
  let names = (kids.len - 2) div 3
  let tag = registerTag(p.section)
  let wrap = p.section == "fld" and names > 1 and
             p.wrapFields.len > 0 and p.wrapFields[^1]
  if wrap: addParLe(p.dest, registerTag("stmts"), NoLineInfo)  # not a node of Nim's AST
  for i in 0 ..< names:
    addParLe(p.dest, tag, nameNodeInfo(kids[3*i], kids[3*i+1], kids[3*i+2]))
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

proc addParams(p: var Parser; c: Cursor; info: NifLineInfo)

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
  let k = kids[0]
  case k.kind
  of CharLit:
    var t = ""
    t.add charLit(k)
    addIdent(p.dest, t, m.info)
  of IntLit: addIdent(p.dest, $intVal(k), m.info)
  of UIntLit: addIdent(p.dest, $uintVal(k), m.info)
  of FloatLit: addIdent(p.dest, $floatVal(k), m.info)
  of StrLit: addIdent(p.dest, strVal(k), m.info)
  else: p.dest.addSubtree k

proc posMarker*(p: var Parser) =
  ## A `.` that only carries the current position to a layout, which removes
  ## it again.
  addDotToken(p.dest, p.info)

proc dotLayout*(p: var Parser; m: Mark) =
  ## `dotExpr`'s rewrite of `x.y[:z](args)` into `y[z](x, args)`: the dot
  ## node's children are `x y (at z...) args...` when the `[:` was there.
  let kids = takeTail(p, m)
  let node = kids[0]
  var parts: seq[Cursor] = @[]
  var c = childCursor(node)
  while c.hasMore:
    parts.add c
    skip c
  if parts.len <= 2:
    p.dest.addSubtree node
    return
  # `dotExpr` builds the call with `p.parLineInfo` right after the `]` --
  # the position `posMarker` recorded, as `parts[3]`
  addParLe(p.dest, registerTag("call"), rawLineInfo(parts[3]))
  addParLe(p.dest, registerTag("at"), parts[2].info)
  p.dest.addSubtree parts[1]
  var z = childCursor(parts[2])
  while z.hasMore:
    p.dest.addSubtree z
    skip z
  addParRi p.dest
  p.dest.addSubtree parts[0]
  for i in 4 ..< parts.len: p.dest.addSubtree parts[i]
  addParRi p.dest

proc curlyOrTable*(p: var Parser; m: Mark) =
  ## `setOrTableConstr` retags `nkCurly` as `nkTableConstr` as soon as one
  ## element is `key: value`.
  let kids = takeTail(p, m)
  let node = kids[0]
  var isTable = false
  var c = childCursor(node)
  while c.hasMore:
    if c.kind == TagLit and c.resolvedTagId == registerTag("kv"): isTable = true
    skip c
  if isTable:
    addParLe(p.dest, registerTag("tabconstr"), node.info)
    var k = childCursor(node)
    while k.hasMore:
      p.dest.addSubtree k
      skip k
    addParRi p.dest
  else:
    p.dest.addSubtree node

proc callOrObjConstr*(p: var Parser; m: Mark) =
  ## `primarySuffix`'s `(`: a call whose first argument is `name: value` is
  ## an object constructor, `Foo(a: 1)` is `(oconstr Foo (kv a 1))`.
  let kids = takeTail(p, m)
  let node = kids[0]
  var c = childCursor(node)
  skip c                                 # the callee
  if c.hasMore and c.kind == TagLit and c.resolvedTagId == registerTag("kv"):
    addParLe(p.dest, registerTag("oconstr"), node.info)
    var k = childCursor(node)
    while k.hasMore:
      p.dest.addSubtree k
      skip k
    addParRi p.dest
  else:
    p.dest.addSubtree node

proc attachBlocks*(p: var Parser; m: Mark) =
  ## `postExprBlocks`: a trailing `:` or `do` block belongs *to* the
  ## expression in front of it. `makeCall` keeps a node that is already a call
  ## -- `foo x:` stays a `cmd` -- and wraps anything else, and every block is
  ## appended as a child: `c.into:` is `(call (dot c into) (stmts ...))`.
  ## The rule that calls this has parsed the operand first, so the operand is
  ## the first tree since `m` and the blocks are the rest.
  let kids = takeTail(p, m)
  if kids.len == 1:
    p.dest.addSubtree kids[0]
    return
  let op = kids[0]
  var isCall = false
  if op.kind == TagLit:
    let t = op.resolvedTagId
    for name in ["call", "cmd", "infix", "prefix", "postfix", "callstrlit"]:
      if t == registerTag(name): isCall = true
  if isCall:
    addParLe(p.dest, op.resolvedTagId, op.info)
    var c = childCursor(op)
    while c.hasMore:
      p.dest.addSubtree c
      skip c
  else:
    addParLe(p.dest, registerTag("call"), op.info)
    p.dest.addSubtree op
  for i in 1 ..< kids.len: p.dest.addSubtree kids[i]
  addParRi p.dest

proc doLayout*(p: var Parser; m: Mark; atBody: bool) =
  ## A `do` block, parsed as `params ret pragmas body`. Without a signature or
  ## pragmas it is just its body; otherwise `(do params ret body)` -- nifler
  ## writes the formal parameters as `(params ...)` plus the return type, and
  ## drops the pragmas.
  # kids: position after `do`, params, ret, pragmas, body, position after
  # the body (both from `posMarker`)
  let kids = takeTail(p, m)
  if kids[1].isEmpty and kids[2].isEmpty and kids[3].isEmpty:
    p.dest.addSubtree kids[4]
  else:
    # `postExprBlocks` builds the first `do` with `stmtList.info`, the loop
    # over further blocks with the position of the `do` keyword. The formal
    # parameters are created where their list starts -- unless there is no
    # list at all, only pragmas, in which case an empty one is made up after
    # the body has been parsed.
    addParLe(p.dest, registerTag("do"), if atBody: kids[4].info else: m.info)
    let paramsInfo = if kids[1].isEmpty and kids[2].isEmpty: rawLineInfo(kids[5])
                     else: rawLineInfo(kids[0])
    addParams p, kids[1], paramsInfo
    p.dest.addSubtree kids[2]
    p.dest.addSubtree kids[4]
    addParRi p.dest

proc routineBodyAllowed*(p: Parser; mode: PrimaryMode): bool {.inline.} =
  ## `parseProcExpr(p, mode != pmTypeDesc, ...)`: in a type `proc (): int = x`
  ## has no body -- the `= x` is the declaration's default value.
  mode != pmTypeDesc

proc emptyDiscriminator*(p: var Parser) =
  ## `parseObjectCase` without a discriminator: an `nkIdentDefs` of empty
  ## nodes. Its name is the empty node, whose position has no file, so
  ## bridge.nim writes it absolute -- as `~1,,???`.
  let unknown = NifLineInfo(file: pool.filenames.getOrIncl("???"), line: 0, col: -1)
  addParLe(p.dest, registerTag("fld"), unknown)
  for i in 0 .. 4: emitEmpty p
  addParRi p.dest

proc wrapNoInfo*(p: var Parser; m: Mark; tag: string) =
  ## A node bridge.nim writes with `addTree` alone.
  wrapAt p, m, tag, NoLineInfo

proc pragmaBlock*(p: var Parser; m: Mark) =
  ## `parseStmtPragma`: a pragma followed by a block is an `nkPragmaBlock`,
  ## `(pragmax pragmas body)`, positioned at the pragma.
  let kids = takeTail(p, m)
  if kids.len == 1:
    p.dest.addSubtree kids[0]
  else:
    addParLe(p.dest, registerTag("pragmax"), kids[0].info)
    for k in kids: p.dest.addSubtree k
    addParRi p.dest

proc inheritLayout*(p: var Parser; m: Mark) =
  ## `nkOfInherit`, created at `of`: one type is written as itself, several
  ## as `(par A B)`.
  let kids = takeTail(p, m)
  if kids.len == 1:
    p.dest.addSubtree kids[0]
  else:
    addParLe(p.dest, registerTag("par"), m.info)
    for k in kids: p.dest.addSubtree k
    addParRi p.dest

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
  p.dest.addSubtree kids[^1]
  for i in 0 ..< kids.len - 1: p.dest.addSubtree kids[i]

proc addParams(p: var Parser; c: Cursor; info: NifLineInfo) =
  ## `parseParamList` always builds an `nkFormalParams`, created at the token
  ## where the list would start.
  if c.isEmpty:
    addParLe(p.dest, registerTag("params"), info)
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
    addParams p, params, m.info
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
        addParams p, params, m.info
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
  addParLe(p.dest, registerTag(keyword), p.infos[^1])
  for i in 0 .. 3: p.dest.addSubtree kids[i]
  addParams p, kids[4], NoLineInfo
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
  # Only the unsuffixed kinds are bare atoms in nifler; every sized kind,
  # the 64-bit ones included, is `(suf value "i64")`. `tkInt64Lit` is also
  # what a plain literal becomes once it leaves int32's range, exactly as in
  # compiler/lexer.nim, so a large plain literal is written with a suffix too.
  of tkIntLit:
    addIntLit(p.dest, p.tok.iNumber, info)
  of tkInt8Lit, tkInt16Lit, tkInt32Lit, tkInt64Lit:
    p.dest.buildTree registerTag("suf"), info:
      addIntLit(p.dest, p.tok.iNumber, info)
      addStrLit(p.dest, (case p.tok.kind
                         of tkInt8Lit: "i8"
                         of tkInt16Lit: "i16"
                         of tkInt32Lit: "i32"
                         else: "i64"), info)
  of tkUIntLit:
    addUIntLit(p.dest, cast[uint64](p.tok.iNumber), info)
  of tkUInt8Lit, tkUInt16Lit, tkUInt32Lit, tkUInt64Lit:
    p.dest.buildTree registerTag("suf"), info:
      addUIntLit(p.dest, cast[uint64](p.tok.iNumber), info)
      addStrLit(p.dest, (case p.tok.kind
                         of tkUInt8Lit: "u8"
                         of tkUInt16Lit: "u16"
                         of tkUInt32Lit: "u32"
                         else: "u64"), info)
  of tkFloatLit:
    addFloatLit(p.dest, floatValue(p.tok), info)
  of tkFloat32Lit, tkFloat64Lit, tkFloat128Lit:
    p.dest.buildTree registerTag("suf"), info:
      addFloatLit(p.dest, floatValue(p.tok), info)
      addStrLit(p.dest, (case p.tok.kind
                         of tkFloat32Lit: "f32"
                         of tkFloat64Lit: "f64"
                         else: "f128"), info)
  of tkStrLit:
    addStrLit(p.dest, p.tok.s, info)
  of tkRStrLit, tkTripleStrLit, tkGStrLit, tkGTripleStrLit:
    # a generalized string literal is raw: `parseGStrLit` makes the argument
    # an `nkRStrLit` (or `nkTripleStrLit`)
    p.dest.buildTree registerTag("suf"), info:
      addStrLit(p.dest, p.tok.s, info)
      addStrLit(p.dest, (if p.tok.kind in {tkRStrLit, tkGStrLit}: "R" else: "T"), info)
  of tkCharLit:
    addCharLit(p.dest, (if p.tok.s.len > 0: p.tok.s[0] else: '\0'), info)
  of tkCustomLit:
    # `identOrLiteral` turns `-1'big` into a call of the suffix operator:
    # `nkDotExpr(nkRStrLit("-1"), ident("'big"))`, the apostrophe included.
    p.dest.buildTree registerTag("dot"), info:
      p.dest.buildTree registerTag("suf"), info:
        addStrLit(p.dest, p.tok.s.substr(0, int(p.tok.suffixPos) - 1), info)
        addStrLit(p.dest, "R", info)
      addIdent(p.dest, p.tok.s.substr(int(p.tok.suffixPos)), info)
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
