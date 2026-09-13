#       Nifler2
# (c) Copyright 2026 Andreas Rumpf
#
# See the file "license.txt", included in this
# distribution, for details about the copyright.

## The grammar of Nim.
##
## Transcribed from Nim's `doc/grammar.txt`, which is itself generated from the
## `#|` comments in `compiler/parser.nim`. See
## `doc/internals/parser_generator.md` for the notation.
##
## Transcription rules followed here:
##
## * `doc/grammar.txt` is the source, and where it disagrees with what
##   `parser.nim` actually does, the transcription follows **parser.nim** and
##   says so in a `# GRAMMAR.TXT:` note. Collecting those is one of the points
##   of the exercise.
## * Ordered choice (`/`) does not exist in the notation. Every `/` in the
##   original is resolved into either disjoint alternatives (usually by an
##   indentation class), a left-factored set of entries, or a declared
##   predicate — and the ones that needed a predicate are marked `# PRED`.
## * `OP0`..`OP10` and the eleven precedence-level rules are gone; `binary(...)`
##   owns precedence, exactly as `parser.nim` does.
## * `DED` is implicit in `indented(...)`.

import nimlexer, nifbuilder   # terminals and the output buffer; see parsegen

grammar:

  # --------------------------------------------------------------- helpers

  module "stmts[ complexOrSimpleStmt ^* (';' | IND{=}) ]"

  comma     "',' trailComment?"
  semicolon "';' trailComment?"
  colon     "':' trailComment?"
  colcom    "':' trailComment?"

  # `COMMENT?` in doc/grammar.txt is never "a comment may appear here": it is
  # `skipComment`, which is `if p.tok.indent < 0: rawSkipComment`. So it means
  # *a trailing comment on the line just parsed* -- a comment that starts its
  # own line is a statement, or a field's doc, or nothing. `flexComment` is
  # the other spelling parser.nim uses, and allows the indented form too.
  # Getting this wrong is not a tag mismatch but a parse failure: a `##` block
  # as the entire body of a proc was consumed as the *routine's* comment, and
  # then `stmt` had a dedent to work with.
  trailComment "NO_IND COMMENT"              # parser.nim's `skipComment`
  flexComment  "validInd COMMENT"            # parser.nim's `flexComment`

  # Named indentation guards. These are zero-width: they constrain the class
  # of the current token without consuming it.
  validInd    "NO_IND | IND{>}"            # parser.nim's `validInd`
  optSameInd  "NO_IND | IND{=}"            # parser.nim's `sameOrNoInd`
  notInd      "NO_IND | IND{=} | IND{<}"   # the `else` of parser.nim's `if realInd(p)`

  optInd "trailComment? validInd"
  optPar "NO_IND | IND{>} | IND{=}"
  # GRAMMAR.TXT: says `(IND{>} | IND{=})?`, i.e. unconstrained. parser.nim's
  # `optPar` errors on a dedent, so it is an assertion, not an option.

  # `operator` and `prefixOperator` are documentation only: operator dispatch
  # happens inside `binary(...)` and in `primary`'s prefix alternative.
  operator """OPR | @'or' | @'xor' | @'and' | @'is' | @'isnot' | @'in' | @'notin'
          | @'of' | @'as' | @'from' | @'div' | @'mod' | @'shl' | @'shr' | @'not'
          | @'..'"""
  # `@'…'` emits the keyword as the operator's name: `not x` is
  # `(prefix not x)`, and a plain terminal would have left `(prefix x)`.
  prefixOperator "operator"
  operatorB "operator"
  # GRAMMAR.TXT: lists `operator` and `operatorB` separately with the same
  # right-hand side modulo ordering.

  plainSymbol """quoted[ '`' (KEYW | IDENT | literal
                        | (OPR | '.' | '..' | '=' | '(' | ')' | '[' | ']'
                          | '{' | '}' | '[.' | '.]' | '{.' | '.}'
                          | '(.' | '.)')+)+ '`' ]"""
  plainSymbol "IDENT"
  symbol "plainSymbol"
  symbol "'addr'"
  symbol "'type'"
  symbol "'static'"
  # GRAMMAR.TXT: one `symbol` production, and an operator run of
  # `operator | '(' | ')' | '[' | ']' | '{' | '}' | '='`. `parseSymbol`'s
  # accent loop takes `{tkOpr, tkDot, tkDotDot, tkEquals, tkParLe..tkParDotRi}`,
  # so `{.borrow: `.`.}` needs the bare dot.
  # `plainSymbol` exists because `parseSymbol` accepts `addr`, `type` and
  # `static` as identifiers but every *dispatch* that asks "does a
  # declaration start here" tests `{tkSymbol, tkAccent}` only:
  # `parseIdentColonEquals`, `parseParamList`, `parseObjectPart`,
  # `parseSection`, `parseVarTuple` and `parseTuple` all do. A field list has
  # to stop at the `type` keyword that starts the next definition.

  symbolOrKeyword "symbol"
  symbolOrKeyword "KEYW"

  # --------------------------------------------------------------- literals

  literal """INT_LIT | INT8_LIT | INT16_LIT | INT32_LIT | INT64_LIT
         | UINT_LIT | UINT8_LIT | UINT16_LIT | UINT32_LIT | UINT64_LIT
         | FLOAT_LIT | FLOAT32_LIT | FLOAT64_LIT | FLOAT128_LIT
         | STR_LIT | RSTR_LIT | TRIPLESTR_LIT
         | CHAR_LIT | CUSTOM_NUMERIC_LIT
         | nil[ 'nil' ]"""
  # GRAMMAR.TXT: writes `NIL` as a token class; it is the keyword.
  # GRAMMAR.TXT: omits FLOAT128_LIT, which the lexer produces.

  generalizedLit "GENERALIZED_STR_LIT | GENERALIZED_TRIPLESTR_LIT"

  # --------------------------------------------------------------- exprs

  exprColonEqExpr "kv[ expr ':' expr ]"
  exprColonEqExpr "vv[ expr '=' expr ]"
  exprColonEqExpr "expr (doBlock extraPostExprBlock*)?"

  exprEqExpr "vv[ expr '=' expr ]"
  exprEqExpr "expr (doBlock extraPostExprBlock*)?"

  exprList            "expr ^+ comma"
  optionalExprList    "expr ^* comma"
  exprColonEqExprList "exprColonEqExpr (comma exprColonEqExpr?)*"
  # GRAMMAR.TXT: `exprColonEqExpr (comma exprColonEqExpr)* comma?`, which is
  # not LL(1): after a comma one token cannot say whether it was a separator
  # or the trailing one. `exprColonEqExprListAux` loops "expr; break unless
  # comma", i.e. it decides on the token *after* the comma -- which is what
  # the form above spells, at the price of also accepting `[a, , b]`.

  qualifiedIdent "dot[ symbol '.' optInd symbolOrKeyword ]"
  qualifiedIdent "symbol"

  setOrTableConstr "curly[ '{' optInd exprColonEqExprList? optPar '}' ]"
  setOrTableConstr "tabconstr[ '{' ':' optPar '}' ]"
  # GRAMMAR.TXT: `'{' ((exprColonEqExpr comma)* | ':') '}'` makes the comma
  # mandatory after every element, so `{a, b}` did not parse -- parser.nim
  # breaks out of the loop on a non-comma, which is `^* comma` plus an
  # optional trailing one, i.e. `exprColonEqExprList`.

  castExpr "cast[ 'cast' '[' optInd typeDesc optPar ']' '(' optInd expr optPar ')' ]"
  castExpr "cast[ 'cast' '(' optInd exprColonEqExpr optPar ')' ]"
  # GRAMMAR.TXT: has the second form in a *commented-out* `#|` line, so it
  # never reached `grammar.txt` -- but `parseCast` has it, and it is the
  # `{.cast(noSideEffect).}` pragma. TAG: parser.nim puts an empty node where
  # the type would be; the notation has no way to say "empty child" yet.

  parKeyw """'discard' | 'include' | 'if' | 'while' | 'case' | 'try'
         | 'finally' | 'except' | 'for' | 'block' | 'const' | 'let'
         | 'when' | 'var' | 'mixin'"""

  semiStmtItem "ifExpr | whenExpr | complexOrSimpleStmt"
  semiStmtList "semiStmtItem (';' semiStmtItem? | semiStmtItem)*"
  # GRAMMAR.TXT: writes every one of these as `(ifExpr / complexOrSimpleStmt)
  # ^+ ';'`, and omits `whenExpr`. `semiStmtList` takes `when` to
  # `parseIfOrWhenExpr` exactly as it takes `if`, and its `;` is *optional*:
  # the loop runs `if tkSemiColon: getTok; if tkParRi: break;
  # complexOrSimpleStmt`, so `( a\n b )` needs no separator at all and
  # `( a; )` may end with one. A separator repetition can say neither.

  par """stmts[ '(' optInd &parKeyw withInd( semiStmtList ) optPar ')' ]"""
  par """stmts[ '(' optInd ';' withInd( semiStmtList )? optPar ')' ]"""
  par "par[ '(' optInd pragmaStmt optPar ')' ]"
  par "tup[ '(' optPar ')' ]"
  par """par[ '(' optInd simpleExpr({-1}, {pmNormal})
                 (doBlock extraPostExprBlock*) optPar ')' ]"""
  par """par[ '(' optInd simpleExpr({-1}, {pmNormal}) asgn[ '=' expr ] optPar ')' ]"""
  par """stmts[ '(' optInd simpleExpr({-1}, {pmNormal}) asgn[ '=' expr ]
                   ';' withInd( semiStmtList )? optPar ')' ]"""
  par """stmts[ '(' optInd simpleExpr({-1}, {pmNormal})
                   ';' withInd( semiStmtList )? optPar ')' ]"""
  par """tup[ '(' optInd simpleExpr({-1}, {pmNormal}) (^kv[ ':' expr ])+
                 (comma exprColonEqExpr?)* optPar ')' ]"""
  par """tup[ '(' optInd simpleExpr({-1}, {pmNormal})
                 comma (exprColonEqExpr comma?)* optPar ')' ]"""
  par "par[ '(' optInd simpleExpr({-1}, {pmNormal}) optPar ')' ]"
  # All but the first three share the `simpleExpr` prefix and are
  # left-factored on the token that follows it -- `do`, `=`, `;`, `:`, `,`,
  # `)` -- which is exactly how `parsePar` decides. The first is chosen by a
  # FIRST-set lookahead over `parKeyw`, the second by ';'.
  # GRAMMAR.TXT: does not spell out `'(' ')'`, `'(' simpleExpr ')'` or the
  # comma form, and gives no tags. `parsePar` starts every alternative as an
  # `nkPar` and *retags* it: to `nkTupleConstr` on `()`, on a `:` and on a
  # `,`, and to `nkStmtListExpr` whenever `semiStmtList` runs. The tags above
  # are those outcomes, which is what retroactive wrapping buys: a tag is a
  # property of the alternative, not of the token that opened it.
  # The `asgn[...]` tag sits *inside* the alternative, so it wraps from the
  # alternative's own mark -- i.e. over the `simpleExpr` that was parsed
  # before it. `kv` does the same from inside a repetition, whose body has a
  # mark of its own, and therefore needs the `^` to reach back. Writing them as an outer
  # `par[ asgn[ ... ] ]` instead would hide `simpleExpr` behind `asgn` and
  # defeat the left-factoring.
  # `(^kv[ ':' expr ])+` can only run once on valid input: `expr` does not stop
  # at a `:`, so nothing after the first one can be a `:` again.
  # GRAMMAR.TXT: no indentation handling at all. `semiStmtList` runs under
  # `withInd`, which is `indented(...)` minus the assertion that the block is
  # deeper -- so `const x = (\n  when a: 1\n  elif b: 2)` measures its `elif`
  # against the `when`, and `(if a: 1 else: 2)` on one line still works
  # because `withInd` happily takes -1 as the indentation.

  tupleConstr  "par[ '(' flexComment? optPar ')' ]"
  tupleConstr  "par[ '(' flexComment? optPar exprColonEqExpr optPar ')' ]"
  tupleConstr  """tup[ '(' flexComment? optPar exprColonEqExpr comma
                        (exprColonEqExpr comma?)* optPar ')' ]"""
  # Same retag: `exprColonEqExprListAux` builds an `nkPar` and turns it into
  # an `nkTupleConstr` the moment it sees a comma, so `(int)` is a `par` and
  # `(int, float)` and `(int,)` are `tup`s. Three entries, one shared prefix.
  arrayConstr  "bracket[ '[' flexComment? optPar exprColonEqExprList? optPar ']' ]"
  # GRAMMAR.TXT: `(exprColonEqExpr comma?)*` would accept `[a b]`;
  # `exprColonEqExprListAux` breaks out of its loop on a non-comma and then
  # eats the `]`. The `?` is what makes `x[]` parse.
  # GRAMMAR.TXT: `optInd` after the bracket. `exprColonEqExprListAux` runs
  # `flexComment; optPar`, and the difference shows on a list whose closing
  # bracket sits at the *enclosing* indentation -- `optInd` is
  # `NO_IND | IND{>}` and rejects it, `optPar` allows IND{=}. An array
  # literal whose body is nothing but `#` comments is exactly that shape.

  identOrLiteral(mode: PrimaryMode) """generalizedLit | symbol | literal
                | arrayConstr | setOrTableConstr | castExpr"""
  identOrLiteral(mode: PrimaryMode) "&parIsTuple(mode) tupleConstr"
  identOrLiteral(mode: PrimaryMode) "par"
  # PRED: `par` vs `tupleConstr` are the same prefix `'('` and the choice is
  # `mode in {pmTypeDesc, pmTypeDef}` -- in a type a `'('` is a tuple, so
  # `(int, var T)` is a return type and not a parenthesised statement list.
  # Without the mode the generator factored the two together and the
  # comma never got its meaning.

  primarySuffix(mode: PrimaryMode) """&noSpaceBefore
                        ^call[ '(' flexComment? optPar exprColonEqExprList? optPar ')' ]"""
  primarySuffix(mode: PrimaryMode) """^dot[ '.' optInd symbolOrKeyword
                             ('[:' exprList ']' ('(' exprColonEqExpr ')')?)? ] generalizedLit?"""
  primarySuffix(mode: PrimaryMode) "&dotLikeOps ^dot[ DOTLIKEOP optInd symbolOrKeyword ] generalizedLit?"
  primarySuffix(mode: PrimaryMode) """&noSpaceBefore
                        ^at[ '[' flexComment? optPar exprColonEqExprList? optPar ']' ]"""
  primarySuffix(mode: PrimaryMode) """&noSpaceBefore
                        ^curlyat[ '{' flexComment? optPar exprColonEqExprList? optPar '}' ]"""
  primarySuffix(mode: PrimaryMode) """&commandStart &commandAllowed(mode)
                                      ^cmd[ commandParam(mode) ]"""
  # GRAMMAR.TXT: `commandStart expr ...` reads like a repetition and was
  # transcribed as `commandParam+`. parser.nim's `commandExpr` takes exactly
  # ONE parameter and then breaks out of `primarySuffix`'s loop: `echo a b c`
  # is `(cmd echo (cmd a (cmd b c)))`, not a flat command. The `+` also made
  # the command swallow the *next statement*, because a repetition's own loop
  # test is not under `suffixStart`.
  # PRED: `commandAllowed` is `mode != pmTrySimple`. parser.nim's
  # `commandExpr` returns its operand untouched in that mode, so a command is
  # not a suffix there at all -- which is what lets `exprStmt` parse
  # `echo a, b` as one command with two parameters.
  # PRED: `noSpaceBefore` is `tsLeading notin tok.spacing` — a space before
  # '(' turns a call into a command. `dotLikeOps` is the `nimPreviewDotLikeOps`
  # switch plus `isDotLike`.
  # The whole loop only runs while NO_IND, or on a '.' at IND{>=}; that is a
  # property of the enclosing repetition in `simplePrimary`.

  pragma "pragmas[ '{.' optInd (exprColonEqExpr comma?)* optPar ('.}' | '}') ]":
    enter: inc p.inPragma
    leave: dec p.inPragma

  identVis "postfix[ plainSymbol OPR ]"
  identVis "plainSymbol"
  identVisDot "postfix[ plainSymbol OPR ]"
  identVisDot "dot[ plainSymbol '.' optInd symbolOrKeyword ]"
  identVisDot "plainSymbol"
  # GRAMMAR.TXT: `symbol '.' optInd symbolOrKeyword OPR?` -- the dot is not
  # optional there and an OPR may follow it. parser.nim's `identVis(allowDot)`
  # is an if/elif on ONE of the two: an `OPR` wins, a `'.'` is the other
  # branch, and a bare symbol is the fallthrough. So `type T = object` never
  # reached the `=`.

  identWithPragma    "pragmax[ identVis pragma ]"
  identWithPragma    "identVis"
  identWithPragmaDot "pragmax[ identVisDot pragma ]"
  identWithPragmaDot "identVisDot"

  declColonEquals """identWithPragma (comma identWithPragma)* comma?
                   (':' optInd typeDescExpr)? ('=' optInd expr)?""":
    fanOut m, p.section          # one (var|let|param|fld ...) per name
  identColonEquals """plainSymbol (comma plainSymbol)* comma?
                    (':' optInd typeDescExpr)? ('=' optInd expr)?""":
    fanOut m, p.section
  declColonEqualsDot """identWithPragmaDot (comma identWithPragmaDot)* comma?
                      (':' optInd typeDescExpr)? ('=' optInd expr)?""":
    fanOut m, p.section

  # --------------------------------------------------------------- types

  tupleTypeBracket "'[' optInd (identColonEquals (comma | semicolon)?)* optPar ']'"
  tupleType "tuple[ 'tuple' tupleTypeBracket? ]"
  # GRAMMAR.TXT: the bracket is mandatory. `parseTuple(indentAllowed=false)`
  # leaves a bare `tuple` alone, which is how `[T: tuple|object]` and
  # `x: typedesc[T]` with `T: tuple` parse.
  tupleDecl "tuple[ 'tuple' tupleTypeBracket ]"
  tupleDecl """tuple[ 'tuple' trailComment?
             (indented( (optSameInd COMMENT)?
                        (identColonEquals (optPar COMMENT)?) ^+ IND{=} ))? ]"""
  # GRAMMAR.TXT: `'tuple' COMMENT? (IND{>} identColonEquals (IND{=}
  # identColonEquals)*)?`. The two comment positions inside the block are
  # `parseTuple`'s `rawSkipComment` calls -- one for the tuple's own doc
  # comment on the line after `tuple`, one for each field's trailing one.

  paramList      """params[ '(' optInd
                    (declColonEquals ((comma | semicolon) declColonEquals?)*)?
                    optPar ')' ]"""
  # GRAMMAR.TXT: `'(' declColonEquals ^* (comma/semicolon) ')'`, which makes
  # the separator a real separator. `parseParamList` breaks its loop on a
  # `)`, so `proc f(q: var Queue;)` is legal -- and the same shape (decide on
  # the token *after* the separator) is what every one of these lists needs.
  paramListArrow "(NO_IND paramList)? (NO_IND '->' optInd typeDesc)?"
  paramListColon "(NO_IND paramList)? (NO_IND ':' optInd typeDesc)?"
  # `parseParamList`: `hasParLe = p.tok.tokType == tkParLe and p.tok.indent
  # < 0`, and the return type likewise. A `(` that opens a line is the next
  # statement, not this routine's parameters.

  doBlock "proc[ 'do' paramListArrow pragma? colcom stmt ]"
  routineExpr "proc[ 'proc' paramListColon pragma? ('=' trailComment? stmt)? ]"
  routineExpr "func[ 'func' paramListColon pragma? ('=' trailComment? stmt)? ]"
  routineExpr "iterator[ 'iterator' paramListColon pragma? ('=' trailComment? stmt)? ]"
  routineType "proc[ 'proc' paramListColon pragma? ]"
  routineType "itertype[ 'iterator' paramListColon pragma? ]"

  rawTypeDesc "routineType (^infix[ @'not' primary({pmTypeDesc}) ])?"
  rawTypeDesc "typeDescKeyw({pmTypeDesc})"
  typeDescKeyw(mode: PrimaryMode) """(tupleType | enum[ 'enum' ] | object[ 'object' ]
               | mut[ 'var' typeKAuxOperand(mode)? ] | out[ 'out' typeKAuxOperand(mode)? ]
               | ref[ 'ref' typeKAuxOperand(mode)? ] | ptr[ 'ptr' typeKAuxOperand(mode)? ]
               | distinct[ 'distinct' typeKAuxOperand(mode)? ])
               (^infix[ @'not' primary({pmTypeDesc}) ])?"""
  typeKAuxOperand(mode: PrimaryMode) "&isTypedefOperand(mode) validInd typeDefValue"
  typeKAuxOperand(mode: PrimaryMode) "&typeOperandFollows validInd primary(mode)"
  # GRAMMAR.TXT: `('var'|'out'|'ref'|'ptr'|'distinct') typeDesc?`.
  # `parseTypeDescKAux` returns *early* on a dedent (`validInd`), skips an
  # operator (`&typeOperandFollows`), and parses a `primary`, not a
  # `typeDesc` -- so `ptr UncheckedArray[T]` works because `primary` owns the
  # suffixes, and `ptr | pointer` leaves the `ptr` bare.
  # `rawTypeDesc` minus `routineType`, because `primary` needs exactly that:
  # parser.nim's `primary` sends `tkTuple`/`tkEnum`/`tkObject`/`tkVar`/`tkOut`/
  # `tkRef`/`tkPtr`/`tkDistinct` to `parseTypeDesc` and `tkProc`/`tkFunc`/
  # `tkIterator` to `parseProcExpr`, so `proc` in expression position is a
  # routine *expression*, never a routine type.
  typeDescExpr """simpleExpr({-1}, {pmTypeDesc})
               (^infix[ @'not' primary({pmTypeDesc}) ])?"""
  # GRAMMAR.TXT: `(routineType / simpleExpr) ('not' primary)?`.
  # `typeDescExpr` is `parseTypeDesc(fullExpr = true)`, whose whole body is
  # `simpleExpr(p, pmTypeDesc)` -- `routineType` as a *separate* alternative
  # took `proc` out of the operator loop, so `p: proc | iterator {.closure.}`
  # stopped at the `|`.
  typeDesc "rawTypeDesc"
  typeDesc "%else typeDescExpr"
  # PRED: 'var'/'out'/'ref'/'ptr'/'distinct'/'tuple'/'enum'/'object'/'proc'/
  # 'iterator' select rawTypeDesc; everything else typeDescExpr. 'proc' and
  # 'iterator' overlap and are left-factored.

  # --------------------------------------------------------------- primary

  forStmt "for[ 'for' ((varTupleLhs | identWithPragma) ^+ comma) 'in' expr colcom stmt ]"
  # GRAMMAR.TXT: says `varTuple`, which ends in `'=' optInd expr`. `parseFor`
  # calls `parseVarTuple`, which is `varTupleLhs`: `for (k, v) in attrs:` has
  # no `=`.
  forExpr "forStmt"

  expr "blockExpr | ifExpr | whenExpr | caseStmt | forExpr | tryExpr"
  expr "simpleExpr({-1}, {pmNormal})"

  simplePrimary(mode: PrimaryMode) "identOrLiteral(mode) (&suffixStart primarySuffix(mode))*"
  # PRED: `suffixStart` is parser.nim's `primarySuffix` loop guard --
  # `p.tok.indent < 0 or (tkDot and p.tok.indent >= baseIndent)`. Without it
  # `let x = 1` followed by `echo 2` on the next line parses as one command,
  # because `commandStart` is happy with any token that can start an
  # expression and the indentation never enters the decision.
  # GRAMMAR.TXT: writes `SIGILLIKEOP? identOrLiteral ...` here, but parser.nim
  # has no such alternative: an operator always takes the prefix branch, and
  # `isSigilLike` only makes the operand bind tighter (`@x.y` is `@(x.y)`'s
  # sibling, not a separate production). Its own comment says as much, and
  # that sigils "should be removed for Nim 2.0". Modelling it as a real
  # alternative is what made SIGILLIKEOP collide with OPR in `primary`.

  # Not in doc/grammar.txt at all, though parser.nim has it.
  commandParam(mode: PrimaryMode) "&inTypeDesc(mode) simpleExpr({-1}, mode)"
  commandParam(mode: PrimaryMode) "%else expr (doBlock extraPostExprBlock*)?"
  # parser.nim's `commandParam` has a third branch, `elif not isFirstParam:
  # exprEqExpr`, and it is unreachable from `commandExpr`, which always passes
  # `isFirstParam = true`. The two callers that pass `false` --
  # `parseExprStmt`'s command branch and `parseTypeDefValue`'s `while tkComma`
  # loop -- both spell it as a repetition, and `exprStmt` and `typeDefValue`
  # here already model those as `(exprEqExpr ^+ comma)`. So the branch is not
  # missing; it is written where it is actually reached.

  # GRAMMAR.TXT: has a `commandStart` production listing the tokens an
  # expression can start with. It was a rule here, which made `&commandStart` a
  # FIRST-set lookahead and therefore inert -- the real condition is not a
  # token set at all. parser.nim guards its command branch with
  # `p.inPragma == 0 and (isUnary(p.tok) or p.tok.tokType notin {tkOpr,
  # tkDotDot})`: an *infix* operator must fall through to `binary`, so
  # `import std / os` is `(infix / std os)` and not a command. It is a
  # predicate now, and the token set is FIRST(commandParam).

  primary(mode: PrimaryMode) "simplePrimary(mode)"
  # GRAMMAR.TXT: documents the command syntax here --
  # `simplePrimary (commandStart expr (doBlock extraPostExprBlock*)?)?` -- but
  # parser.nim implements it inside `primarySuffix`'s loop, which is the only
  # place it is under the indentation guard. Transcribing it in both places
  # made `let x = 1` followed by `echo 2` parse as one command: the copy here
  # has no guard to be under. `primarySuffix`'s `cmd` alternative is the one
  # that matches parser.nim, and `commandParam`'s `%else` alternative already
  # carries the `doBlock extraPostExprBlock*` tail.
  primary(mode: PrimaryMode) """&isSigilLike
              prefix[ prefixOperator (simplePrimary(mode) | %else primary({pmNormal})) ]"""
  # `primary`'s sigil branch is `if isSigil and p.tok.tokType in
  # identOrLiteralKinds: identOrLiteral + primarySuffix else: primary(pmNormal)`
  # -- the fallback is what parses `@! === result = "abc"`. It is a two-token
  # decision in parser.nim and a one-token one here, because the operator is
  # already consumed when the operand is dispatched.
  primary(mode: PrimaryMode) "&isUnary prefix[ operatorB primary(mode) ]"
  primary(mode: PrimaryMode) "routineExpr"
  primary(mode: PrimaryMode) "bind[ 'bind' optInd primary({pmNormal}) ]"
  # GRAMMAR.TXT: omits it. `primary` has `of tkBind:` -- legacy syntax that is
  # a no-op in current Nim, but it still has to parse.
  primary(mode: PrimaryMode) "typeDescKeyw(mode)"
  # GRAMMAR.TXT: reads as though `rawTypeDesc` were only reachable in a type
  # description, and it was transcribed with an `&inTypeDesc(mode)`. parser.nim
  # sends `tkTuple`/`tkEnum`/`tkObject`/`tkConcept`/`tkVar`/`tkOut`/`tkRef`/
  # `tkPtr`/`tkDistinct` to `parseTypeDesc` whatever the mode is; only
  # `proc`/`func`/`iterator` consult it, and there it decides whether a *body*
  # is allowed, not which production runs.
  primary(mode: PrimaryMode) "prefix[ prefixOperator primary(mode) ]"
  # PRED: `operatorB primary` vs `prefixOperator primary` are distinguished by
  # `isUnary` (spacing) in parser.nim; the two entries are otherwise identical.

  simpleExpr(limit: int, mode: PrimaryMode) "binary( primaryPragma(mode), getPrecedence, isRightAssoc, infix, limit )"
  primaryPragma(mode: PrimaryMode) """primary(mode)
                (&pragmaOnPrimary(mode) validInd ^pragmax[ pragma ])?"""
  # GRAMMAR.TXT: `simpleExpr = ... pragma?`, unguarded. `simpleExprAux` takes
  # the pragma only when it is `NO_IND` or a real indent AND the mode is
  # `pmNormal`. Without the guard a `{.noSideEffect.}:` block on the line
  # after `var a = 1` became the *value's* pragma and then took the `:` block
  # that belonged to it.

  # --------------------------------------------------------- post-expr blocks

  extraPostExprBlock "IND{=} doBlock"
  extraPostExprBlock "of[ IND{=} 'of' exprList ':' stmt ]"
  extraPostExprBlock "elif[ IND{=} 'elif' expr ':' stmt ]"
  extraPostExprBlock "except[ IND{=} 'except' optionalExprList ':' stmt ]"
  extraPostExprBlock "fin[ IND{=} 'finally' ':' stmt ]"
  extraPostExprBlock "else[ IND{=} 'else' ':' stmt ]"

  postExprBlocks "NO_IND doBlock extraPostExprBlock*"
  postExprBlocks "NO_IND ':' trailComment? (extraPostExprBlock | stmt) extraPostExprBlock*"
  # GRAMMAR.TXT: omits the guard; `postExprBlocks` opens with
  # `if p.tok.indent >= 0: return`. It belongs in the rule rather than at the
  # eight `postExprBlocks?` call sites, all of which need it.

  # --------------------------------------------------------------- stmts

  exprStmt """asgn[ simpleExpr({-1}, {pmTrySimple}) '='
                  optInd expr postExprBlocks? ]"""
  exprStmt """cmd[ simpleExpr({-1}, {pmTrySimple})
                 NO_IND (exprEqExpr ^+ comma) postExprBlocks? ]"""
  # GRAMMAR.TXT: omits the indentation guard. parser.nim's `parseExprStmt`
  # enters the command branch only `if p.tok.indent < 0 and isExprStart(p)`,
  # and without the `NO_IND` here `echo 1` followed by `echo 2` on the next
  # line is one command with two parameters.
  # GRAMMAR.TXT: says `simplePrimary` here; parser.nim calls
  # `simpleExpr(p, pmTrySimple)` for all three and decides afterwards, which is
  # also what makes the three left-factor.
  exprStmt "simpleExpr({-1}, {pmTrySimple}) postExprBlocks?"
  # The three share the `simpleExpr`/`simplePrimary` prefix; left-factored,
  # with the `asgn`/`cmd` tags inserted at the mark once the alternative is
  # known. This is the motivating case for retroactive wrapping.

  importStmt "import[ 'import' optInd expr (comma expr)* ]"
  importStmt "importexcept[ 'import' optInd expr 'except' optInd (expr ^+ comma) ]"
  exportStmt "export[ 'export' optInd expr (comma expr)* ]"
  exportStmt "exportexcept[ 'export' optInd expr 'except' optInd (expr ^+ comma) ]"
  includeStmt "include[ 'include' optInd expr ^+ comma ]"
  fromStmt "fromimport[ 'from' expr 'import' optInd expr (comma expr)* ]"

  optExprBody "COMMENT"        # `parseReturnOrRaise` takes a comment either way
  optExprBody "validInd expr postExprBlocks?"
  returnStmt   "ret[ 'return' optExprBody? ]"
  raiseStmt    "raise[ 'raise' optExprBody? ]"
  yieldStmt    "yld[ 'yield' optExprBody? ]"
  discardStmt  "discard[ 'discard' optExprBody? ]"
  breakStmt    "break[ 'break' optExprBody? ]"
  continueStmt "continue[ 'continue' optExprBody? ]"
  # GRAMMAR.TXT: writes `'return' optInd expr?` for all six, which asserts the
  # indentation even when the expression is absent -- and `return` at the end
  # of a block is followed by a *dedent*. parser.nim's `parseReturnOrRaise`
  # never calls `optInd`; it tests `indent >= 0 and indent <= currInd or not
  # isExprStart(p)` and takes the empty branch. `(optInd expr)?` is still not
  # it: `optInd` is `COMMENT? validInd`, so a *comment* enters the group and
  # then the indentation assertion fires on whatever follows the comment.
  # `parseReturnOrRaise` takes a comment as the whole answer -- `discard`
  # followed by an indented doc comment has no expression at all -- which is
  # what `optExprBody` says. The `postExprBlocks?` is missing from
  # grammar.txt as well: `return quote: toJs(x)` needs it.

  condStmt """elif[ expr colcom stmt ] trailComment?
            (elif[ optSameInd 'elif' expr colcom stmt ])*
            (else[ optSameInd 'else' colcom stmt ])?"""
  # GRAMMAR.TXT: `IND{=}` on both. `parseIfOrWhen` tests `sameOrNoInd`, which
  # is what makes the one-liner `if c: a else: b` a *statement* and not just
  # an `ifExpr` -- and one-liners are everywhere.
  ifStmt   "if[ 'if' condStmt ]"
  whenStmt "when[ 'when' condStmt ]"

  condExpr """elif[ optInd expr colcom stmt ] trailComment?
            (elif[ 'elif' optInd expr colcom stmt ] trailComment?)*
            (else[ 'else' colcom stmt ])?"""
  # GRAMMAR.TXT: `expr colcom stmt optInd ('elif' ...)* 'else' colcom stmt`.
  # Two things are wrong. The `optInd` is the one at the *start* of each
  # branch, before its expression -- between the branches
  # `parseIfOrWhenExpr` asserts nothing at all, which is what lets
  # `let x = if c:` put its `else` back at the `let`'s own indentation. And
  # the `else` is optional (`if p.tok.tokType == tkElse`), so
  # `(if it.native: flag = true)` parses.
  ifExpr   "if[ 'if' condExpr ]"
  whenExpr "when[ 'when' condExpr ]"

  whileStmt "while[ 'while' expr colcom stmt ]"

  ofBranch   "of[ 'of' exprList colcom stmt ]"
  ofBranches """ofBranch (IND{=} ofBranch)*
              (elif[ IND{=} 'elif' expr colcom stmt ])*
              (else[ IND{=} 'else' colcom stmt ])?"""
  ofBranches """(elif[ 'elif' expr colcom stmt ])+
              (else[ IND{=} 'else' colcom stmt ])?"""
  ofBranches "else[ 'else' colcom stmt ]"
  # GRAMMAR.TXT: the first `ofBranch` is mandatory. `parseCase`'s loop is a
  # `case` over `of`/`elif`/`else` with no ordering requirement beyond "no
  # `of` after an `elif`", so a `case` whose only branch is `else` -- a
  # placeholder switch waiting for its first flag -- is legal.
  caseStmt "case[ 'case' expr ':'? trailComment? (indented( ofBranches ) | IND{=} ofBranches) ]"

  tryStmt """try[ 'try' colcom stmt &(optSameInd ('except' | 'finally'))
           (except[ optSameInd 'except' optionalExprList colcom stmt ])*
           (fin[ optSameInd 'finally' colcom stmt ])? ]"""
  tryExpr """try[ 'try' colcom stmt &('except' | 'finally')
           (except[ 'except' optionalExprList colcom stmt ])*
           (fin[ 'finally' colcom stmt ])? ]"""
  # GRAMMAR.TXT: `&(optInd 'except'|'finally')` and an `optInd` on each
  # branch. `parseTry`'s loop is `while sameOrNoInd(p) or isExpr`, so for an
  # *expression* try the indentation is not consulted at all -- which is what
  # `let x = try: i` followed by a dedented `except ValueError as ex:` needs.
  # `optInd` is `NO_IND | IND{>}` and rejected exactly that.

  blockStmt   "block[ 'block' symbol? colcom stmt ]"
  blockExpr   "block[ 'block' symbol? colcom stmt ]"
  staticStmt  "staticstmt[ 'static' colcom stmt ]"
  deferStmt   "defer[ 'defer' colcom stmt ]"
  asmStmt     "asm[ 'asm' pragma? (STR_LIT | RSTR_LIT | TRIPLESTR_LIT) ]"

  genericParamName "prefix[ &inOrOut KEYW plainSymbol ]"
  genericParamName "plainSymbol"
  genericParam """typevar[ genericParamName (comma genericParamName?)*
                (colon expr)? ('=' optInd expr)? ]"""
  # GRAMMAR.TXT: `symbol (comma symbol)* ...`. `parseGenericParam` has an
  # `of tkIn, tkOut:` branch that wraps the name in an `nkPrefix` --
  # `MyPtr[out T]`, the covariance markers -- and breaks its loop on a
  # non-comma, so the comma is trailing-tolerant like every other list here.
  genericParamList """typevars[ '[' optInd
                     (genericParam ((comma | semicolon) genericParam?)*)?
                     optPar ']' ]"""

  pattern "'{' stmt '}'"
  indAndComment "flexComment? | trailComment?"
  # `indAndComment` is `if indent > currInd: (a COMMENT, or "invalid
  # indentation") else: skipComment` -- i.e. `(validInd COMMENT)?`, with the
  # indented form *demanding* the comment. The demand is the only part not
  # modelled.

  routineName "postfix[ symbolOrKeyword OPR ]"
  routineName "symbolOrKeyword"
  routine """optInd (routineName pattern? genericParamList?)? paramListColon
           (validInd pragma)? (validInd '=' trailComment? stmt)? indAndComment"""
  # GRAMMAR.TXT: the name is mandatory and nothing carries an indentation
  # guard. `parseRoutine` delegates to `parseProcExpr` when the token after
  # the keyword is not a name at all, so `proc (): int {.closure.} = x` is a
  # statement -- an anonymous routine, which nifler emits with `.` for the
  # name. The three `validInd`s are `p.validInd` in `parseRoutine`; without
  # them a dedented `{.` or `=` would still be read as part of the routine.
  # `routineName` is `identVis` over `symbolOrKeyword` rather than
  # `plainSymbol`: `parseRoutine` treats *any* keyword as an attempt at a
  # name, which is what `func addr*[T](x: T): ptr T` needs -- and the choice
  # between "a name" and "an anonymous routine" is then exactly
  # `{tkSymbol, tkAccent, KEYW}` against `(` and `:`.

  commentStmt "comment[ COMMENT ]"

  section(R: rule) """trailComment?
                   (indented( (R | COMMENT) ^+ IND{=} ) | NO_IND R)"""
  # GRAMMAR.TXT: `COMMENT? RULE / (IND{>} (RULE / COMMENT)^+IND{=} DED)`, as
  # two alternatives -- but `parseSection` skips the comment *before* deciding
  # between them, so `type ## doc` followed by an indented block needs the
  # comment outside the choice. The single-definition form is NO_IND only
  # (`p.tok.indent < 0`), not "any non-indent".

  enumDecl """enum[ 'enum' optInd flexComment?
                  (validInd symbol pragma? ('=' optInd expr)? comma? flexComment?)+ ]"""
  # GRAMMAR.TXT: no indentation guard on the repetition, and a bare `optInd`
  # in front of the optional `'='`. `parseEnum` ends its loop on
  # `p.tok.indent >= 0 and p.tok.indent <= p.currInd`, which is `validInd` on
  # the next field -- without it the enum ate the *next* type definition of the
  # section, and the stray `optInd` asserted an indentation for a `=` that was
  # not there. The COMMENT is outside the `'='` group because `parseEnum`
  # calls `rawSkipComment` on every exit path, so `A  ## what A means` does
  # not end the field list.

  objectWhen """when[ 'when' elif[ expr colcom objectPart ] flexComment?
              (elif[ 'elif' expr colcom objectPart ] flexComment?)*
              (else[ 'else' colcom objectPart ] flexComment?)? ]"""
  objectBranch "of[ 'of' exprList colcom objectPart ]"
  objectBranches """objectBranch (IND{=} objectBranch)*
                  (else[ IND{=} 'else' colcom objectPart ])?"""
  objectBranches "else[ 'else' colcom objectPart ]"
  # GRAMMAR.TXT: lists `(IND{=} 'elif' expr colcom objectPart)*` between the
  # two. `parseObjectCase`'s loop has no `of tkElif` at all -- an object
  # variant has no `elif` branches.
  objectCase """case[ 'case' (declColonEquals | pragma)? ':'? flexComment?
              (indented( objectBranches ) | IND{=} objectBranches) ]"""

  objectPart "stmts[ indented( (optSameInd COMMENT)? objectPart ^+ IND{=} ) ]"
  # GRAMMAR.TXT: no comment here. `parseObjectPart` calls `rawSkipComment`
  # *inside* its `withInd`, so an object's own doc comment on the line after
  # `object` belongs to the field list, at the field indentation -- not to
  # `objectDecl`, whose `COMMENT?` is `skipComment` and therefore NO_IND.
  objectPart "optSameInd objectWhen"
  objectPart "optSameInd objectCase"
  objectPart "optSameInd nil[ 'nil' ]"
  objectPart "optSameInd nil[ 'discard' ]"   # `parseObjectPart` makes both an `nkNilLit`
  objectPart "optSameInd declColonEquals (optPar COMMENT)?"
  objectPart "%else"
  # GRAMMAR.TXT: `IND{>} objectPart^+IND{=} DED / objectWhen / objectCase /
  # 'nil' / 'discard' / declColonEquals`, i.e. the five branches are simply
  # "not indented". `parseObjectPart` tests `realInd` first and then
  # `sameOrNoInd` -- a *dedent* falls through to the empty node and ends the
  # object. Reading the dedent as a branch made `type\n  T = object\n  when
  # cond:` parse the `when` as a field list.
  # `parseObjectPart` calls `rawSkipComment` after the field, which is how
  # `a: int  ## what a is` does not end the field list.

  objectDecl "object[ 'object' (NO_IND 'of' typeDesc)? trailComment? (IND{>} objectPart)? ]"
  # GRAMMAR.TXT: `objectPart` unconditionally. `parseObject` ends with
  # `if not realInd(p): result.add(emptyNode) else: parseObjectPart(p)` -- an
  # object with an empty body is the common case (`Foo[T] = object` on its own
  # line), and reading the *next type definition* at IND{=} as a field is how
  # `Bar[T] = object` after it became a field named `Bar`.

  conceptParam "('var' | 'out' | 'ptr' | 'ref' | 'static' | 'type')? symbol"
  conceptDecl """concept[ 'concept' trailComment? (NO_IND conceptParam ^* ',')?
               (NO_IND pragma)? (NO_IND 'of' typeDesc ^* ',')? trailComment?
               (IND{>} stmt)? ]"""
  # GRAMMAR.TXT: no COMMENT and no indentation guards. `parseTypeClass` skips
  # a doc comment first, then takes the parameter list, the pragma and the
  # `of` clause only while `p.tok.indent < 0` -- so
  # `concept ## doc` followed by an indented body has *no* parameters, and
  # without the guards the body's first `func` was read as a `conceptParam`.
  # The body is optional: `parseTypeClass` only errors on a missing one for a
  # *new-styled* concept without parents, and `Equatable = concept x` on its
  # own line is the old style.

  typeDef """type[ identVisDot genericParamList? pragma?
                 ('=' optInd typeDefValue)? indAndComment? ]"""
  # GRAMMAR.TXT: writes `pragma` and the `'='` as required. Both are optional
  # in `parseTypeDef` (`optPragmas`, and `if p.tok.tokType == tkEquals`).

  typeDefValue """(tupleDecl | enumDecl | objectDecl | conceptDecl)
               (^infix[ @'not' primary({pmTypeDesc}) ])?"""
  # GRAMMAR.TXT: also lists `('ref'|'ptr'|'distinct') (tupleDecl |
  # objectDecl)` here, which made the operand mandatory and refused
  # everything else -- `distinct int32` did not parse. The three are gone
  # from this rule entirely: `parseTypeDefValue` routes them to
  # `parseTypeDescKAux(.., pmTypeDef)`, which differs from the `typeDesc`
  # case in exactly two ways, both of which now live in `typeKAuxOperand`
  # and `typeDescKeyw(mode)` -- an `object`/`tuple` operand may bring a
  # *body*, and the result is fed back into the operator loop, so
  # `SomePointer = ref | ptr | pointer | proc` parses. Reaching it through
  # `primary` is what supplies that loop for free.
  typeDefValue """%else simpleExpr({-1}, {pmTypeDef})
               (comma exprEqExpr)* postExprBlocks?
               (^infix[ @'not' primary({pmTypeDesc}) ])?"""
  # GRAMMAR.TXT: `simpleExpr (exprEqExpr ^+ comma postExprBlocks?)?`, which
  # reads as though the extra parameters came *before* the commas. They come
  # after: `parseTypeDefValue` runs `while p.tok.tokType == tkComma`. Written
  # the other way the optional group is entered on FIRST(exprEqExpr) -- every
  # token an expression can start with -- so `FileHandle* = cint` swallowed
  # the next line of the type section and `= bool` swallowed the next
  # `type` keyword.

  varTupleLhs """unpacktup[ '(' optInd
               (identWithPragmaDot | varTupleLhs) (comma (identWithPragmaDot | varTupleLhs)?)*
               optPar ')' (':' optInd typeDescExpr)? ]"""
  # GRAMMAR.TXT: `identWithPragma`; `parseVarTuple` passes `allowDot=true`.
  varTuple "unpackdecl[ varTupleLhs '=' optInd expr ]"

  variable "(varTuple | declColonEqualsDot) postExprBlocks? indAndComment"
  # GRAMMAR.TXT: `colonBody?`, i.e. `colcom stmt postExprBlocks?`.
  # `parseVariable` has no `colcom`: it applies `postExprBlocks` to the value,
  # which is the rule that owns both the `:` and the `do` form. So
  # `var res = f(x) do (a: int) -> string:` parses. That leaves
  # grammar.txt's `colonBody` with no caller at all, so it is gone.
  # GRAMMAR.TXT: says `identColonEquals`, i.e. a bare `IDENT`. `parseVariable`
  # passes `{withPragma, withDot}`, so `let navigator {.importc.}: JsObject`
  # and `var a.b: T` are both legal.
  constant """(varTupleLhs | identWithPragma (colon typeDesc)?)
            '=' optInd expr postExprBlocks? indAndComment"""
  # GRAMMAR.TXT: `(varTuple / identWithPragma) (colon typeDesc)? '=' ...`.
  # `varTuple` ends in `'=' optInd expr`, so `const (a, b) = (1, 2)` wanted
  # two `=`. `parseConstant` calls `parseVarTuple`, which is `varTupleLhs`,
  # and the `(colon typeDesc)?` belongs to the other branch -- `varTupleLhs`
  # carries its own. `postExprBlocks?` is missing from grammar.txt too.

  bindStmt  "bind[ 'bind' optInd qualifiedIdent ^+ comma ]"
  mixinStmt "mixin[ 'mixin' optInd qualifiedIdent ^+ comma ]"
  pragmaStmt "pragma (NO_IND ':' trailComment? stmt)?"

  simpleStmt """(returnStmt | raiseStmt | yieldStmt | discardStmt | breakStmt
             | continueStmt | pragmaStmt | importStmt | exportStmt | fromStmt
             | includeStmt | commentStmt) trailComment?"""
  simpleStmt "%else exprStmt trailComment?"

  complexOrSimpleStmt """ifStmt | whenStmt | whileStmt | tryStmt | forStmt
                     | blockStmt | staticStmt | deferStmt | asmStmt
                     | bindStmt | mixinStmt | caseStmt"""
  complexOrSimpleStmt "except[ 'except' colcom stmt ]"
  complexOrSimpleStmt "fin[ 'finally' colcom stmt ]"
  # GRAMMAR.TXT: the `#|` comment on `complexOrSimpleStmt` lists neither
  # `caseStmt` nor the standalone `except`/`finally` blocks, though the `case`
  # branch is right there in the code. Without `caseStmt` a `case` *statement*
  # was unreachable: only `expr` offered one, and `exprStmt` goes through
  # `simpleExpr`, which does not.
  complexOrSimpleStmt "proc[ 'proc' routine ]"
  complexOrSimpleStmt "method[ 'method' routine ]"
  complexOrSimpleStmt "func[ 'func' routine ]"
  complexOrSimpleStmt "iterator[ 'iterator' routine ]"
  complexOrSimpleStmt "macro[ 'macro' routine ]"
  complexOrSimpleStmt "template[ 'template' routine ]"
  complexOrSimpleStmt "converter[ 'converter' routine ]"
  complexOrSimpleStmt "'type' section(typeDef)"
  complexOrSimpleStmt """'type' typeof[ '(' primary({pmTypeDesc}) ')' ]
                      binaryTail( simpleExpr, getPrecedence, isRightAssoc, infix,
                                  {-1}, {pmNormal} )
                      postExprBlocks?"""
  # GRAMMAR.TXT: only `'type' section(typeDef)`. `complexOrSimpleStmt`'s
  # `of tkType:` branch checks for a `(` and builds an `nkTypeOfExpr` instead,
  # then runs `parseOperators` and `postExprBlocks` over it -- which is how
  # `type(z(type(x))) is type(x)` is a statement in a concept body. The tag
  # sits *after* the `'type'` so that both entries share it as their first
  # item and the generator can factor them; written as an outer
  # `typeof[ 'type' ... ]` it would hide the keyword from the dispatch.
  # `binaryTail(...)` is `parseOperators` applied to a node that is already
  # on the buffer: `binary(...)` cannot express that, because its first
  # argument *is* the left operand.
  complexOrSimpleStmt "'const' section(constant)"
  complexOrSimpleStmt "'let' section(variable)"
  complexOrSimpleStmt "'var' section(variable)"
  complexOrSimpleStmt "'using' section(variable)"
  complexOrSimpleStmt "%else simpleStmt"
  # PRED: 'static' is both `staticStmt` and a `symbol`; parser.nim decides on
  # the token after it. See the conflict report.

  stmt "indented( complexOrSimpleStmt ((IND{=} | ';') complexOrSimpleStmt?)* )"
  # GRAMMAR.TXT: `complexOrSimpleStmt ^+ (IND{=} / ';')`, i.e. a real
  # separator. `parseStmt` re-tests the indentation *after* eating the `;` and
  # breaks on a dedent, so a trailing `;` at the end of a block is legal. A
  # separator repetition cannot express that -- its loop commits to another
  # item as soon as it sees the separator -- so the item is optional instead,
  # which also gives up the over-indentation diagnostic here. `module` keeps
  # it, and that is the one that matters for a bad dedent.
  stmt "notInd simpleStmt ^+ ';'"
