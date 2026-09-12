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

  comma     "',' COMMENT?"
  semicolon "';' COMMENT?"
  colon     "':' COMMENT?"
  colcom    "':' COMMENT?"

  # Named indentation guards. These are zero-width: they constrain the class
  # of the current token without consuming it.
  validInd    "NO_IND | IND{>}"            # parser.nim's `validInd`
  optSameInd  "NO_IND | IND{=}"            # parser.nim's `sameOrNoInd`
  notInd      "NO_IND | IND{=} | IND{<}"   # the `else` of parser.nim's `if realInd(p)`

  optInd "COMMENT? validInd"
  optPar "NO_IND | IND{>} | IND{=}"
  # GRAMMAR.TXT: says `(IND{>} | IND{=})?`, i.e. unconstrained. parser.nim's
  # `optPar` errors on a dedent, so it is an assertion, not an option.

  # `operator` and `prefixOperator` are documentation only: operator dispatch
  # happens inside `binary(...)` and in `primary`'s prefix alternative.
  operator """OPR | 'or' | 'xor' | 'and' | 'is' | 'isnot' | 'in' | 'notin'
          | 'of' | 'as' | 'from' | 'div' | 'mod' | 'shl' | 'shr' | 'not' | '..'"""
  prefixOperator "operator"
  operatorB "operator"
  # GRAMMAR.TXT: lists `operator` and `operatorB` separately with the same
  # right-hand side modulo ordering.

  symbol """quoted[ '`' (KEYW | IDENT | literal | (operator | '(' | ')' | '[' | ']'
                        | '{' | '}' | '=')+)+ '`' ]"""
  symbol "IDENT"
  symbol "'addr'"
  symbol "'type'"
  symbol "'static'"

  symbolOrKeyword "symbol"
  symbolOrKeyword "KEYW"

  # --------------------------------------------------------------- literals

  literal """INT_LIT | INT8_LIT | INT16_LIT | INT32_LIT | INT64_LIT
         | UINT_LIT | UINT8_LIT | UINT16_LIT | UINT32_LIT | UINT64_LIT
         | FLOAT_LIT | FLOAT32_LIT | FLOAT64_LIT | FLOAT128_LIT
         | STR_LIT | RSTR_LIT | TRIPLESTR_LIT
         | CHAR_LIT | CUSTOM_NUMERIC_LIT
         | 'nil'"""
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
  exprColonEqExprList "exprColonEqExpr (comma exprColonEqExpr)* comma?"

  qualifiedIdent "dot[ symbol '.' optInd symbolOrKeyword ]"
  qualifiedIdent "symbol"

  setOrTableConstr "curly[ '{' (exprColonEqExpr comma)* '}' ]"
  setOrTableConstr "tabconstr[ '{' ':' '}' ]"

  castExpr "cast[ 'cast' '[' optInd typeDesc optPar ']' '(' optInd expr optPar ')' ]"

  parKeyw """'discard' | 'include' | 'if' | 'while' | 'case' | 'try'
         | 'finally' | 'except' | 'for' | 'block' | 'const' | 'let'
         | 'when' | 'var' | 'mixin'"""

  par "stmts[ '(' optInd &parKeyw (ifExpr | complexOrSimpleStmt) ^+ ';' optPar ')' ]"
  par "stmts[ '(' optInd ';' (ifExpr | complexOrSimpleStmt) ^+ ';' optPar ')' ]"
  par "par[ '(' optInd pragmaStmt optPar ')' ]"
  par "par[ '(' optInd simpleExpr (doBlock extraPostExprBlock*) optPar ')' ]"
  par "par[ '(' optInd simpleExpr '=' expr (';' (ifExpr | complexOrSimpleStmt) ^+ ';')? optPar ')' ]"
  par "par[ '(' optInd simpleExpr ':' expr (',' exprColonEqExpr ^+ ',')? optPar ')' ]"
  par "par[ '(' optInd simpleExpr optPar ')' ]"
  # The last four share the `simpleExpr` prefix and are left-factored; the
  # first is chosen by a FIRST-set lookahead over `parKeyw`, the second by ';'.
  # GRAMMAR.TXT: does not spell out the bare `'(' simpleExpr ')'` case.

  tupleConstr  "tup[ '(' optInd (exprColonEqExpr comma?)* optPar ')' ]"
  arrayConstr  "bracket[ '[' optInd (exprColonEqExpr comma?)* optPar ']' ]"

  identOrLiteral """generalizedLit | symbol | literal
                | par | arrayConstr | setOrTableConstr | tupleConstr | castExpr"""
  # PRED: `par` vs `tupleConstr` are the same prefix `'('`; parser.nim decides
  # inside one routine. Left-factored here into `par`'s entries, with
  # `tupleConstr` reachable from the `simpleExpr comma` case.

  primarySuffix(mode) "&noSpaceBefore ^call[ '(' (exprColonEqExpr comma?)* ')' ]"
  primarySuffix(mode) """^dot[ '.' optInd symbolOrKeyword
                             ('[:' exprList ']' ('(' exprColonEqExpr ')')?)? ] generalizedLit?"""
  primarySuffix(mode) "&dotLikeOps ^dot[ DOTLIKEOP optInd symbolOrKeyword ] generalizedLit?"
  primarySuffix(mode) "&noSpaceBefore ^at[ '[' optInd exprColonEqExprList optPar ']' ]"
  primarySuffix(mode) "&noSpaceBefore ^curlyat[ '{' optInd exprColonEqExprList optPar '}' ]"
  primarySuffix(mode) "&commandStart ^cmd[ commandParam(mode)+ ]"
  # PRED: `noSpaceBefore` is `tsLeading notin tok.spacing` — a space before
  # '(' turns a call into a command. `dotLikeOps` is the `nimPreviewDotLikeOps`
  # switch plus `isDotLike`.
  # The whole loop only runs while NO_IND, or on a '.' at IND{>=}; that is a
  # property of the enclosing repetition in `simplePrimary`.

  pragma "pragmas[ '{.' optInd (exprColonEqExpr comma?)* optPar ('.}' | '}') ]":
    enter: inc p.inPragma
    leave: dec p.inPragma

  identVis "postfix[ symbol OPR ]"
  identVis "symbol"
  identVisDot "dot[ symbol '.' optInd symbolOrKeyword ] OPR?"

  identWithPragma    "pragmax[ identVis pragma ]"
  identWithPragma    "identVis"
  identWithPragmaDot "pragmax[ identVisDot pragma ]"
  identWithPragmaDot "identVisDot"

  declColonEquals """identWithPragma (comma identWithPragma)* comma?
                   (':' optInd typeDescExpr)? ('=' optInd expr)?""":
    fanOut m, p.section          # one (var|let|param|fld ...) per name
  identColonEquals """IDENT (comma IDENT)* comma?
                    (':' optInd typeDescExpr)? ('=' optInd expr)?""":
    fanOut m, p.section

  # --------------------------------------------------------------- types

  tupleTypeBracket "'[' optInd (identColonEquals (comma | semicolon)?)* optPar ']'"
  tupleType "tuple[ 'tuple' tupleTypeBracket ]"
  tupleDecl "tuple[ 'tuple' tupleTypeBracket ]"
  tupleDecl "tuple[ 'tuple' COMMENT? (indented( identColonEquals ^+ IND{=} ))? ]"

  paramList      "params[ '(' declColonEquals ^* (comma | semicolon) ')' ]"
  paramListArrow "paramList? ('->' optInd typeDesc)?"
  paramListColon "paramList? (':' optInd typeDesc)?"

  doBlock "proc[ 'do' paramListArrow pragma? colcom stmt ]"
  routineExpr "proc[ 'proc' paramListColon pragma? ('=' COMMENT? stmt)? ]"
  routineExpr "func[ 'func' paramListColon pragma? ('=' COMMENT? stmt)? ]"
  routineExpr "iterator[ 'iterator' paramListColon pragma? ('=' COMMENT? stmt)? ]"
  routineType "proc[ 'proc' paramListColon pragma? ]"
  routineType "itertype[ 'iterator' paramListColon pragma? ]"

  rawTypeDesc """(tupleType | routineType | enum[ 'enum' ] | object[ 'object' ]
              | mut[ 'var' typeDesc? ] | out[ 'out' typeDesc? ]
              | ref[ 'ref' typeDesc? ] | ptr[ 'ptr' typeDesc? ]
              | distinct[ 'distinct' typeDesc? ]) (infix[ 'not' primary ])?"""
  typeDescExpr "(routineType | simpleExpr) (infix[ 'not' primary ])?"
  # PRED: `routineType` vs `simpleExpr` both start at 'proc'/'iterator';
  # left-factored by the generator.
  typeDesc "rawTypeDesc"
  typeDesc "%else typeDescExpr"
  # PRED: 'var'/'out'/'ref'/'ptr'/'distinct'/'tuple'/'enum'/'object'/'proc'/
  # 'iterator' select rawTypeDesc; everything else typeDescExpr. 'proc' and
  # 'iterator' overlap and are left-factored.

  # --------------------------------------------------------------- primary

  forStmt "for[ 'for' ((varTuple | identWithPragma) ^+ comma) 'in' expr colcom stmt ]"
  forExpr "forStmt"

  expr "blockExpr | ifExpr | whenExpr | caseStmt | forExpr | tryExpr"
  expr "simpleExpr"

  simplePrimary "identOrLiteral primarySuffix*"
  # GRAMMAR.TXT: writes `SIGILLIKEOP? identOrLiteral ...` here, but parser.nim
  # has no such alternative: an operator always takes the prefix branch, and
  # `isSigilLike` only makes the operand bind tighter (`@x.y` is `@(x.y)`'s
  # sibling, not a separate production). Its own comment says as much, and
  # that sigils "should be removed for Nim 2.0". Modelling it as a real
  # alternative is what made SIGILLIKEOP collide with OPR in `primary`.

  # Not in doc/grammar.txt at all, though parser.nim has it.
  commandParam(mode) "&inTypeDesc simpleExpr"
  commandParam(mode) "&notFirstParam exprEqExpr"
  commandParam(mode) "%else expr (doBlock extraPostExprBlock*)?"

  commandStart """&('`' | IDENT | literal | 'cast' | 'addr' | 'type' | 'var' | 'out'
                | 'static' | 'enum' | 'tuple' | 'object' | 'proc')"""
  # This is `parser.nim`'s `isExprStart`, written out by hand there. The
  # generator checks it against FIRST(commandParam).

  primary(mode) "simplePrimary (commandStart cmd[ expr (doBlock extraPostExprBlock*)? ])?"
  primary(mode) "&isSigilLike prefix[ prefixOperator simplePrimary ]"
  primary(mode) "&isUnary prefix[ operatorB primary(mode) ]"
  primary(mode) "routineExpr"
  primary(mode) "&inTypeDesc rawTypeDesc"
  primary(mode) "prefix[ prefixOperator primary(mode) ]"
  # PRED: `operatorB primary` vs `prefixOperator primary` are distinguished by
  # `isUnary` (spacing) in parser.nim; the two entries are otherwise identical.

  simpleExpr(limit, mode) "binary( primaryPragma(mode), getPrecedence, isRightAssoc, infix, limit )"
  primaryPragma(mode) "primary(mode) (pragmax[ pragma ])?"

  # --------------------------------------------------------- post-expr blocks

  extraPostExprBlock "IND{=} doBlock"
  extraPostExprBlock "of[ IND{=} 'of' exprList ':' stmt ]"
  extraPostExprBlock "elif[ IND{=} 'elif' expr ':' stmt ]"
  extraPostExprBlock "except[ IND{=} 'except' optionalExprList ':' stmt ]"
  extraPostExprBlock "fin[ IND{=} 'finally' ':' stmt ]"
  extraPostExprBlock "else[ IND{=} 'else' ':' stmt ]"

  postExprBlocks "doBlock extraPostExprBlock*"
  postExprBlocks "':' (extraPostExprBlock | stmt) extraPostExprBlock*"
  # GRAMMAR.TXT: parser.nim additionally requires NO_IND before the whole
  # thing (`if p.tok.indent >= 0: return`).

  # --------------------------------------------------------------- stmts

  exprStmt "asgn[ simpleExpr '=' optInd expr postExprBlocks? ]"
  exprStmt "cmd[ simpleExpr (exprEqExpr ^+ comma) postExprBlocks? ]"
  # GRAMMAR.TXT: says `simplePrimary` here; parser.nim calls
  # `simpleExpr(p, pmTrySimple)` for all three and decides afterwards, which is
  # also what makes the three left-factor.
  exprStmt "simpleExpr postExprBlocks?"
  # The three share the `simpleExpr`/`simplePrimary` prefix; left-factored,
  # with the `asgn`/`cmd` tags inserted at the mark once the alternative is
  # known. This is the motivating case for retroactive wrapping.

  importStmt "import[ 'import' optInd expr (comma expr)* ]"
  importStmt "importexcept[ 'import' optInd expr 'except' optInd (expr ^+ comma) ]"
  exportStmt "export[ 'export' optInd expr (comma expr)* ]"
  exportStmt "exportexcept[ 'export' optInd expr 'except' optInd (expr ^+ comma) ]"
  includeStmt "include[ 'include' optInd expr ^+ comma ]"
  fromStmt "fromimport[ 'from' expr 'import' optInd expr (comma expr)* ]"

  returnStmt   "ret[ 'return' optInd expr? ]"
  raiseStmt    "raise[ 'raise' optInd expr? ]"
  yieldStmt    "yld[ 'yield' optInd expr? ]"
  discardStmt  "discard[ 'discard' optInd expr? ]"
  breakStmt    "break[ 'break' optInd expr? ]"
  continueStmt "continue[ 'continue' optInd expr? ]"

  condStmt """elif[ expr colcom stmt ] COMMENT?
            (elif[ IND{=} 'elif' expr colcom stmt ])*
            (else[ IND{=} 'else' colcom stmt ])?"""
  ifStmt   "if[ 'if' condStmt ]"
  whenStmt "when[ 'when' condStmt ]"

  condExpr """elif[ expr colcom stmt ] optInd
            (elif[ 'elif' expr colcom stmt ] optInd)*
            else[ 'else' colcom stmt ]"""
  ifExpr   "if[ 'if' condExpr ]"
  whenExpr "when[ 'when' condExpr ]"

  whileStmt "while[ 'while' expr colcom stmt ]"

  ofBranch   "of[ 'of' exprList colcom stmt ]"
  ofBranches """ofBranch (IND{=} ofBranch)*
              (elif[ IND{=} 'elif' expr colcom stmt ])*
              (else[ IND{=} 'else' colcom stmt ])?"""
  caseStmt "case[ 'case' expr ':'? COMMENT? (indented( ofBranches ) | IND{=} ofBranches) ]"

  tryStmt """try[ 'try' colcom stmt &(optSameInd ('except' | 'finally'))
           (except[ optSameInd 'except' optionalExprList colcom stmt ])*
           (fin[ optSameInd 'finally' colcom stmt ])? ]"""
  tryExpr """try[ 'try' colcom stmt &(optInd ('except' | 'finally'))
           (except[ optInd 'except' optionalExprList colcom stmt ])*
           (fin[ optInd 'finally' colcom stmt ])? ]"""

  blockStmt   "block[ 'block' symbol? colcom stmt ]"
  blockExpr   "block[ 'block' symbol? colcom stmt ]"
  staticStmt  "staticstmt[ 'static' colcom stmt ]"
  deferStmt   "defer[ 'defer' colcom stmt ]"
  asmStmt     "asm[ 'asm' pragma? (STR_LIT | RSTR_LIT | TRIPLESTR_LIT) ]"

  genericParam "typevar[ symbol (comma symbol)* (colon expr)? ('=' optInd expr)? ]"
  genericParamList "typevars[ '[' optInd genericParam ^* (comma | semicolon) optPar ']' ]"

  pattern "'{' stmt '}'"
  indAndComment "(IND{>} COMMENT)? | COMMENT?"

  routine """optInd identVis pattern? genericParamList? paramListColon pragma?
           ('=' COMMENT? stmt)? indAndComment"""

  commentStmt "comment[ COMMENT ]"

  section(R: rule) "notInd COMMENT? R"
  section(R: rule) "indented( (R | COMMENT) ^+ IND{=} )"

  enumDecl "enum[ 'enum' optInd (symbol pragma? optInd ('=' optInd expr COMMENT?)? comma?)+ ]"

  objectWhen """when[ 'when' elif[ expr colcom objectPart ] COMMENT?
              (elif[ 'elif' expr colcom objectPart ] COMMENT?)*
              (else[ 'else' colcom objectPart ] COMMENT?)? ]"""
  objectBranch "of[ 'of' exprList colcom objectPart ]"
  objectBranches """objectBranch (IND{=} objectBranch)*
                  (elif[ IND{=} 'elif' expr colcom objectPart ])*
                  (else[ IND{=} 'else' colcom objectPart ])?"""
  objectCase """case[ 'case' (declColonEquals | pragma)? ':'? COMMENT?
              (indented( objectBranches ) | IND{=} objectBranches) ]"""

  objectPart "stmts[ indented( objectPart ^+ IND{=} ) ]"
  objectPart "notInd objectWhen"
  objectPart "notInd objectCase"
  objectPart "notInd 'nil'"
  objectPart "notInd 'discard'"
  objectPart "notInd declColonEquals"

  objectDecl "object[ 'object' ('of' typeDesc)? COMMENT? objectPart ]"

  conceptParam "('var' | 'out' | 'ptr' | 'ref' | 'static' | 'type')? symbol"
  conceptDecl """concept[ 'concept' (conceptParam ^* ',' pragma?)? ('of' typeDesc ^* ',')?
               &IND{>} stmt ]"""

  typeDef "type[ identVisDot genericParamList? pragma '=' optInd typeDefValue indAndComment? ]"

  typeDefValue """(tupleDecl | enumDecl | objectDecl | conceptDecl
               | ref[ 'ref' (tupleDecl | objectDecl) ]
               | ptr[ 'ptr' (tupleDecl | objectDecl) ]
               | distinct[ 'distinct' (tupleDecl | objectDecl) ])
               (infix[ 'not' primary ])?"""
  typeDefValue "%else simpleExpr (exprEqExpr ^+ comma postExprBlocks?)? (infix[ 'not' primary ])?"

  varTupleLhs """unpacktup[ '(' optInd (identWithPragma | varTupleLhs) ^+ comma optPar ')'
               (':' optInd typeDescExpr)? ]"""
  varTuple "unpackdecl[ varTupleLhs '=' optInd expr ]"

  colonBody "colcom stmt postExprBlocks?"
  variable "(varTuple | identColonEquals) colonBody? indAndComment"
  constant "(varTuple | identWithPragma) (colon typeDesc)? '=' optInd expr indAndComment"

  bindStmt  "bind[ 'bind' optInd qualifiedIdent ^+ comma ]"
  mixinStmt "mixin[ 'mixin' optInd qualifiedIdent ^+ comma ]"
  pragmaStmt "pragma (':' COMMENT? stmt)?"

  simpleStmt """(returnStmt | raiseStmt | yieldStmt | discardStmt | breakStmt
             | continueStmt | pragmaStmt | importStmt | exportStmt | fromStmt
             | includeStmt | commentStmt) COMMENT?"""
  simpleStmt "%else exprStmt COMMENT?"

  complexOrSimpleStmt """ifStmt | whenStmt | whileStmt | tryStmt | forStmt
                     | blockStmt | staticStmt | deferStmt | asmStmt
                     | bindStmt | mixinStmt"""
  complexOrSimpleStmt "proc[ 'proc' routine ]"
  complexOrSimpleStmt "method[ 'method' routine ]"
  complexOrSimpleStmt "func[ 'func' routine ]"
  complexOrSimpleStmt "iterator[ 'iterator' routine ]"
  complexOrSimpleStmt "macro[ 'macro' routine ]"
  complexOrSimpleStmt "template[ 'template' routine ]"
  complexOrSimpleStmt "converter[ 'converter' routine ]"
  complexOrSimpleStmt "'type' section(typeDef)"
  complexOrSimpleStmt "'const' section(constant)"
  complexOrSimpleStmt "'let' section(variable)"
  complexOrSimpleStmt "'var' section(variable)"
  complexOrSimpleStmt "'using' section(variable)"
  complexOrSimpleStmt "%else simpleStmt"
  # PRED: 'static' is both `staticStmt` and a `symbol`; parser.nim decides on
  # the token after it. See the conflict report.

  stmt "indented( complexOrSimpleStmt ^+ (IND{=} | ';') )"
  stmt "notInd simpleStmt ^+ ';'"
