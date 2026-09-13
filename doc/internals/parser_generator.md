# An LL(1) parser generator for Nim's syntax

Status: design, agreed 2026-09-12. Implementation in `src/nifler2/`.

## What this replaces

`nifler` parses Nim and emits NIF. It does so by linking Nim's own
`compiler/parser.nim`, building `PNode` trees and walking them
(`src/nifler/bridge.nim`). That has three costs:

* the Nimony toolchain carries a dependency on the Nim 2 compiler sources for
  its front end;
* every token is turned into a `PNode` only to be walked once and thrown away;
* **the grammar is not written down anywhere that is checked.** `parser.nim`
  carries it in `#|` comments, `isMainModule` extracts them into
  `doc/grammar.txt`, and `tools/grammar_nanny.nim` checks only that every
  non-terminal that is used is also declared. Nothing verifies that the
  comments describe the code, and nothing verifies that the grammar is
  unambiguous.

The replacement is a Nimony program whose parser is *generated* from a grammar
that lives in the program, with the actions beside it. The grammar is the
source of truth, the conflict check is a compile error, and `doc/grammar.txt`
becomes a generated constant.

The design goal is not "a parser generator". It is: **keep every degree of
freedom a hand-written recursive-descent parser has, and pay for it by
declaring each one.**

## Surface syntax

```nim
template grammar*(rules: varargs[untyped]) {.plugin: "deps/parsegen".}

grammar:
  RuleName "<production>":
    <actions>                          # optional; most rules have none

  RuleName(param: T) "<production>"    # parameterized rule
```

`RuleName "..."` is an ordinary command call and `RuleName"..."` a generalized
string literal; both are accepted, they differ only in the tag nifler gives
them. The trailing `:` block is `postExprBlocks`, so all of this is valid Nim
and reaches the plugin as untyped NIF.

Putting the production in a **string literal** is deliberate. An EBNF embedded
in Nim expressions fights Nim's own syntax at every turn — implicit
concatenation has no operator, `?`/`*`/`+` are not postfix, and unary-vs-binary
spacing rules would bite us in the very file that is supposed to formalize
them. Inside a string we own the notation completely.

**Repeating a rule name adds an alternative.** Top-level `|` does not exist;
each production gets its own entry. Alternatives are tried in written order,
and the checker requires them to be LL(1)-disjoint unless separated by a
declared predicate. Rendering the documentation is then close to the identity
function: join the strings of same-named entries with `|`.

## The grammar mini-language

| Notation | Meaning |
|---|---|
| `'if'` `'('` `'.}'` | terminal: match, emit nothing |
| `IDENT` `INT_LIT` `COMMENT` | token class: match, emit a leaf |
| `simpleExpr` | non-terminal (lowerCamel) |
| juxtaposition | sequence |
| `a \| b`, `( ... )` | alternation and grouping *within* one entry |
| `x?` `x*` `x+` | option and repetition |
| `x ^* sep` `x ^+ sep` | separated repetition (as in `doc/grammar.txt`) |
| `IND{=}` `IND{>}` `IND{<}` `NO_IND` | zero-width indentation guards |
| `indented( ... )` | `withInd`: assert `IND{>}`, rebind `currInd`, restore |
| `&name` `!name` | semantic predicate; `name` is a proc `(p) -> bool` |
| `&name(args)` | ditto, with the rule's parameters passed along: `&inTypeDesc(mode)` is `inTypeDesc(p, mode)` |
| `&( a \| b )` | FIRST-set lookahead, consumes nothing |
| `tag[ ... ]` | emit `(tag …)` around what the group produces |
| `^tag[ ... ]` | ditto, anchored at the enclosing repetition's subject |
| `binary(primary, prec, rightAssoc, tag)` | precedence climbing |
| `la2( ... )` | explicit two-token lookahead; every use is reported |
| `%else` | marks the rule's fallback alternative (`parser.nim`'s `case … else:`) |

Predicates are bare names rather than embedded Nim expressions, so
`isDotLike`, `noSpaceBefore` and friends stay real procs that Nimony
type-checks.

`&name` means two different things depending on what `name` is, and the
generator decides once the whole rule set is known: if `name` is a declared
rule it is a **FIRST-set lookahead**, which is decidable from the current token
and therefore checkable; otherwise it is an opaque **semantic predicate**,
which is not. Both are reported, but only the second is a real escape hatch.

A production that does not fit on one line must use a triple-quoted string —
a Nim string literal cannot span lines. Nifler delivers those as `(suf <str>
"T")` rather than a bare `StrLit`, so the plugin accepts both.

## Tags: the whole action language, most of the time

Terminals emit nothing, token classes emit a leaf, non-terminals emit their own
subtree. So a tagged group is usually the entire mapping from syntax to NIF and
the rule needs no action block at all:

```nim
  ifStmt "if[ 'if' elif[expr colcom stmt]
              (IND{=} 'elif' elif[expr colcom stmt])*
              (IND{=} 'else' else[colcom stmt])? ]"
```

Tags and non-terminals are distinguished by the bracket: `name[...]` is a NIF
tag, bare `name` is a rule.

### Two anchors

`tag[...]` inserts its open token at the position where **the group started
matching**. Because NIF is a token buffer with an explicit `ParRi`, "insert an
open token at a saved position" is `nifcore.insert` and costs a shift of the
subtree just parsed — which is why the generator can left-factor freely and
still produce the tree the unfactored grammar describes.

`^tag[...]` anchors instead at the value the **caller** has accumulated so far.
It is for exactly one shape — a rule used as the body of a postfix loop in
another rule — and it is the one piece of notation with no counterpart in
ordinary EBNF:

```nim
  primary        "identOrLiteral primarySuffix*"
  primarySuffix  "&noSpaceBefore ^call[ '(' (exprColonEqExpr ','?)* ')' ]"
  primarySuffix  "^dot[ '.' optInd symbolOrKeyword ] generalizedLit?"
  primarySuffix  "&noSpaceBefore ^at[ '[' optInd exprColonEqExprList optPar ']' ]"
  primarySuffix  "&isExprStart ^cmd[ cmdArg+ ]"
```

Between them the two anchors cover every retroactive wrap in `parser.nim`, and
the split is sharper than it first looks: a tag that spans its whole
alternative — `asgn[ simpleExpr '=' … ]`, `prefix[ operatorB primary ]`, even
`(infix[ 'not' primary ])?` trailing an alternative — already wraps from the
alternative's own mark and must **not** use `^`. In the whole Nim grammar
exactly one rule needs the inherited anchor, and the generator threads it as
an explicit `anchor: int` parameter:

```nim
proc pPrimary*(p: var Parser) =
  let m0 = mark(p)
  pIdentOrLiteral p
  while canPrimarySuffix(p):
    pPrimarySuffix(p, m0)      # every iteration wraps from the same mark

proc pPrimarySuffix*(p: var Parser; anchor: int) = ...
```

Which rules need it is decided statically: a rule needs an anchor parameter
when it contains a `^tag` that is not inside a repetition of its own.

A consequence worth stating, because the grammar leans on it: a tag written
**mid-sequence** wraps from the alternative's mark, i.e. over everything the
alternative has already parsed. That is what `parsePar`'s retagging needs —

```nim
  par "tup[ '(' optInd simpleExpr (kv[ ':' expr ])+ (comma exprColonEqExpr?)* … ]"
  par "par[ '(' optInd simpleExpr asgn[ '=' expr ] optPar ')' ]"
```

— where `kv` and `asgn` have to take the `simpleExpr` in front of them, and
writing them as an outer `par[ asgn[ … ] ]` instead would hide `simpleExpr`
behind `asgn` and defeat the left-factoring that makes the six `'('`
alternatives one dispatch. The same trick gives
`complexOrSimpleStmt`'s `'type' typeof[ '(' primary ')' ]` a shared `'type'`
prefix with `'type' section(typeDef)`.

The body of an option or a repetition gets a mark of its own, so a tag inside
one wraps only what the body parses: `(else[ 'else' colcom stmt ])?` is the
`else` branch and nothing else. A tag in such a body that is meant to reach
back over what the enclosing sequence already built says so with `^`, which
there resolves to the enclosing mark — `(^infix[ @'not' primary ])?`,
`(^kv[ ':' expr ])+`, `(^pragmax[ pragma ])?`. (Until this was fixed an option
reused the enclosing mark, and `if c: 1 else: 2` came out as
`(if (else (elif c 1) 2))`; `try`'s `finally` swallowed its `except`s the same
way.)

### `@'…'`: a terminal that is content

A quoted terminal is punctuation: it is consumed and leaves nothing behind.
`@'not'` is consumed *and* emitted as a leaf, which is what a keyword
operator needs — `not x` is `(prefix not x)`, and `operator`'s `'not'` used to
yield `(prefix x)`. As the first item of an anchored tag the leaf goes in at
the anchor rather than at the end, the way `binary(...)` places an infix
operator, so `^infix[ @'not' primary ]` builds `(infix not a b)` although `a`
was parsed first.

### `withInd(...)`

`indented( X )` is `parser.nim`'s `withInd` *plus* the `realInd` assertion that
usually guards it. `semiStmtList` is the one place with no such guard: the
indentation simply becomes the current token's, whatever it is, including -1
for a token that is not first on its line. `withInd( X )` is that, and it is
why

```nim
const NR_gettid = (
  when defined(amd64): clong(186)
  elif defined(i386): clong(224)
  else: clong(178))
```

measures its `elif` against the `when` rather than against the enclosing
block, while `(if a: 1 else: 2)` on one line still works. For FIRST the
outward map is the inverse of `indented`'s: a token that is `IND{=}` inside
could have been anything outside.

### `binaryTail(...)`

`binary(operand, …)`'s first argument *is* the left operand, and its recursion
goes back into the rule it appears in. `parser.nim` also calls
`parseOperators` on a node that is already built, by a different rule:

```nim
  complexOrSimpleStmt """'type' typeof[ '(' primary({pmTypeDesc}) ')' ]
                      binaryTail( simpleExpr, getPrecedence, isRightAssoc,
                                  infix, {-1}, {pmNormal} )
                      postExprBlocks?"""
```

`binaryTail` is the loop without the head: it wraps from the enclosing mark and
names the rule its right operand comes from, passing `prec + assoc` as that
rule's first (limit) argument. It matches nothing on its own, so its FIRST set
is empty and it is nullable. `type(z(type(x))) is type(x)` — a statement in a
concept body — is what needs it.

## Indentation is part of the LL(1) key

Nim's indentation cannot be a token-stream property: `INDENT`/`DEDENT` tokens
are wrong because indentation is ignored inside `(...)`, and because
"continues the current line" is a first-class state. Keep Nim's model — each
token carries `indent` (its column if it is the first token on its line, else
`-1`) and the parser carries a `currInd` register — and lift the *relation*
into the grammar.

Relative to `currInd`, the current token is in exactly one of four classes:

```
NO_IND    indent < 0                 continues the current line
IND{<}    0 <= indent < currInd      dedent
IND{=}    indent == currInd          same level
IND{>}    indent > currInd           indent
```

**FIRST and FOLLOW are therefore sets of `(TokenKind, IndentClass)` pairs**,
not sets of token kinds. This is the one decision that makes the rest work: it
is what lets the checker prove that

```
  postExprBlocks ... (IND{=} 'of' exprList ':' stmt | ...)
```

is unambiguous against "the enclosing statement list continues" — a fact that
today is true only because a human asserted it. The generated code is a `case`
on the kind with an inner comparison on the class; both are integer tests.

`indented(x)` is `withInd`. Across its boundary the checker re-maps inherited
FOLLOW conservatively: outer `IND{=}` and `IND{<}` become inner `IND{<}`,
`NO_IND` stays `NO_IND`, and outer `IND{>}` is unconstrained inside. That is
sound because the inner `currInd` is strictly greater than the outer one.
`doc/grammar.txt`'s `DED` disappears into this combinator.

### Guards export their constraint

A rule whose body is nothing but indentation guards — `optInd`, `optPar`,
`validInd`, `notInd` — matches no token at all. Its whole effect is to
constrain the *next* token, so the analysis has to carry that constraint across
the call:

```
  notInd "NO_IND | IND{=} | IND{<}"      # the `else` of `if realInd(p)`
  section(R: rule) "notInd COMMENT? R"
```

Every rule therefore exports a **leading mask** — the indentation classes it
still permits — alongside its FIRST set, computed in the same fixed point.
Without it the constraint is silently dropped at every call site and the
checker reports conflicts that do not exist; with it, six of the seven
conflicts left in the first full run disappeared.

The mask also persists across nullable items in a sequence. A guard constrains
whichever token turns out to be first, and every candidate for that token comes
from what remains, so `notInd COMMENT? R` restricts `FIRST(COMMENT) ∪ FIRST(R)`
and not just `FIRST(COMMENT)`.

## Left factoring replaces backtracking

A hand-written parser gets ordered choice for free: it parses `simpleExpr`,
looks at what comes next, and decides in hindsight what it just read. The
generator gets the same effect from automatic left factoring across
same-named entries plus retroactive wrapping, so the grammar can be *written*
in the natural unfactored form:

```nim
  exprStmt "asgn[ simpleExpr '=' optInd expr postExprBlocks? ]"
  exprStmt "cmd[ simpleExpr cmdArg+ postExprBlocks? ]"
  exprStmt "simpleExpr postExprBlocks?"
```

The generator factors `simpleExpr`, marks the buffer, dispatches on one token,
and inserts the `asgn` or `cmd` open token at the mark. The rendered grammar
shows the three alternatives unfactored, which reads better than the factored
form and matches how `doc/grammar.txt` reads today.

A dividend: the `cmd` alternative is selected by `FIRST(cmdArg)`, which *is*
`parser.nim`'s hand-maintained `isExprStart` token set — a set that has to be
kept in sync with `primary` by hand today. Here it is derived, and
`&isExprStart` can be checked against it. The same goes for `&parKeyw`, which
is `FIRST(complexOrSimpleStmt)` restricted to keywords.

## The three escape hatches

Everything a hand-written parser does that LL(1) cannot, made explicit so the
checker keeps ownership of the rest.

**1. Operator precedence.** Nim computes precedence from the operator's
spelling at run time, so no static table exists and unrolling the eleven levels
into rules would describe a parser we do not have.
`binary(primary, getPrecedence, isRightAssoc, infix)` emits the standard
precedence-climbing loop; the checker treats it as a black box with
`FIRST = FIRST(primary)`. `OP0`..`OP10` leave the notation entirely.

**2. Semantic predicates.** `&noSpaceBefore`, `&isDotLike`, `&isUnary` — the
handful of decisions that depend on spacing or on an operator's spelling.
Within one lookahead class, guarded alternatives are tried in order and **at
most one unguarded fallback** is allowed. Every pair of alternatives separated
only by a predicate is listed in the generator's report. That list is the
honest inventory of where Nim's syntax is not decidable from one token, and it
is expected to stay short.

**3. `la2(...)`.** Explicit two-token lookahead, for whatever is left. Every
use is reported. If the list grows past a handful, the grammar is wrong.

And one thing that is not an escape hatch but must still be declared:
**`%else`**. A rule like `complexOrSimpleStmt` dispatches on a keyword and
falls back to `exprStmt` for everything else — and `exprStmt`'s FIRST contains
those keywords too, because `proc`, `var`, `static` and friends can also start
an expression. `parser.nim` writes that as `case tok … else:`; the notation
writes it as a `%else` alternative, which is skipped when any other alternative
claims the token. Eleven rules need one. Marking them makes the list
countable instead of implicit in the order of a `case`.

Ordered choice (`/` in `doc/grammar.txt`) is deliberately **not** in the
notation: overlapping alternatives must be resolved by factoring or by a
declared predicate, never by silence.

## Action blocks

A bare body runs after the alternative has matched, with `m` (the alternative's
mark) and `p` in scope; the output buffer is reached through `p`, which is what
every other generated call uses. `enter:`/`leave:` wrap the match — both are
valid Nim, `enter: stmt` being an ordinary call with a colon block. `leave:`
runs after the items and before the tag's `wrap`, so it brackets the *match*;
the bare body runs last, once the node it inspects exists.

```nim
  pragma "'{.' optInd (exprColonEqExpr ','?)* optPar ('.}' | '}')":
    enter: inc p.inPragma
    leave: dec p.inPragma

  namedParams "call[ '(' (exprColonEqExpr ','?)* ')' ]":
    if p.arity(m) == 2 and p.secondChild(m).isKv:
      p.setTag m, OconstrL          # nkCall -> nkObjConstr, post hoc
```

The state a predicate reads is what these are for. `parser.nim` threads
`commandParam`'s `isFirstParam` as a `var bool` through `commandExpr`'s loop;
there is no notation for a loop-local variable, so it lives on the parser —
`primarySuffix`'s `cmd` alternative sets it on `enter:` and every alternative
of `commandParam` clears it on `leave:`, which is what `commandParam` does on
all three of its paths.

"Parse, then inspect the buffer at `m`, then retag" replaces most of
`parser.nim`'s mid-rule fiddling, which is why there is no notation for
interleaved actions. None is added until a rule demands one.

## Parameterized and higher-order rules

Rule parameters are ordinary Nim values threaded into the generated procs and
usable in predicates (`primary(mode: PrimaryMode)`, `simpleExpr(limit: int)`).
A parameter may also be a rule, which the generator monomorphizes per call
site — this is `doc/grammar.txt`'s `section(RULE)`:

```nim
  section(R: rule) "COMMENT? R | indented( (R | COMMENT) ^+ IND{=} )"
  constStmt "const[ 'const' section(constDecl) ]"
```

A rule-valued parameter has no FIRST set of its own, so it is monomorphized
before analysis: one specialization per distinct argument, with the parameter
substituted into a copy of the rule's productions. `section(R: rule)` becomes
`section_typeDef`, `section_constant` and `section_variable`, and everything
downstream — FIRST, FOLLOW, conflicts, emission — sees ordinary rules.

## Terminals

Terminals are assumed to exist; the generated parser's module does
`import nimlexer` and that is not the grammar's business. The mapping is
hardcoded in `src/nifler2/deps/terminals.nim`, taken from `compiler/lexer.nim`.

For keywords no table is needed: Nim's `TokType` enum names every keyword
`tk` & `capitalize(spelling)`, so only the keyword *set* is listed. The
punctuation is the irregular part and is seventeen rows:

```
(  tkParLe        )  tkParRi        [  tkBracketLe   ]  tkBracketRi
{  tkCurlyLe      }  tkCurlyRi      [. tkBracketDotLe .] tkBracketDotRi
{. tkCurlyDotLe   .} tkCurlyDotRi   (. tkParDotLe    .) tkParDotRi
,  tkComma        ;  tkSemiColon    :  tkColon       :: tkColonColon
=  tkEquals       .  tkDot          .. tkDotDot      [: tkBracketLeColon
`  tkAccent
```

A quoted terminal in neither table is matched as `tkOpr` with that exact
spelling. Today that covers exactly `'->'`, and it means a spelling-specific
operator needs no table edit.

Token classes map by a small table: `IDENT`→`tkSymbol`, `COMMENT`→`tkComment`,
`KEYW`→the range `tkAddr..tkYield`, `OPR`→`tkOpr`,
`DOTLIKEOP`→`tkOpr`+`isDotLike`, `SIGILLIKEOP`→`tkOpr`+`isSigilLike`, and one
row per literal kind. The two predicated classes remain valid LL(1) keys
because the predicate depends only on the current token; the checker is told
that `DOTLIKEOP ⊂ OPR` so it can flag an unordered choice between them.

## Comments

`COMMENT?` as written in `doc/grammar.txt` is never "a comment may appear
here". It is `skipComment`, which is `if p.tok.indent < 0: rawSkipComment` —
*a trailing comment on the line just parsed*. A comment that starts its own
line is a statement, or a field's doc comment, or nothing. The grammar spells
the two forms `parser.nim` has:

```nim
  trailComment "NO_IND COMMENT"       # parser.nim's `skipComment`
  flexComment  "validInd COMMENT"     # parser.nim's `flexComment`
```

Getting this wrong is not a tag mismatch but a parse failure, and it goes both
ways: a `##` block that is the entire body of a proc was consumed as the
*routine's* comment, leaving `stmt` with a dedent; and once `COMMENT?` was
tightened, an `object`'s or `enum`'s own doc comment on the line after the
keyword stopped being absorbed by accident and had to be written where
`parser.nim` actually reads it — inside the field list, at the field
indentation, because `parseObjectPart` calls `rawSkipComment` *inside* its
`withInd`.

`COMMENT` stays in the productions — the grammar should describe where a
comment may legally appear even when the tool does not act on it — and the
generator erases it before analysis in phase 1:

* `COMMENT?` becomes ε; a bare `COMMENT` alternative drops out of its choice
  (only `section`'s `(R | COMMENT)^+` today).
* The erasure is sound only if the lexer never emits `tkComment`, so the two
  are paired and the generator asserts it.
* A group that erases to nothing becomes ε. If that makes a `*`/`+` body
  nullable it is an error, not a silent infinite loop. Nothing in today's
  grammar trips this.
* Documentation renders from the untouched source strings, so the `COMMENT?`s
  appear in `grammar.txt` regardless. Phase 2 is "stop erasing" and needs no
  grammar edit.

`nifler --docs` needs `##` comments attached to declarations, and that
attachment is not really syntactic — `parser.nim` has `indAndComment` and
`flexComment` purely for it. The lexer therefore keeps the last-seen doc
comment in a side channel and the `routine`/`type`/`field` actions pick it up.
`--docs` then works in phase 1 and `COMMENT` can stay documentary
indefinitely.

## What the generator rejects

* an LL(1) conflict between two alternatives of a rule, reported as the rule
  name, the offending `(kind, indentClass)` pair and both productions;
* left recursion, with a pointer to `binary(...)` or `^tag[...]`;
* a nullable `*`/`+` body — this is what all of `parser.nim`'s `hasProgress`
  bookkeeping guards against, and it disappears;
* an unguarded alternative that is not last among overlapping ones;
* a rule that is declared and never used, or used and never declared (what
  `grammar_nanny` does today);
* a call that does not pass a parameterized rule's arguments. This is not
  pedantry: `simplePrimary "identOrLiteral primarySuffix*"` read perfectly
  well and generated a `pPrimarySuffix` call with `mode` silently dropped, so
  every `mode`-dependent decision in the rule was made on nothing. The check
  found sixteen such calls in a grammar that reported zero conflicts;
* a rule parameter without a type, which emitted
  `proc pPrimary*(p: var Parser; mode)` — not Nim.

## Generated output

* one proc per rule, mutually recursive, forward-declared; dispatch is a `case`
  on the token kind, so the emitted code is ordinary readable Nimony that can
  be stepped through — no tables, no interpreter;
* `const grammarText*`, the rendered EBNF, which `nifler grammar` prints and a
  test compares against `doc/grammar.txt`;
* a report listing every predicate-separated choice and every `la2`.

## The lexer half

`src/nifler2/nimlexer.nim`. It produces the `Token` the generated parser reads:
a `TokKind` with `compiler/lexer.nim`'s spellings (which the grammar's terminal
table already assumes), the decoded literal text, `indent` — the column when
the token is first on its line and -1 otherwise, the single input the parser's
`IndClass` is computed from — and the leading/trailing/eof `spacing` that
`f(x)` vs `f (x)` and `-x` vs `a - x` turn on. `getPrecedence`, `isRightAssoc`,
`isUnary`, `isDotLike` and `isSigilLike` live there too, because they are
lexer-level in Nim and the grammar's `binary(...)` takes them as parameters.

Hand-written, because a DFA has no business doing them: the indentation and
spacing bookkeeping, comments (including nested `#[ ]#` and the rule that
consecutive `##` lines are one token), string and character literals, and the
punctuation whose meaning depends on the character *after* it — `(.` is one
token but `(..` is two, so maximal munch gives the wrong answer.

Generated by `lex`: keywords and identifiers, every numeric literal form, and
the type suffix behind a number. Numbers are where the automaton earns its
keep; `getNumber` in Nim's lexer is a hundred lines of hand-rolled state, most
of it spelling out where `_` may appear.

* **Keywords are case-sensitive**, like every other identifier in Nimony:
  `proc` is the keyword, `pRoC` and `p_roc` are identifiers. Nim's keywords
  are style-insensitive; nobody relies on that, and it would have made a
  keyword a family of spellings instead of one `of "proc":` branch.
  `.feature: "ignoreStyle"` does not extend to keywords.
* **The numeric suffix is a second automaton**, run where the first one
  stopped, as `getNumber` does. One automaton for both cannot work: maximal
  munch sees `0x1f32` as `0x1` with the suffix `f32` just as readily as the
  hex literal it is, and the two are the same length.

Both used to be hand-written, because `lex` capped every automaton it built at
255 states; master removed that ceiling (#2514).

### The differential test

`nimlexer` is checked token by token against the lexer it replaces.
`src/nifler2/tools/refdump.nim` links Nim 2's `compiler/lexer.nim` and
`src/nifler2/tools/nimlexdump.nim` is its `nimlexer` counterpart; both print
the same one-line-per-token format, and `src/nifler2/tools/lexdiff.sh` requires
the two to be byte-identical over whole directories.

    nim c -o:bin/refdump src/nifler2/tools/refdump.nim
    bin/nimony c src/nifler2/tools/nimlexdump.nim     # then copy to bin/
    src/nifler2/tools/lexdiff.sh lib src tests/nimony

Identifiers are compared after `nimIdentNormalize` and keywords by kind alone,
because Nim's identifier cache cannot report the spelling it saw: `Foo` and
`foO` share one `PIdent`. Kinds, literal text, `indent` and `spacing` are
compared exactly. Three of Nim's tests differ on purpose, because they spell a
keyword style-insensitively (`not_in`, `tyPE`): `tests/sets/tnewsets.nim`,
`tests/tools/tlinter.nim`, `tests/tools/tlinter_warnings.nim`.

`src/nifler2/tests/tnimlexer.nim` is the small half — 28 constructs, each
expectation produced by `refdump` rather than guessed (except the one for
case-sensitive keywords, where Nimony and Nim disagree by design), so a failure
names the construct instead of the file.

The sweep is what found the parts no amount of reading `lexer.nim` would have:
a UTF-8 BOM, `tkIntLit`'s promotion to `tkInt64Lit` (which makes the token
*kind* depend on the integer's value) and the fact that an out-of-range literal
keeps `tkIntLit` because Nim abandons the conversion, that `getNumber`
lower-cases the base prefix so `0X10` and `0x10` are one token, and that `0x`
with no digits is one bad number rather than `0` followed by `x`.

## Result of the first full run

`src/nifler2/nimgrammar.nim` is `doc/grammar.txt` transcribed into the notation:
186 productions, 118 rules. `src/nifler2/tools/gramcheck.nim` is the analysis
half of the future plugin behind a throwaway text front end, so it can be run
today:

```
nim c -r src/nifler2/tools/gramcheck.nim src/nifler2/nimgrammar.nim
```

It reports:

```
  productions:            187
  rules:                  118
  rules with FOLLOW:      114
  notation errors:          0
  left-factored pairs:     31
  FIRST-set refinements:    5
  predicate-separated:      9
  declared %else:          11
  need FOLLOW:              0
  unresolved conflicts:     0
```

So **Nim's grammar is LL(1) over `(TokenKind, IndentClass)`**, given automatic
left factoring, eleven declared fallbacks, and six semantic predicates. That
was the open question the design rested on, and the answer is yes.

The six predicates are the complete inventory of what one token plus its
indentation cannot decide:

| predicate | what it asks | where |
|---|---|---|
| `noSpaceBefore` | `tsLeading notin tok.spacing` | `a(x)` is a call, `a (x)` a command |
| `isUnary` | operator in prefix position | `a - b` vs `a -b` |
| `inTypeDesc` | `mode == pmTypeDesc` | `proc`/`iterator` as a type vs a lambda |
| `notFirstParam` | position in a command's argument list | `exprEqExpr` only after the first |
| `dotLikeOps` | the `nimPreviewDotLikeOps` switch | `DOTLIKEOP` as a `.`-suffix |
| `isSigilLike` | operator spelling | `@x.y` binds the operand tighter |

Note that `inTypeDesc` and `notFirstParam` are predicates over a **rule
parameter**, not over the token — which is why parameterized rules are load
bearing rather than a convenience.

### What the transcription found in `doc/grammar.txt`

Faithfully transcribing and then checking turned up places where the published
grammar does not describe the parser. Each is marked `# GRAMMAR.TXT:` in
`nimgrammar.nim`:

* `exprStmt` says `simplePrimary` for the command form; `parser.nim` calls
  `simpleExpr(p, pmTrySimple)` for all three alternatives and decides
  afterwards. This is also what makes the three left-factor.
* `optPar` is written `(IND{>} | IND{=})?`, i.e. unconstrained; `parser.nim`
  errors on a dedent, so it is an assertion.
* `section`, `objectPart` and `stmt` give their flat alternative no
  indentation constraint, but `parser.nim` reaches it only through the `else`
  of `if realInd(p)`.
* `postExprBlocks` omits the leading `NO_IND` that `parser.nim` requires.
* `literal` lists `NIL` as a token class (it is the keyword) and omits
  `FLOAT128_LIT`.
* `commandParam` is missing entirely.
* `simplePrimary` is written `SIGILLIKEOP? identOrLiteral …`, but `parser.nim`
  has no such alternative — an operator always takes the prefix branch and
  `isSigilLike` only makes the operand bind tighter. Its own comment admits
  this ("sigil like operators are currently not reflected in the grammar …
  should be removed for Nim 2.0"). Modelling it as a real alternative is what
  made `SIGILLIKEOP` collide with `OPR`, so the checker found the documented
  wart on its own.
* `identWithPragmaDot` is declared and never used — the one thing
  `grammar_nanny` would also have caught.

Running the generated parser (stage 3 below) found seven more, and five of
them are the same mistake: **`doc/grammar.txt` records the token sequence and
not the indentation**, so a repetition that reads correctly swallows the next
statement.

* `'return' optInd expr?` — and the same for `raise`, `yield`, `discard`,
  `break`, `continue` — asserts the indentation even when the expression is
  absent, and a `return` at the end of a block is followed by a *dedent*.
  `parseReturnOrRaise` never calls `optInd`; the shape is `(optInd expr)?`.
* `enumDecl`'s field repetition has no guard. `parseEnum` ends its loop on
  `indent >= 0 and indent <= currInd`, which is `validInd` on the next field —
  without it an `enum` eats the next type definition of the section.
* `exprStmt`'s command form has no guard. `parseExprStmt` enters it only
  `if p.tok.indent < 0`, so `echo 1` / `echo 2` on two lines was one command.
* The command syntax is documented on `primary` and implemented inside
  `primarySuffix`'s loop, which is the only place it is under the indentation
  guard. Transcribed in both places, the copy on `primary` had no guard to be
  under. It also reads like a repetition (`commandStart expr …`) and is not:
  `commandExpr` takes exactly one parameter, so `echo a b c` is
  `(cmd echo (cmd a (cmd b c)))`.
* `commandStart` is written as a token set. The real guard is
  `p.inPragma == 0 and (isUnary(p.tok) or p.tok.tokType notin {tkOpr,
  tkDotDot})` — an *infix* operator must fall through to `binary`, so
  `import std / os` is `(infix / std os)` and not a command.
* `setOrTableConstr` writes `(exprColonEqExpr comma)*`, making the comma
  mandatory after every element, so `{a, b}` did not parse.
* `identVisDot` writes the `'.'` as required and allows an `OPR` after it.
  `identVis(allowDot)` is an if/elif on one of the two, with a bare symbol as
  the fallthrough — so `type T = object` never reached the `=`. `typeDef`
  likewise writes `pragma` and the `'='` as required; both are optional.
* Missing from `complexOrSimpleStmt`: `caseStmt`, and the standalone
  `except`/`finally` blocks. `case` as a *statement* was therefore
  unreachable — only `expr` offered one, and `exprStmt` goes through
  `simpleExpr`, which does not.

Getting from 65 of 188 files to every Nim file we have found twenty-five more.
They fall into four groups.

**The indentation is missing (again).** The same mistake as the five above,
and still the commonest one.

* `condStmt` writes `IND{=}` on its `elif` and `else`; `parseIfOrWhen` tests
  `sameOrNoInd`, which is what makes the one-liner `if c: a else: b` a
  *statement* rather than only an `ifExpr`. One-liners are everywhere.
* `condExpr` writes an `optInd` between branches and a mandatory `else`.
  `parseIfOrWhenExpr` asserts *nothing* between branches — that is what lets
  `let x = if c:` put its `else` back at the `let`'s own indentation — and its
  `else` is optional, so `(if it.native: flag = true)` parses.
* `tryExpr` writes `optInd` on each `except`. `parseTry`'s loop is
  `while sameOrNoInd(p) or isExpr`, so for an expression `try` the indentation
  is not consulted at all.
* `objectDecl` writes `objectPart` unconditionally; `parseObject` enters it
  only on a real indent. Reading a dedent as a branch made the next type
  definition a *field*: `Foo[T] = object` followed by `Bar[T] = object` gave a
  field named `Bar`. `objectPart`'s five flat branches are `sameOrNoInd`, not
  "not indented" — a dedent ends the object.
* `objectDecl`'s `('of' typeDesc)?`, `paramListColon`'s and
  `paramListArrow`'s `paramList?` and return type, `pragmaStmt`'s `':'`,
  `primaryPragma`'s `pragma?`, and `routine`'s `pattern?`,
  `genericParamList?`, `pragma?` and `'='` all carry a guard in `parser.nim`
  and none in grammar.txt. `primaryPragma`'s cost the most: without it a
  `{.noSideEffect.}:` block on the line after `var a = 1` became the
  *value's* pragma and then ate the `:` block that belonged to it.
* `semiStmtList` runs under `withInd`, and its `';'` is *optional* — the loop
  is "if `;` then skip it; if `)` then stop; parse a statement". grammar.txt
  writes `^+ ';'` everywhere, which can express neither the missing separator
  nor a trailing one.
* `stmt` writes `complexOrSimpleStmt ^+ (IND{=} / ';')`. `parseStmt` re-tests
  the indentation *after* eating the `;`, so a trailing `;` at the end of a
  block is legal.

**A separator that is not a separator.** `parser.nim`'s list loops all read
"parse an item; break unless a comma follows", i.e. they decide on the token
*after* the separator. grammar.txt writes them as `^+ comma` or
`(item comma)*`, which decides on the separator itself. Both spellings are
wrong in LL(1), in opposite directions: the first refuses a trailing comma,
the second demands one. `exprColonEqExprList`, `arrayConstr`, `paramList`,
`varTupleLhs`, `genericParamList` and `genericParam` all needed the
`item (comma item?)*` shape; the price is accepting `[a, , b]`.

**A production that was never written down.** Nine of them.

* `castExpr` has a second form, `'cast' '(' exprColonEqExpr ')'` — it is in a
  *commented-out* `#|` line in `parser.nim`, so it never reached grammar.txt,
  and it is the `{.cast(noSideEffect).}` pragma.
* `identOrLiteral` takes `'('` to a tuple in a type and to `parsePar`
  everywhere else. Both productions were listed and the generator factored
  them together, so the comma never got its meaning and `(int, var T)` was
  not a return type. It needs `mode`; see below for what else it wanted.
* `tupleType`'s bracket is optional — a bare `tuple` is a type, which is what
  `[T: tuple|object]` needs.
* `('ref'|'ptr'|'distinct')` in `typeDefValue` is not
  `(tupleDecl | objectDecl)`: `parseTypeDescKAux` returns early on a dedent,
  skips an operator, and otherwise parses a `primary` — and then feeds the
  result back into the operator loop, which is how
  `SomePointer = ref | ptr | pointer | proc` parses. All three are gone from
  `typeDefValue` now and reached through `primary`, which supplies the loop
  for free.
* `typeDefValue`'s extra command parameters come *after* the commas
  (`while p.tok.tokType == tkComma`), not before them. Written the other way
  the optional group is entered on FIRST(exprEqExpr) — every token an
  expression can start with — so `FileHandle* = cint` swallowed the next line
  of the type section and `= bool` swallowed the next `type` keyword.
* `typeDescExpr` is `parseTypeDesc(fullExpr = true)`, whose whole body is
  `simpleExpr(pmTypeDesc)`. Listing `routineType` as a separate alternative
  took `proc` out of the operator loop, so `p: proc | iterator {.closure.}`
  stopped at the `|`.
* `complexOrSimpleStmt` has a `'type' '(' primary ')'` form, with
  `parseOperators` and `postExprBlocks` over it.
* `genericParam` has an `of tkIn, tkOut:` branch — the covariance markers of
  `MyPtr[out T]`.
* `primary`'s sigil branch falls back to `primary(pmNormal)` when what follows
  the operator is not an operand kind; that is what parses
  `@! === result = "abc"`.
* `routine`'s name is optional: `parseRoutine` delegates to `parseProcExpr`
  when the token after the keyword is not a name, so
  `proc (): int {.closure.} = x` is a statement. And the name it does accept
  is `{tkSymbol, tkAccent, KEYW}` — `func addr*[T](x: T): ptr T` is in the
  stdlib — while every *declaration* dispatch (`parseIdentColonEquals`,
  `parseParamList`, `parseObjectPart`, `parseSection`, `parseVarTuple`,
  `parseTuple`) tests `{tkSymbol, tkAccent}` only. Two different sets, one
  `symbol` production in grammar.txt; the grammar now has `plainSymbol` for
  the narrow one.

**A rule reached through the wrong door.** `variable` says `identColonEquals`
and `colonBody?`; `parseVariable` passes `{withPragma, withDot}` and applies
`postExprBlocks` to the *value*, so `let navigator {.importc.}: JsObject` and
`var res = f(x) do (a: int) -> string:` both need it, and grammar.txt's
`colonBody` has no caller at all. `constant` says `varTuple`, which ends in
`'=' optInd expr`, so `const (a, b) = (1, 2)` wanted two `=`; `parseConstant`
calls `parseVarTuple`, which is `varTupleLhs`. `forStmt` says `varTuple` for
the same reason and has the same problem: `for (k, v) in attrs:` has no `=`.
`ofBranches` and `objectBranches` make the first `of` mandatory, but
`parseCase` is a `case` over `of`/`elif`/`else` with no ordering requirement,
so a `case` whose only branch is `else` is legal — and `parseObjectCase` has
no `elif` branch at all, which grammar.txt lists.

### Known gaps

* **`fanOut` is a stub.** `declColonEquals` parses `a, b: T = v` and NIF wants
  one `(var a T v)` per name. The runtime cannot do it: it needs the *number
  of names*, and the buffer alone cannot tell a trailing name from a type from
  a value. The notation needs a way to say "this item repeats, count it";
  until then the declaration stays as parsed and `p.section` — which nothing
  assigns yet either — is unused.
* **Source filters are not implemented.** `#? stdtmpl(…)` is a
  *preprocessor*: `nifler` links Nim's `filters.nim` and hands the parser
  rewritten text. Three files in the corpus use it
  (`tests/nimony/sysbasics/tscf_stdtmpl.nim` and two `niminaction` views),
  and they are the only files that fail for a reason other than being
  deliberately invalid. (Ported in stage 8.)
* **Two operator tails are still flat or absent.** `binaryTail` covers
  `complexOrSimpleStmt`'s `type(…)`. `parseTypeDescKAux`'s trailing
  `parseOperators` is covered by routing `ref`/`ptr`/`distinct` through
  `primary`. What is *not* covered is `parseOperators`' `modeB = if mode ==
  pmTypeDef: pmTypeDesc else: mode` and `simpleExprAux`'s
  `if mode == pmTrySimple: mode = pmNormal` — the generated `binary` threads
  the mode through unchanged, so the right operand of an operator is parsed
  in a slightly wider mode than `parser.nim` would use. No corpus file
  notices.
* **FOLLOW is wide.** It converges (10 rounds, 114 of 118 rules) and no pair
  needed it, but it inherits the whole-language FOLLOW through `expr`, so it
  will not be a sharp tool for error recovery without pruning.
* Rule-valued parameters are approximated by the union of every argument ever
  passed at that position — sound for FIRST, but coarser than
  monomorphization, which is what code generation will have to do anyway.

## Stage 2: code emission

`gramcheck --emit:<file>` generates the parser. The design's claim that the
output should be ordinary steppable code rather than a table-driven engine
holds up: `pStmt` for the real Nim grammar is twenty-five readable lines.

```nim
proc pStmt*(p: var Parser) =
  let m0 = mark(p)
  if (p.tok.kind in Tk1 and indClass(p) in {icGt}):
    pushInd p
    pComplexOrSimpleStmt p
    while (p.tok.kind in Tk1 and indClass(p) in {icEq}) or (p.tok.kind in {tkSemiColon}):
      if (p.tok.kind in {tkSemiColon}): expect p, tkSemiColon
      else: checkInd p, {icEq}
      pComplexOrSimpleStmt p
    if (p.tok.kind in Tk2) and indClass(p) == icGt:
      error p, "invalid indentation"
    popInd p
  elif (p.tok.kind in Tk17 and indClass(p) in {icNoInd, icLt, icEq}):
    pNotInd p
    pSimpleStmt p
    while (p.tok.kind in {tkSemiColon}):
      expect p, tkSemiColon
      pSimpleStmt p
  else:
    error p, "expected stmt"
```

and `pExprStmt` is the motivating case, parsing `simpleExpr` once and deciding
afterwards:

```nim
    pSimpleExpr p
    if (p.tok.kind in {tkEquals}):
      expect p, tkEquals; pOptInd p; pExpr p
      if (p.tok.kind in {tkColon, tkDo}): pPostExprBlocks p
      wrap p, m0, "asgn"
    elif (p.tok.kind in Tk8):
      pExprEqExpr p
      while (p.tok.kind in {tkComma}): pComma p; pExprEqExpr p
      ...
      wrap p, m0, "cmd"
```

**Every tag is emitted as a retroactive `wrap` at the alternative's mark**, not
as an `openTag`/`closeTag` pair. That is what makes factoring transparent: the
tag is chosen after the shared prefix has already been parsed, and the grammar
can keep saying `asgn[ simpleExpr '=' … ]` as though backtracking were free.

Token sets are hoisted into named constants (`Tk1`, `Tk2`, …); a
ninety-element inline set is not readable, and `pStmt`'s FIRST set has eighty.

### End-to-end proof

`src/nifler2/tests/` holds a miniature indentation-based language — the
smallest grammar that uses every mechanism the design rests on — its runtime,
and a test suite over the *generated* parser:

```
nim c -r src/nifler2/tests/tmini.nim
```

Twenty-four tests, all passing. They cover an indented block, a nested block,
a block followed by a dedented sibling, an inline `if c: y = 2` body,
`;`-separated statements, the `asgn`/`cmd` retroactive wrap, an
over-indentation error, precedence climbing (left, right, parenthesised, and
an operator refused at the start of a line), left-associative postfix chains
(`a.b(c).d[e]` -> `(at (dot (call (dot a b) c) d) e)`), and the
space-sensitivity predicate (`f(1)` is a call, `f (1)` a command).

### What emission taught the analysis

Three corrections, each a real bug rather than a prototype slip:

* **`leadMask` of an alternation may only union its *nullable* alternatives.**
  `(';' | IND{=})` constrains the next token to `IND{=}` exactly when the `;`
  was absent; letting the consuming alternative widen the mask to `AnyInd`
  silently dropped the constraint, and `module`'s statement loop would have
  accepted a dedented statement as a continuation.
* **The mask must be threaded past partially nullable items**, not only past
  pure guards — same root cause, different place.
* **`^` was over-used in the transcription.** Fourteen productions had it
  where the tag spans the whole alternative and plain wrapping is correct;
  with `^` they would have anchored on the caller's value, so `- -x` would
  have produced one `prefix` node covering both operators. Emission forced
  the distinction into the open.
* **Only over-indentation is unambiguously an error.** After a repetition
  stops, a token whose *kind* could have continued it is reported as
  "invalid indentation" only when it is `IND{>}`; a dedent is a legitimate
  handoff to an enclosing construct. Getting this wrong broke every nested
  block. With the rule in place the generated parser reproduces `parser.nim`'s
  diagnostic without anyone writing it per rule.

### Predicates have to reach the caller

A FIRST set cannot express `&noSpaceBefore`, so a caller's loop test cannot
either — `while p.tok.kind in {tkParLe, …}` would enter `primarySuffix` for
`f (x)` and then find every alternative's guard false. For each rule whose
alternatives carry predicates the generator therefore emits a companion:

```nim
proc canPrimarySuffix*(p: Parser): bool =
  ((p.tok.kind in {tkParLe}) and noSpaceBefore(p)) or
  ((p.tok.kind in {tkBracketLe}) and noSpaceBefore(p)) or
  (p.tok.kind in {tkDot}) or ...
```

and uses `canX(p)` wherever a "can this start here" test is needed. Three
rules need one in the Nim grammar: `primary`, `primarySuffix`, `commandParam`.

### Still to do in emission

* `la2(...)` is the only construct still emitted as a comment.
* A redundant re-test of a trailing `x?` inside its own branch — harmless,
  worth folding away.
* A leading `&X` where `X` is a *rule* is now intersected into the dispatch
  condition (`intersectFirst`). It was dropped before, and it was *not*
  harmless: `par`'s `&parKeyw` alternative was entered on the whole of
  FIRST(complexOrSimpleStmt), so every `( … )` went to the statement-list
  branch and `(1, 2)` never reached the tuple form. A `&X` that is not
  leading — `tryStmt`'s `&(optSameInd ('except' | 'finally'))` — is still
  dropped, and there it only weakens an assertion.
* A nullable item in front of a leading `&X` hides it from `leadAhead` and so
  from the refinement. `parsePar`'s real prelude is `optInd; flexComment`, and
  spelling the `flexComment?` cost the refinement; it is left out, since
  `( ## doc` in front of the deciding keyword is not LL(1) anyway.
* The generator is still `gramcheck.nim`, a standalone text scanner; the real
  one is the `deps/parsegen` plugin behind `template grammar*(...)`.

## Stage 3: the runtime

`src/nifler2/parserrt.nim` is what the generated code is written against, and
`src/nifler2/nifler2.nim` is the tool: Nim in, NIF out, no Nim-compiler
dependency anywhere in it.

```
bin/nimony c -o:bin/nifler2 src/nifler2/nifler2.nim
bin/nifler2 t file.nim      # print the tree
bin/nifler2 p file.nim      # write file.nif
```

Two halves, and the generator names but does not know either: the token stream
(`nimlexer`, plus every predicate the grammar asks about a token) and the
output buffer (a `nifcore.TokenBuf`).

**`wrap` is the whole trick.** `mark` records a position *and* the line info of
the token that was current there, and `wrap` retroactively splices the opening
tag in. Since marks nest rather than interleave, a wrapped node always ends at
the current end of the buffer — which is exactly the situation nifcore's own
`reopenLastTree` + `closeTag` is written for, so the jump arithmetic (including
the `ExtendedSuffix` a body over 2^19 tokens needs) is not reimplemented. The
splice itself appends through `add` before shifting, because `growRawUninit`
grows capacity to *exactly* the requested length and a one-token splice per
node would otherwise realloc the buffer on every node.

`mark` returning a `Mark` rather than an `int` is what makes line info work at
all: the tag's position is the position of a token that was read before the
rule knew which tag it would use.

Six predicates were needed beyond what `nimlexer` already answers, and four of
them are conditions `parser.nim` writes out by hand at the call site:
`suffixStart`, `commandStart`, `commandAllowed`, `inTypeDesc`, `dotLikeOps`
and `noSpaceBefore`.

### What running it found

The pipeline exposed eleven defects that neither the conflict checker nor
`nim check` could see, because all of them are about *what the parser does*
rather than whether it is well-formed. Four are in the generator:

* **A nullable alternative got an `else: error` branch.** `indAndComment` is
  `(IND{>} COMMENT)? | COMMENT?`; matching nothing is legal and was reported
  as "expected alternative".
* **`condOf` ignored a body's leading predicates.** `(&suffixStart X)*` looped
  on FIRST(X) alone, and a predicated `X` was entered on FIRST(X) instead of
  `canX`.
* **`leadMask` gave up at the first item that *could* consume a token.**
  `optInd` is `COMMENT? validInd`, so its indentation constraint was lost
  entirely — every `optInd` in the grammar was inert.
* **`firstOf` restricted an `indented(...)` body to `IND{>}` instead of
  re-measuring it.** The body's FIRST set is measured against the *inner*
  indentation, and `pushInd` makes the block's first token define it: a token
  that is `IND{=}` inside is `IND{>}` outside. Restricting instead of mapping
  made every left-recursive indented alternative unreachable — `objectPart`'s
  own guard is `notInd`, which excludes `IND{>}`, so the intersection was empty
  and the branch compiled to `if false:`.

The other seven are transcription errors, listed under "What the transcription
found in `doc/grammar.txt`" above and marked `# GRAMMAR.TXT:` in the grammar
itself.

### How much parses

`src/nifler2/tools/parsesweep.sh` runs the tool over whole directories and
groups the failures by message, because the message names the production that
is still wrong:

    src/nifler2/tools/parsesweep.sh lib src tests

Everything parses, except source filters:

| corpus | parses | fails |
| --- | --- | --- |
| `src lib tests examples` (nimony) | 1405 | 1 |
| Nim's `lib` + `compiler` + `tools` | 535 | 0 |
| Nim's `tests` | 3210 | 35 |

The one nimony failure and two of the 35 are `#? stdtmpl` source filters. Of
the remaining 33, 30 are tests that *expect* to be rejected (`errormsg:`,
`action: "reject"`, `tt.Error`, or `disabled: true`) and three are
deliberately-broken helper modules or legacy code that `nifler` rejects at the
same line and column — checked one by one, not assumed.

Getting there took the twenty-five further grammar findings listed above, two
new pieces of notation (`withInd`, `binaryTail`), and three generator fixes
(`&rule` folded into the dispatch, `{…}` arguments rendered raw, an outward
FIRST map for `withInd`). The sweep script is what made it a grind rather than
a guess: it groups failures by message, and the message names the production.

(The count went *down* by one when `commandStart` learned `parser.nim`'s
token set: `Bar not nil not nil` in `parser/tdoublenotnil.nim` expects an
error at the second `not`, `nifler` gives it there, and nifler2 used to accept
the file by reading `not nil` as a command argument.)

The tree is **not** yet the tree `src/nifler` produces — see `fanOut` above
and "Stage 4" below. This measures acceptance only.

## Stage 4: the tree diff

`src/nifler2/tools/treediff.nim` compares what the two tools *build*:

    nim c -o:bin/treediff src/nifler2/tools/treediff.nim
    bin/treediff file.nim                    # one file, every difference
    bin/treediff --sweep [--top:N] lib src tests

It runs `bin/nifler p` and `bin/nifler2 p` on each file, reads both outputs
through `nifreader` and walks the two trees in step. Line information and the
header directives are ignored; atoms are compared by decoded value, so an
escape or a number spelled differently is not a difference.

The walk is what makes the output usable. When two children are not even the
same kind of node — a different tag, or an atom where a subtree should be —
the difference is recorded and the rest of the *enclosing* node is skipped on
both sides, because nothing after it lines up any more. A difference deeper
inside a child that otherwise matches does not do that, so the child's
siblings are still compared and a file with three unrelated bugs reports
three. (One consequence: a head mismatch directly under `stmts` hides the rest
of the module, so the `(stmts` row undercounts until declarations line up.)

The sweep prints two tables. **By enclosing node** is the work list — which
node kinds disagree, and in how many files. **By signature** groups each
difference by the enclosing tag and the *shape* of both sides (a tag name or
an atom kind: `x` and `y` are the same difference), with one example each.
A file whose output the reader cannot walk is reported as *unreadable*
rather than compared, since that is an output bug in its own right.

First run, before any fidelity work:

| corpus | same | different | differences | unreadable |
| --- | --- | --- | --- | --- |
| nimony `src lib tests examples` | 91 | 1313 | 8123 | 2 |
| Nim `lib` + `compiler` | 1 | 491 | 5085 | 3 |

What the top of the list says, in order of size:

* **Empty children.** nifler writes every absent child as `.`:
  `(proc name exported pattern typevars params result pragmas effects body)`
  with dots where nothing was written. nifler2 omits them, so the first
  present child lands in a slot that expects a dot — 2574 times in `proc`
  alone.
* **Export markers.** nifler puts an `x` in the second slot; nifler2 writes
  `(postfix name *)` as `parser.nim`'s AST does.
* **Declarations.** `let`/`var`/`const` produce one node per name in nifler
  and the bare parsed pieces in nifler2 — the `fanOut` gap.
* **`(stmts …)`.** nifler wraps every branch body (`elif`, `else`, `try`,
  `block`) in `stmts`, including single-statement ones.
* **Comments.** nifler writes `(comment)` without the text unless `--docs` is
  given.
* Two point bugs: `` `=copy` `` loses its `=` (`plainSymbol`'s quoted form
  drops a quoted terminal, the same bug `@'…'` fixed for operators), and a
  non-finite float literal is written as `(inf)@D,25` — line info after a `)`,
  which is not NIF. nifler writes `(suf (inf) "f64")`; the line info is
  attached by `nifpools.addFloatLit`, which would do the same for any caller.

### Placeholders, export markers and one node per name

The dialect nifler writes is not the shape of the source, and three notation
pieces plus a handful of runtime layouts close most of the gap:

* **`X?.`** is an option that writes `.` when absent, and a bare **`.`** is a
  slot that is always empty. The generator has to know that both *write*
  something although they consume nothing — otherwise a `.` looks like a
  pure indentation guard, and an empty match looks like it needs no branch.
* **`exportMarker`** is `OPR` with an action that replaces the operator by
  the identifier `x`, which is what nifler writes in the export slot.
* **Layouts.** The grammar parses in source order and writes every slot;
  runtime procs then rearrange what a rule wrote, by copying the trees since
  the rule's mark to a scratch buffer and writing them back:
  - `fanOut`: `name x pragmas` per name, then `type value`, becomes one
    `(section name x pragmas type value)` per name. The count is exact
    because every slot is written — the reason `fanOut` was a stub before
    is gone. The section tag comes from a stack pushed by the rule that opens
    the section (`let`/`var`/`const`, `paramList`, `genericParamList`,
    `objectDecl`).
  - `routineLayout` / `procLayout`: nine children with the effects slot, and
    the lambda-versus-type decision for an anonymous routine (a type keeps a
    missing signature as a single `.`, a bare `proc` is `(proctype)`).
  - `moveLastToMark`: `for` and `let (a, b) = v` put the iterated or unpacked
    value first.

The placeholders flushed out bugs that acceptance could not see, because a
missing child only becomes a crash once something counts the children:
`'addr'`, `'type'` and `'static'` as names were plain terminals and wrote
nothing (`template t(): type` lost its return type), backquoted operator runs
lost their punctuation (`` `[]=` ``), `return false;` ending a proc took the
next proc into its body, and an object's nested `when` took the outer `elif`.

Two places deliberately do *not* follow nifler. Its section is a single
variable that nothing restores, so a nested construct changes the tag of the
names after it: `var a, b: proc (x: int)` makes `b` a `param`. nifler2 keeps
the section of the declaration. The one place the leak is reproduced is
`using`, which nifler never sets and which therefore inherits whatever was
opened last; nothing downstream reads that tag.

After this pass:

| corpus | same | different | differences | unreadable |
| --- | --- | --- | --- | --- |
| nimony `src lib tests examples` | 225 | 1179 | 10775 | 2 |
| Nim `lib` + `compiler` | 17 | 475 | 10178 | 3 |

The difference count went *up* because declarations now line up at the top
of a module, so the walk no longer abandons the rest of the file at the first
`let`. Of what remains, all but about 160 per corpus is one thing: nifler
wraps every statement body in `(stmts …)`, including single statements.
The rest is object constructors (`oconstr`), `'i64` suffixes on promoted
literals, `callstrlit`, statement-list expressions and the section leak above.

### `(stmts …)`

`parseStmt` builds an `nkStmtList` in both of its branches, so every statement
body is a `stmts` in nifler, a single statement included: `if c: x` is
`(if (elif c (stmts x)))`. Both `stmt` alternatives carry the tag now.

The exception is inside parentheses. `semiStmtList` increments
`p.inSemiStmtList`, and while it is non-zero `parseStmt`'s one-line branch
returns the bare statement — `(if a: b else: c)` is `(elif a b)`. The counter
is not reset by anything nested, so the runtime keeps the same counter and
`stmt` has a third, predicated alternative. The list itself is an
`nkStmtListExpr`, written `(expr (stmts <all but the last>) <last>)`, which
`stmtListExprLayout` builds.

Making `stmt` a predicated rule exposed one more generator bug: an optional
`(IND{>} stmt)?` was entered through `canStmt(p)`, which knows `stmt`'s own
predicates but not the guard written in front of it at the call site, so a
concept without a body took the next line as one. `condOf` now ANDs the
leading guards' mask into a `canX` test.

| corpus | same | different | differences | unreadable |
| --- | --- | --- | --- | --- |
| nimony `src lib tests examples` | 907 | 497 | 3894 | 2 |
| Nim `lib` + `compiler` | 191 | 301 | 2130 | 3 |

The largest remaining class is `postExprBlocks`: a trailing `:` block turns
its expression into a call that owns the block — `c.into:` is
`(call (dot c into) (stmts …))` — where nifler2 writes the block as a
sibling. After that come object constructors (`oconstr`), `'u64`/`'i64`
suffixes on promoted literals, `callstrlit`, and pragma blocks (`pragmax`).

### The rest of the dialect

The remaining classes were each a construct `parser.nim` rewrites after it
has parsed it, and each became a runtime layout or retag:

* **`postExprBlocks`** makes its operand a call that owns the blocks —
  `c.into:` is `(call (dot c into) (stmts …))`, and `foo x:` stays a `cmd`
  that gains the block. Every call site is now a rule that parses the operand
  and the blocks together and ends in `attachBlocks`, whose mark is where the
  operand starts; `exprBlocks` is that rule for the sites that apply it to
  one `expr`. A `do` block is `(do params ret body)` or, with no signature,
  just its body.
* **`nkObjConstr`** and **`nkTableConstr`**: a call or a `{…}` whose element
  is `name: value` is retagged (`callOrObjConstr`, `curlyOrTable`).
* **Literals**: the sized kinds including `'i64`/`'u64`/`'f64` are
  `(suf value "i64")`; a generalized string is `(callstrlit f (suf "…" "R"))`,
  as a suffix of its own because the lexer only produces one right after an
  identifier; a custom numeric literal is `(dot (suf "-1" "R") 'big)`; a
  literal inside backquotes is an identifier.
* **Rewrites**: `x.y[:z](a)` is `(call (at y z) x a)` (`dotLayout`); a pragma
  with a block is `(pragmax pragmas body)`; a sigil-like prefix takes the
  following suffixes onto the whole prefix node; an operator's right operand
  is parsed in `rhsMode(mode)` — `pmTrySimple` becomes `pmNormal`,
  `pmTypeDef` becomes `pmTypeDesc` — which `binary(...)` now accepts as extra
  arguments after the limit.
* **Configuration**: nifler does not define `nimPreviewDotLikeOps`, so a
  dot-like operator is an ordinary infix operator.

And, as every pass so far has, it found places where nifler2 accepted the
wrong program: `try`'s `except`/`finally` is one loop in any order (so a
second `finally` is not a syntax error) guarded by `optPar` in the expression
form; a proc type's pragma needs `validInd`; `binaryNot` needs the `not` on
the same line; a command's comma-separated arguments stop at a comma that is
dedented below the block; `discard` takes a comment only on the same line.
One generator fix: an action on an alternative whose anchored tag sits behind
a predicate got the rule's mark instead of the anchor.

| corpus | same | different | differences |
| --- | --- | --- | --- |
| nimony `src lib tests examples` | 1405 | 1 | 1 |
| Nim `lib` + `compiler` + `tools` | 523 | 12 | 13 |
| Nim `tests` | 3192 | 6 | 10 |

The one nimony file is a `#? replace` source filter. Every difference in Nim's
`lib`, `compiler`, `tools` and `tests` is nifler's section leak, which is
deliberately not reproduced (see above). Of the files only one tool rejects,
the eight nifler rejects are either tests that expect the rejection or
assertion failures inside `bridge.nim` on valid input; the three nifler2
rejects are two source filters and `errmsgs/t10735`, an error test. Two Nim
tests are unreadable because of the non-finite float writer bug in
`nifcoreparse`, fixed on its own branch.

## Stage 5: line info and headers

nifler2's output is now byte-identical to `nifler --portablePaths p`, header
and positions included, which is what `treediff --bytes` checks:

| corpus | byte-identical |
| --- | --- |
| nimony `src lib tests examples` | 1405 of 1406 |
| Nim `lib` + `compiler` + `tools` | 523 of 535 |
| Nim `tests` | 3193 of 3200 |

The exceptions are the section leak (see above), the `#? replace` source
filter, and two files with a line longer than 1024 characters
(`src/models/nimony_tags.nim`, Nim's `tests/sets/tsets.nim`): after such a
line Nim's lexer reports columns off by 1024, and that is not reproduced.

**The writer, `niflerout.nim`**, cannot be `nifcoreparse`'s: that one gives a
token without a position the position of the token before it and writes an
index. nifler's rules are:

* header `(.nif27)`, `(.vendor "Nifler")`, `(.dialect "nim-parsed")`, no index;
* the root carries the file, relative to the current directory;
* every other position is the difference to a *reference* and is omitted when
  that is zero; a `.` and the export marker `x` (`addRaw " x"`, a literal
  space even after a `)`) never carry one;
* `addEmpty(n)` writes `n` dots without separators, and bridge.nim uses
  `n > 1` in three places the writer reproduces: the first four and last two
  slots of `proctype`/`itertype`, the four slots of a `for` loop's tuple
  variable, and the type/value slots of every other unpacking `let`/`var`/
  `const`.

The reference is the node's parent *in Nim's AST*. Where that is not the
enclosing NIF tag the writer knows it: a routine's result type is relative to
its `(params)`, a lambda writes its position on the name placeholder
(`(proc .@5,1 ...`) with its children relative to it, a tuple field `(kv` is
relative to the tuple's parent, and `(expr (stmts ...) last)` puts the
position on the `stmts` with `last` relative to the expression. Those quirks
live only in the writer; the buffer holds true positions.

**The positions** are `parser.nim`'s: `newNodeP` takes the token current when
the node is *created*, which is the first token for most nodes and something
else for many:

* **`%at`** in the notation marks the token a node is positioned at: `asgn`,
  `kv` and `vv` at their operator, a type definition at its `=`, a `for` loop
  after the keyword, an `elif` of `if`/`when` at its condition (but `case`'s
  and a post-expression block's `elif` at the keyword), an enum field at its
  value or after its pragma, a bare `tuple` after the keyword, a `not nil`
  after its operand. A `%at` shared by left-factored alternatives is captured
  by the group before the shared token is consumed.
* An anchored tag (`^call[ '(' ...]`) is positioned where its own items start,
  an infix node from `binary` at its operator.
* A declaration is positioned at parser.nim's *name node*: the `{.` if there
  is a pragma, the export operator if exported, else the name.
* `cmd` takes its first child's position, a pragma block its pragma's, a
  routine its keyword's (through a small position stack, since the keyword is
  consumed before the rule that builds the node).
* `posMarker` passes a position to a layout that needs one it cannot see:
  `x.y[:z]()` builds its call after the `]`, and a `do` with pragmas but no
  parameter list gets its made-up `(params)` after the body.
* Tags nifler invents (`ranges`, `unpackflat`, `unpacktup`, the unpacking
  `let`s, the `stmts` around a multi-name field) carry no position, and the
  empty discriminator of a bare object `case` has an empty node's, written
  as `~1,,???`.

## Stage 6: the deps file

`nifler2 --deps p file.nim out.nif` also writes `out.deps.nif`, and
`nifler2 deps` writes only that; both are byte-identical to `nifler`'s on
every corpus file whose main output is (`treediff --sweep --bytes --deps`).

bridge.nim produces the deps file *while* it translates, into a second
builder with positions off. Everything it looks at is in the finished tree, so
here it is a walk over the buffer (`writeDeps` in `niflerout.nim`):

* `import`, `importexcept`, `fromimport`, `include`, `export` and
  `exportexcept` are copied wherever they occur, a proc body included;
* inside `when` branches each carries a `(when COND...)` marker after its tag,
  an `else` contributing `(prefix not COND)` for every earlier condition --
  an object's `when` does not count, bridge.nim only tracks `nkWhenStmt`;
* `{.plugin: "name".}` becomes `(plugin (when...)? "name")`, the name written
  as a plain string even when the source has a raw one;
* nothing under a `runnableExamples` call counts.

Both outputs are written only if they changed, like nifler's `OnlyIfChanged`:
nimony's incremental build relies on an untouched `.p.nif` keeping its
modification time, so a `touch` does not re-run `nimsem`.

The command line takes what nimony passes -- `--portablePaths`, `--deps`,
`-f`, options before or after the command -- and names the outputs as nifler
does. End to end, a toolchain with nifler2 installed as `nifler` builds and
runs 79 tests from 15 directories with the same output as the normal one.

## Stage 7: error messages

A file nifler rejects, nifler2 rejects with the same first line:
`/abs/file.nim(line, col) Error: message`, on stdout, exit code 1, no output
written. nifler recovers and reports on; nifler2 stops at the first error, so
the first line is what is compared. `tools/errsweep.sh dir...` runs both over
every file nifler rejects and groups the differences by message pair;
`tools/errmutate.nim` supplies the files -- mutants of valid code, one
deleted, duplicated or inserted token, shifted indentation or joined line
each.

| corpus | same first error |
|---|---|
| Nim's `tests/` (every syntax error nifler reports) | 36 / 36 |
| mutants of `lib/std` + `tests/nimony` (the development set) | 930 / 938 |
| mutants of Nim's `lib/pure` + `compiler` | 145 / 145 |
| mutants of `src` + Nim's `lib/std` + `lib/system` | 217 / 219 |
| `errmutate` seed 5 over every other file of `lib/std` + `tests/nimony` | 961 / 975 |

The first three sets came from a throwaway prototype of `errmutate` and are
not reproducible from the seed; the last one is.

Counted over files nifler rejects *without crashing* -- bridge.nim asserts on
some broken trees, which the sweep lists separately.

parser.nim positions a message at the current token (`parMessage`) or at the
lexer, the end of that token (`lexMessage`, which `eat` uses). The runtime has
both (`error`, `lexError`) and Nim's helpers on top: `expect` is `eat`'s
"expected: ')', but got: 'keyword proc'" with `prettyTok`'s spelling.

Nim has no single place a production fails. A message is whatever check of
the hand-written procedure the token trips first, so the generated dispatch
reports `ruleError(p, rule, misplaced)` and the runtime picks the message:

* `misplaced` is computed by the generator: the token starts some alternative,
  or every alternative opens with an indentation guard (`optInd expr`), but
  the indentation is wrong. That is nearly always "invalid indentation".
* otherwise `identOrLiteral`'s "expression expected", except for the rules
  whose Nim procedure says something else -- `parseSymbol`'s "identifier
  expected", `parseStmt`'s "nestable statement requires indentation",
  `parseIdentColonEquals`'s "':' or '=' expected", `parsePragma`'s "expected
  '.}'".

What is left are places where parser.nim loops *until* a token and so
complains about the one in front of it rather than about the missing closer.
Those are `%else` rules with an action and no production: `listEnd(close)`
after an opening bracket or a comma (`exprColonEqExprListAux` loops `while
p.tok.tokType != endTok`), `strictListEnd` after a section or field list
(`while sameInd(p)` and "identifier expected" for anything else),
`enumListEnd`, `paramStart`, `stmtListEnd`, `accentEnd`, `missingEquals`
(`indAndComment`'s "maybe you forgot a '='", which points at the end of the
previous token), and the empty-branch checks `requireFields` and
`requireExcept`.

Matching the messages also meant matching what is *accepted*, and every such
change moves nifler2 towards Nim:

* `simpleStmt` takes an expression only on `isExprStart`: `of x` and `.foo`
  are not statements, though `primary` could parse them;
* a one-line statement body cannot be `var`, `if`, `proc` and the other
  statements `parseStmt` refuses before it tries `simpleStmt`, and it has to
  be on the same line;
* `var a` needs a type or a value; only a parameter may have neither;
* a routine name is a symbol, `addr`, `type` or `static` -- any other keyword
  is "identifier expected" instead of an anonymous routine;
* a command takes one parameter and ends the suffix loop:
  `x = 3 proc p() = discard` made `p()` a second parameter of `3`;
* `&inOrOut` sat inside a tag, where a predicate is dropped, so `[var T]` was
  a generic parameter;
* a binary operator checks `optPar` behind it and skips a comment;
* `case x` with no branches is legal, an object-case branch with no fields is
  not, a `try` needs an `except` or `finally`, and a `func` type is an error;
* `f(a, , b)`, an indented first line and a comma-continued command parameter
  on a dedented line are rejected.

Over the tree corpora the only status changes are files nifler agrees with:
Nim's `t10735` (an empty `case`) now parses to the same tree, and
`temptycaseobj`, `ttypecommandindent1` and `tfunc_type` fail with nifler's
message.

The lexer's messages moved to Nim's positions too: a number's errors are
reported where `lexMessageLitNum` reports them and quote the literal the way
it does, a trailing underscore at the start of the identifier, a missing
closing quote behind the last escape sequence, and `0O5` gets its own
message.

What still differs is recovery-shaped: a `{.` that swallows the rest of a
block, an unclosed bracket spanning lines, the concept-body messages, and
`5else` lexing as a bad float in Nim and as `5` followed by `else` here.

## Stage 8: source filters

A first line `#? stdtmpl(subsChar = '$') | standard` -- after an optional BOM
and shebang line -- runs the file through a filter before it is parsed.
`src/nifler2/filters.nim` is a port of Nim's `syntaxes.nim` (the pipe),
`filters.nim` (`strip`, `replace`) and `filter_tmpl.nim` (`stdtmpl`), text in
and text out: the parser, the writer and the deps file see the filtered
source under the original file name, which is also what nifler does.

The pipe is Nim code and nifler2's parser parses it; the filters read their
arguments off that tree (`vv` for `name = value`, a `suf` around a raw or
triple-quoted string). Two details of Nim's stream layer are kept because
they change bytes: the file is read with `readLine` and a filter's output
with `llStreamReadLine`, which disagree on a trailing empty line, and the
second of two chained filters reads the first one's output that way.

The filters' errors (`'x' not allowed here`, `invalid expression`, `expected
closing '}'`, `'end' does not close a control flow construct`, `invalid
filter`) are reported like nifler's, at the same positions, and the parse
goes on; the exit code is 1 afterwards. One place differs on purpose: a
`$` at the very end of a template line is an index error that crashes
nifler, and nifler2 reports "invalid expression" instead.

All four filter files in the corpora (`tscf_replace`, `tscf_stdtmpl` and two
`niminaction` views) are byte-identical to nifler, deps file included, and so
are fifteen synthetic ones covering the filter arguments, chaining, CRLF
input, a BOM, a shebang line and each of the error messages. With them no
file in any corpus is rejected by nifler2 alone.

## Performance

Both optimized (`nim c -d:release` for nifler as hastur builds it,
`nimony c -d:release` for nifler2), timed on only the files both accept. 2026-09-13, AMD Ryzen AI Max+ 395, best of 3, the
shell loop's own cost subtracted:

| workload | nifler | nifler2 | nifler2/nifler |
| --- | --- | --- | --- |
| startup: empty file, 200 processes | 0.091s | 0.028s | 0.31 |
| one 4.8 MB file (4 compiler modules x10), 5 runs | 0.749s | 0.849s | 1.13 |
| nimony `src lib tests examples`, 1411 files, one process each | 0.744s | 0.539s | 0.72 |
| Nim `lib compiler tools`, 535 files, one process each | 0.506s | 0.440s | 0.87 |

The way nimony uses it — one process per module — nifler2 is faster, because
nifler is a 12 MB binary that initializes Nim's compiler state and nifler2 is
0.7 MB. On raw throughput it is 13–25% slower, and it runs twice the
instructions (callgrind: 294M vs 152M on the four modules once) at half the
peak memory (48 MB vs 104 MB on the big file). `src/nifler2/tools/phasebench.nim`
splits the big file's 169 ms: read 1 ms, lex 23 ms, parse 74 ms, write 56 ms.
The hot spots callgrind names are the writer's `emit` (11%, plus 5% comparing
tag names as strings), `splice` under `wrapAt` (8%: every retroactive wrap
shifts the tokens after its mark) and `takeTail` under `fanOut` and
`routineLayout` (5%).

Past line 65535 nifler's line info goes wrong (Nim's `TLineInfo.line` is
16 bits); nifler2's does not, so the big file's outputs differ there and
nowhere else.

## Staging

1. `src/nifler2/deps/parsegen.nim` — the mini-language front end, FIRST/FOLLOW
   over `(kind, indentClass)`, the conflict report. Exercised first as a
   standalone checker against the transcribed Nim grammar, before any code is
   emitted: the point is to find out what the conflict list actually looks
   like while the design can still change.
2. Code emission, exercised on a small grammar (`nifgram`'s own NIF grammar is
   a good first target).
3. `nimlexer` in Nimony, differentially tested against `compiler/lexer.nim`
   token by token over the whole Nim stdlib.
4. The Nim grammar, rule by rule, seeded by transcribing `doc/grammar.txt`.

The acceptance test is a byte diff: old `nifler` against new over `tests/`,
`lib/`, `examples/` and Nim's stdlib. Same NIF, or a difference that was
deliberate and reviewed.
