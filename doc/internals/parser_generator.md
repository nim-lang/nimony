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

It is also a trap: inside a `( … )?` the mark is *not* fresh (a repetition's
is), so `(else[ 'else' colcom stmt ])?` trailing an `elif` chain wraps the
chain too. See "Tag placement" under Known gaps.

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

Generated by `lex`: identifiers, and every numeric literal form. Numbers are
where the automaton earns its keep; `getNumber` in Nim's lexer is a hundred
lines of hand-rolled state, most of it spelling out where `_` may appear.

Two things are *not* generated that were meant to be, both covered in
`doc/internals/lexer_state_limit.md`:

* **Keywords.** Nim's keywords are style-insensitive — `p_roc` and `pRoC` are
  the keyword `proc`, `Proc` is an identifier — so a keyword is a family of
  spellings rather than a literal. It is expressible (`p(_?[rR])(_?[oO])(_?[cC])`)
  but costs 401 DFA states for the set. `nimlexer` normalizes and binary-searches
  instead, which is what Nim's identifier cache does.
* **The numeric suffix.** `'i8` / `u32` / `'myLit` was in the patterns and had
  to come out: six number patterns each carrying it overflow the plugin's
  255-state ceiling, even though the minimized DFA is 30 states.

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
compared exactly.

`src/nifler2/tests/tnimlexer.nim` is the small half — 26 constructs, each
expectation produced by `refdump` rather than guessed, so a failure names the
construct instead of the file.

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
* **Tag placement inside `( … )?`.** A tag mid-sequence wraps from the
  alternative's mark, which is deliberate and load bearing (see "Two
  anchors"), but a repetition gives its body a fresh mark and an *option*
  does not. So `(else[ 'else' colcom stmt ])?` at the end of `condStmt` wraps
  the `elif` chain in front of it: `if c: 1 else: 2` comes out as
  `(if (else (elif c 1) 2))` instead of `(if (elif c 1) (else 2))`. The same
  shape appears in `condExpr`, `ofBranches`, `objectBranches` and
  `objectWhen`. The fix is to give `nOpt` a fresh mark and let `^tag` inside
  an option resolve to the enclosing one, exactly as `nRep0` already does —
  but it touches every `(tag[ … ])?` in the grammar, so it belongs with the
  tree-diff harness rather than before it.
* **Source filters are not implemented.** `#? stdtmpl(…)` is a
  *preprocessor*: `nifler` links Nim's `filters.nim` and hands the parser
  rewritten text. Three files in the corpus use it
  (`tests/nimony/sysbasics/tscf_stdtmpl.nim` and two `niminaction` views),
  and they are the only files that fail for a reason other than being
  deliberately invalid.
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
| Nim's `tests` | 3211 | 34 |

The one nimony failure and two of the 34 are `#? stdtmpl` source filters. Of
the remaining 32, 29 are tests that *expect* to be rejected (`errormsg:`,
`action: "reject"`, `tt.Error`, or `disabled: true`) and three are
deliberately-broken helper modules or legacy code that `nifler` rejects at the
same line and column — checked one by one, not assumed.

Getting there took the twenty-five further grammar findings listed above, two
new pieces of notation (`withInd`, `binaryTail`), and three generator fixes
(`&rule` folded into the dispatch, `{…}` arguments rendered raw, an outward
FIRST map for `withInd`). The sweep script is what made it a grind rather than
a guess: it groups failures by message, and the message names the production.

The tree is **not** yet the tree `src/nifler` produces — see `fanOut` and
"Tag placement" above. This measures acceptance only. The next chunk is a
tree-diff harness against `bin/nifler`, and it wants to exist before the tag
work starts, not after.

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
