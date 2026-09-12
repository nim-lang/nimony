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
| `&name` `!name` | semantic predicate; `name` is a proc `(p, tok) -> bool` |
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
mark) and the output buffer `b` in scope. `enter:`/`leave:` wrap the match —
both are valid Nim, `enter: stmt` being an ordinary call with a colon block.

```nim
  pragma "'{.' optInd (exprColonEqExpr ','?)* optPar ('.}' | '}')":
    enter: inc p.inPragma
    leave: dec p.inPragma

  namedParams "call[ '(' (exprColonEqExpr ','?)* ')' ]":
    if b.arity(m) == 2 and b.secondChild(m).isKv:
      b.setTag m, OconstrL          # nkCall -> nkObjConstr, post hoc
```

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
  `grammar_nanny` does today).

## Generated output

* one proc per rule, mutually recursive, forward-declared; dispatch is a `case`
  on the token kind, so the emitted code is ordinary readable Nimony that can
  be stepped through — no tables, no interpreter;
* `const grammarText*`, the rendered EBNF, which `nifler grammar` prints and a
  test compares against `doc/grammar.txt`;
* a report listing every predicate-separated choice and every `la2`.

## The lexer half

`std/regex`'s `lex` covers the regular part in one DFA — all keywords plus
identifiers in a single automaton, numbers, operators, character literals.
Four things are hand-written because a DFA has no business doing them: nested
`#[ ]#` comments, triple-quoted and raw strings, generalized string literals,
and the indentation/spacing bookkeeping that produces `indent` and `spacing`
on each token.

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

### Known gaps in the checker

* **Arity is not checked.** `primary` is called both bare and as
  `primary(mode)`; rule parameters need defaults, as Nim's do.
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
* The output is Nim; the real generator emits Nimony from the plugin.

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
