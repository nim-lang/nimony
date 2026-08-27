# NIF rewrite engine — NFA/DFA design

A `lexim`-style pattern matcher, but the alphabet is **NIF tokens** instead of
characters. It replaces the compile-time macro matcher (`rewriter.nim`) with a
runtime engine: rules → NFA → **DFA** → (bytecode VM today, generated code
later). Built on `nifcore` (`TagLit` with a `jump`; no `ParRi`).

The pipeline:

```
  .rewrite.nif  ──parse──►  pattern trees
                ──linearize──►  strings over the NIF alphabet
                ──Thompson──►  NFA  (per root tag; rules ε-merged)
                ──subset──►  DFA   (+ tagged captures, ordered accepts)
                ──emit──►  bytecode  (VM)   ── or ──►  Nim / asm (codegen)
```

Status: iteration 1 ships the degenerate case — one rule = one linear program,
dispatched by root tag, tried in priority order (`vmrewriter.nim`). This doc is
the target the merge step builds toward; the bytecode/VM is already shaped to be
the DFA's back end.


## 1. The alphabet

A tree pattern is matched against a *child sequence*. Each automaton symbol
consumes **exactly one child** of the current scope — that uniform advance is
what makes the classic automaton constructions valid over trees.

| symbol        | matches                              | runtime advance        | captures |
|---------------|--------------------------------------|------------------------|----------|
| `Open(tag)`   | child is `TagLit` with `tag`         | descend (push scope)   | no       |
| `End`         | scope exhausted (no token)           | pop scope              | no       |
| `Int(v)` `Dot` `SymLit(s)` | concrete leaf            | `inc` (head width)     | no       |
| `AnyInt` `AnySym` `AnyLit` | typed leaf class         | `inc`                  | yes      |
| `Wild`        | any subtree (`(any X)` / bare ident) | `skip`                 | yes      |
| `Pure`        | any *pure* subtree (`(pure X)`)      | `skip`                 | yes      |
| `Same(slot)`  | subtree structurally `== caps[slot]` | `skip`                 | no       |

`Wild`/`Pure`/`Same` are the **non-regular features handled as special edges** —
exactly lexim's trick for `\1`/captures. The automaton treats them as ordinary
one-step symbols; the *variable-width* consumption (`skip` a balanced subtree,
`subtreeEqual`, `isPureSubtree`) happens in the VM, never in the automaton. So
the automaton stays regular even though trees are not.


## 2. Linearization

Pre-order with explicit `Open`/`End` turns a tree pattern into a string:

```
  (add T X 0)            →  Open(add) Wild Wild Int(0) End
  (mul T (pure X) 0)     →  Open(mul) Wild Pure Int(0) End
  (not (not X))          →  Open(not) Open(not) Wild End End
```

`End` is synthesized, not a token (nifcore has no close paren); the VM emits the
`End` event when a scope's body is exhausted (tracked via `cursorJump`).


## 3. NFA

Thompson construction over the alphabet. Today's patterns are finite, so each
NFA is a chain; the construction is where `|` (alternation) and `*`/`+` (Kleene,
for future variadic `args*`) plug in unchanged.

All rules sharing a root tag are merged: an ε-branch from one start state into
each rule's chain. Each rule's final state is tagged with its **rule index**;
priority = lowest index wins (lexim's `toRules`).


## 4. The one real hazard: descend vs. skip

`Open(tag)` advances by the head (descends); `Wild` advances by the whole
subtree (`skip`). If at the *same scope position* rule A has `Open(tag)` and
rule B has `Wild`, then on the same input (a tag node) the merged automaton
would need two states that have consumed **different amounts of input** — which
a single DFA state (one cursor position) cannot represent.

**Resolution — separate horizontal from vertical.** Make the DFA per *scope*
(the sibling sequence), and treat descending as a **push to a sub-automaton**,
not a transition competing with `Wild`:

- A scope's DFA only ever advances one **sibling** at a time. At that level
  `Open(tag)` and `Wild` both consume exactly one sibling → uniform, no hazard.
- `Open(tag)` additionally *calls* the child tag's sub-DFA (push the scope
  stack); on `End` it returns and the parent DFA resumes.

That is a **deterministic pushdown tree automaton**: a stack of per-scope DFAs.
The VM already carries the scope stack; the merge step swaps the per-rule linear
programs for per-scope merged DFAs.


## 5. NFA → DFA (subset construction) + captures

Standard ε-closure + move over the per-scope alphabet. Finite patterns yield a
trie; Kleene yields a genuine DFA. Optional Hopcroft minimization per scope.

**Captures = tagged DFA (TDFA).** The hard part of merging: two rules may bind
the same matched child to different slots, or one captures where another does
not. A plain transition can't carry both. So each transition carries
**register operations** — "write capture register *k* := current cursor" — and
each accept resolves registers → the winning rule's named captures.

For our anchored, loop-free patterns this stays simple: captures are positional;
a transition either writes register *k* or doesn't. Where two rules disagree on
the capture action for the *same* input, keep those states distinguished —
i.e. **fold the capture-action (and the accept-rule) into the state identity**
during subset construction / minimization, exactly as lexim folds the accept
rule into state identity. A little less merging, full correctness. (Full
generality = Laurikari/RE2-style tag tracking.)

**Accepts are ordered.** A DFA accept state may stand for several rules. `when`
predicates are *not* in the automaton; they run at accept time. So an accept
carries an **ordered list** of `(rule, conditions)`, tried by priority; if a
rule's `when` fails, fall through to the next candidate (extends lexim's single
`toRules` per state). Match commits on the first rule whose predicates pass.


## 6. Why DFA before the VM

A VM *could* walk the NFA directly (tracking a state set), but we translate to a
DFA first:

- **No backtracking, deterministic dispatch.** Each state has one outgoing move
  per input class; matching is O(input).
- **Codegen-friendly — the real reason.** A DFA maps one-to-one to generated
  code: state → labeled block, transitions → compares + `goto`, accept →
  emit-RHS. That is precisely lexim's `genMatcher`. An NFA would need runtime
  set-tracking, which doesn't lower to clean code. So the **DFA is the stable IR
  between front (patterns) and back (VM *or* codegen)** — the same DFA feeds the
  bytecode VM now and a Nim/asm emitter later, with no change to the front end.


## 7. Runtime model (VM over the DFA)

State: cursor `c`; scope stack of `(rem, width)` frames; capture registers
`caps[]`. Per DFA state:

1. inspect the current child;
2. pick the matching transition, **most specific first**
   (`Int(v)` > `AnyInt` > `Wild`; `Open(tag)` > `Wild`);
3. apply its advance — `inc` (atom), `skip` (wild/pure/same), or descend (Open);
4. apply the transition's capture register-ops;
5. `goto` dest. On `End`, pop the scope (deduct subtree width from the parent
   frame); on accept, run the ordered `when` checks and emit the RHS of the
   first passing rule.

`skip`, `subtreeEqual`, `isPureSubtree` are the special-edge calls; `cursorJump`
/ `subtreeWidth` drive the scope stack so subtree boundaries need no close token.


## 8. Codegen future

The same DFA lowers to straight-line Nim (a `case state` with gotos, lexim
style) or machine instructions. The scope stack remains a small runtime array;
special edges become calls. RHS construction stays `nifcore` building
(`openTag`/`addSubtree`/`closeTag`), with line info preserved (rebuilt tags
re-`appendLineInfo`; spliced captures keep their source info; synthesized nodes
take the matched node's info).


## 9. Out of scope (later)

- Kleene/variadic patterns for the "cheap vectorizer" idioms (`(call f args*)`).
- A canonicalization pre-pass (local/param names only) so structural exactness
  carries the safety argument without aliasing/stride analysis.
- Wiring into `shoggoth.nim`; a matmul/elementwise → intrinsic example.
