# Multi-join IR

The backend works on two IRs:

- **Shoggoth IR**: what nimsem publishes for a module, and the input of
  `shoggoth` (passes in `src/hexer/`).
- **Arkham IR**: what `shoggoth` produces, which is
  [Leng](leng-spec.md). arkham, lengc and the other back ends read it.

"Multi-join IR" names how both IRs model control flow. There is no `while`,
`block` or `break`. A `loop` has a single back-edge, branching is `ite`, and
every other exit is a forward, scoped `(jmp L)` to a `(lab L)`. Any number of
jumps can join at the same label. That last property gives the model its name.

The code still calls the lowering that establishes this form "Final IR"
(`src/finalir/`, `TowardsFinalIr`, `FinalIrKind`, `phasePostFinalIr`). Those
are the same thing as the multi-join form.

This document is the design reference for that form and for the statement
discipline the passes follow.

The full specification — control-flow constructs (`loop`, `ite`, `case`,
`lab`/`jmp`, `try`/`finally`/`except`), exit summaries, the Side-PC-IR
scratchpad for destructor injection, and the IR as a structured assembler —
lives in <https://github.com/nim-lang/nimony/issues/1946>. This document
records the parts the compiler implements today, and *why* they are shaped the
way they are.

Implementation points:

- `src/finalir/finalir.nim` — the lowering to the structured control-flow form
  (`loop`/`ite`/`lab`/`jmp`). It runs in nimsem, as the module's last step
  (`semmain.lowerAndProve`): the contract and nil analysis
  (`src/nimony/contracts_fir.nim`) reads its output, and that same buffer is
  what nimsem publishes, so the whole backend starts from it.
- `src/hexer/xelim.nim` — the `Goal` enum; `TowardsFinalIr` is the mode
  `finalir.nim` runs `lowerExprs` in.
- `src/hexer/pipeline.nim` — the backend pass order.

---

## The normal form

Two rules carry almost all the weight. Everything below is a consequence.

1. **Control flow is statement-based.** `if`/`case`/`try` in expression
   position are lowered to their statement form; the branches assign into the
   expression's destination. `and`/`or` are if-expressions and go the same way.
   No control flow hides inside an expression.

2. **A `loop` has no condition slot.** A `while` whose condition is impure is
   rewritten to `while true:` with the condition as a *leading guard* in the
   body:

   ```
   while cond: body     ⇒     while true:
                                <statements computing cond>
                                if cond: body else: break
   ```

Together these give the property every later pass depends on:

> **Hoisting rule.** For any sub-expression of a statement, evaluating it in a
> new statement placed immediately in front of that statement is
> semantics-preserving.

There is no short-circuit region to escape from (rule 1), and there is no
position where "in front of the statement" means "outside a loop that the
expression is evaluated once per iteration of" (rule 2).

That rule is the whole point. It is what lets a pass that needs a temporary
*emit a statement* instead of fabricating one inside an expression.

## Constructors: `construct` regions

A constructor with a call (or any other complex value) among its values is not
an expression in the normal form. `xelim` splits it into stores into its
destination and wraps them in a `construct` region:

```
let x = Obj(a: f(), b: 2)    ⇒   (var x Obj .)
                                 (construct x (stmts
                                   (asgn (dot x a) (call f))
                                   (asgn (dot x b) 2)))
```

A constructor whose values are all pure stays an `oconstr`/`aconstr`/
`tupconstr`. Constants, static data and the optimizer's patterns over inline
constructors are unaffected.

### Why split, and why the wrapper

Every call becomes a statement that binds to its destination, which is what
the normal form wants of calls anyway (*Calls bind directly to their
destination*, below). Three problems with the constructor as an expression go
away with it:

- **Temps.** The value no longer needs a temp of its own to be evaluated in
  order. A 16 MB `default(array)` used to be built in a stack temp and copied
  into a heap object (nimony#2846).
- **Evaluation order.** A pass that hoists a pre-statement in front of "the
  statement" now hoists it in front of one field's store, not in front of the
  whole constructor, so the sibling fields keep source order. C's
  initializer lists are unsequenced; separate stores are not.
- **Analysis.** The prover sees a field value as an ordinary assignment, calls
  included.

The wrapper keeps what splitting would otherwise lose: that the stores, taken
together, write the whole object. A pass that wants to combine stores
(memidioms: a region of zero stores is one `memset`, a region copying from one
source is one `memcpy`) reads that off the tag instead of proving coverage.

### Rules

`(construct D (stmts …))`, where `D` is an lvalue, the **destination**:

1. **Fresh storage.** On entry `D` holds no value: it is a local, `result` or
   global declared without a value, the payload of a `(newobj T (noinit))`, or
   a location whose old value was destroyed. A store into `D` inside the region
   *initializes*: there is no old value to destroy and no self-assignment to
   guard against.
2. **Coverage.** The region is equivalent to `(asgn D (oconstr T …))` with the
   same values, so it inherits the totality rule of `oconstr` (`doc/tags.md`):
   on every path from the region's start to its end, every field of `D` is
   stored exactly once, inherited fields included. A field of aggregate type is
   covered by a store to the field itself or by a nested `construct` region on it.
   Array elements are `(at D i)`, tuple fields the tuple's field paths. A value
   that is control flow lands in its field on each arm:
   `Obj(a: if c: 1 else: 2)` becomes `(ite c (asgn D.a 1) (asgn D.a 2))`.
3. **No reads of `D`.** Nothing in the region reads `D` or a part of it. A
   reassignment whose values read the destination (`x = Obj(a: x.b, b: x.a)`) is
   built into a fresh temp first. Reaching `D` through a raw pointer while the
   region runs sees a partial object, as with `trySpliceAsgn`'s NRVO.
4. **Exits.** Control leaves the region only at its end. There is no `jmp`
   out of it, no `ret`, and no `raise`: a `.raises` value is bound to a temp
   *before* the region, so after `eraiser` the region contains no raise check.
   A `noreturn` call (a panic) may appear. A suspension point (`cps`) may
   appear; the frame keeps the partial object.
5. **Pre-statements.** Besides the stores, the region contains the statements
   that compute their values: temps, the checks a pass emits for a value, and
   control flow that stays inside the region. They follow the hoisting rule
   at the granularity of the store they belong to.
6. **After the region** `D` holds a value: it owns its fields and is destroyed
   like any other value of its type.

### What each layer does with it

- **`xelim`** (both goals) produces it, for a complex constructor in any
  position: a declaration's value, an `asgn`'s right-hand side, a `newobj`'s
  fields, a constructor nested in another (a nested region or field paths).
  In any other position (an argument, a `ret` value) the constructor goes into
  a fresh `var` temp, and the region fills the temp.
- **`newobj`.** `(newobj T (noinit))` allocates and leaves the payload
  uninitialized. The `construct` region on `(deref p)` that follows fills it. The
  duplifier emits the allocation and the out-of-memory check as before; the
  allocation now runs before the field values. In its output the heap block
  is `{rc, rootIdx?, data: payload}`, so it wraps the header stores and the
  payload's region, now on `(dot (deref p) data)`, in an outer region on
  `(deref p)`: the whole block is one region.
- **duplifier** treats a store in a region as an initialization (rule 1).
  Before a region that reassigns a live location it emits one `=destroy(D)`.
- **destroyer**: `D` becomes live at the end of the region (rule 6). Rule 4
  guarantees that no exit sees it half built.
- **`contracts_fir`** marks `D` initialized at the region's end, accepts stores
  into a `let` destination inside its region, and attributes a borrow stored
  into a field (`establishesBorrow`, views) to `D`'s root.
- **`intramodinliner`** splices a callee's `result` into a field-path
  destination, so `D.f = default(array)` writes into `D.f` directly.
- **`lengcgen`** keeps the tag: it is a Leng statement too
  (`doc/internals/leng-spec.md`). lengc prints the stores and ignores the
  wrapper. arkham may ignore it as well. It must route a call returning an
  aggregate into a field path through the hidden result pointer, at the field's
  address.

## `assume`

`(assume <cond>)` is a statement the lowering vouches for: it asserts nothing
and checks nothing, it only tells the contract analysis something it could not
have worked out on its own. One producer exists today — `forRangeAssumes` in
`finalir.nim`.

The reason it has to exist: an *inline* iterator is not inlined until hexer's
`elimForLoops`, long after `contracts_fir` runs, so the Shoggoth IR for a `for`
loop is a `(loop …)` that says nothing about the loop variable — it is not even
declared there. The iterator's `.ensures` does say something, and it is exactly
what the body needs:

```nim
iterator `..<`*[T: Ordinal](a, b: T): T {.inline,
    ensures: (a <= result and result < b).}
```

`trFor` substitutes the loop variable for `result` and the arguments for the
parameters, and emits one `(assume …)` per conjunct at the head of the loop
body (one per conjunct because the fact engine models a single comparison; a
conjunction as one opaque condition contributes nothing). Without it the most
ordinary contract in the language — `s[i]` under `for i in 0 ..< s.len` — could
not be discharged at compile time.

A conjunct that does not fit the grammar the substitution rebuilds is dropped
rather than approximated, and an `assume` the analysis cannot model contributes
no fact: an assumption is a statement of what holds, not an obligation, so
losing one costs precision and nothing else.

Two consequences of the same "not inlined yet" fact are worth knowing:

* Nothing jumps out of that `(loop …)`, so `trLoopFromBody` emits no exit
  label and **everything after such a loop is unreachable** as far as the Shoggoth
  IR is concerned. Analyses must not report on a dead path: the join that
  follows one keeps both arms of the next `if`, so contradictory facts hold at
  once there.
* The loop variable has no declaration to attach an init-state to.

## Why `xelim` used to run three times

`xelim` ("eliminate eXpressions") is the pass that establishes rule 1: it
turns `let x = if cond: 3 else: 4` into a temp plus a statement `if`, and it
hoists `(expr (stmts …) v)` — the statement-list-expression — out of
expression position.

The problem was that `xelim` was the *only* pass with a statement-insertion
point. Every other pass that needed a temporary had nowhere to put it, so it
built one in place:

```
(expr (stmts (var `tmp T <value>) …) `tmp)
```

— which is exactly the construct `xelim` exists to remove. So the pipeline
alternated between passes that broke the normal form and `xelim` runs that
repaired it:

```
… → eraiser → xelim1 → duplifier → xelim2 → destroyer → … → vtables →
constparams → xelim_final
```

`xelim2` did no lowering of its own. Its entire job was to flatten what the
duplifier had just built. The same held for the "after raises" half of
`xelim1` and part of `xelim_final`.

## How the nesting is avoided by construction

Give each pass the statement-insertion point it was missing, and the
`(expr (stmts …) v)` is never born.

The mechanism is small and identical in every pass:

- a `hoisted: TokenBuf` on the pass context, holding statements that must run
  *before* the statement currently being emitted;
- a `hoistTail(pos)` helper that relocates `dest[pos ..< ^0]` — a run of
  complete statements the pass just emitted — into `hoisted`;
- a flush at the statement boundary (`trStmts`), splicing `hoisted` in front of
  the statement that was just translated. The enclosing statement's own hoists
  are parked across the descent, so a `(if cond (stmts …))` puts the
  condition's temps in front of the `if`, not in front of the body's first
  statement.

A pass then writes

```nim
let pos = dest.len
# … emit `(var `tmp T <value>)` and whatever setup it needs …
hoistTail(c, pos)          # those statements move in front of the statement
dest.addSymUse tmp, info   # the expression keeps only the temp's name
```

The splice position is past every still-open tag, so it cannot disturb an
enclosing scope's bookkeeping: `closeTag` recomputes its jump from the buffer
length it sees at close time.

Two properties make this sound, and both are the normal form:

- The relocated statements land in front of the *enclosing statement*, which
  the hoisting rule says is a legal evaluation point.
- The relocation is exactly what the follow-up `xelim` run did anyway. Doing it
  at the point of creation is not a behaviour change; it removes a build-then-
  flatten round trip.

## Short-circuit conditions: the two-target condition compiler

`and`/`or` are if-expressions, so a naive lowering of a *condition* either
duplicates the arms (exponentially, for nested chains) or materialises the
boolean. `xelim` used to materialise:

```
(var `x bool)
(if (elif b (stmts <f(a)'s statements> (asgn `x …)))
    (else (stmts (asgn `x (false)))))
(if (elif `x <then>) (else <else>))
```

— a store immediately followed by a re-test of the same slot, and a second
diamond. Recovering the branch the source actually wrote takes jump threading
plus a proof that `x` is dead afterwards; that is work the optimizers were
being asked to redo at every `and`.

`xelim.trCondJump` is the **two-target condition compiler** (Appel's `Cx`)
instead. A condition is compiled against a target and a polarity — "transfer to
`(lab L)` exactly when this is `true`/`false`, otherwise fall through":

```
Cjmp(a and b)(T, true)  = Cjmp(a)(z, false); Cjmp(b)(T, true);  (lab z)
Cjmp(a and b)(F, false) = Cjmp(a)(F, false); Cjmp(b)(F, false)
Cjmp(a or  b)(T, true)  = Cjmp(a)(T, true);  Cjmp(b)(T, true)
Cjmp(a or  b)(F, false) = Cjmp(a)(z, true);  Cjmp(b)(F, false); (lab z)
Cjmp(not a) (X, p)      = Cjmp(a)(X, not p)          -- a target swap
leaf                    = (if <leaf> (jmp X))        -- polarity via `not`
```

`and` and `or` are exact duals and `not` is a target swap, so there is no
polarity asymmetry to hand-write. Short-circuiting is not a special case, it is
the layout: an operand's own statements are emitted *after* the guard that can
skip it, so they run only on the paths that reach them — which is why none of
`trAnd`/`trOr`'s hoisting gymnastics are needed on this path.

`if`/`elif` chains go with it. `trIfFlat` lays the whole statement out flat:

```
(scope Cjmp(c1)(L1, false) B1); (jmp Lend)
(lab L1); (scope Cjmp(c2)(L2, false) B2); (jmp Lend)
(lab L2); (scope B3)
(lab Lend)
```

Each merge is one `(lab)` with several `(jmp)`s into it — shared, never
duplicated — and the `elif` chain needs no nesting at all. This is why the
mechanism is `lab`/`jmp` and not lexically-scoped `break`: a merge that is not
an enclosing region's end needs no wrapper to name it, where the bracket form
would nest one wrapper per merge.

Measured on `tests/nimony/stdlib/tjson.nim` and its dependency closure the
generated Leng is 5.3% smaller than before this and the `xelim2` removal
(`syncio` −36%, `unicode` and `parseutils` −5.5% each).

### `lab`/`jmp` are Nimony statement tags

`(lab D)` and `(jmp Y)` were Leng-only (`lengcgen` produced them from
`block`/`break`). They are now `NimonyStmt` tags as well — `LabS`, `JmpS` in
`StmtKind`, plus `LabY` in `SymKind` for the label symbol — so the hexer passes
see them in their ordinary `case n.stmtKind` traversal instead of needing a
side channel. `lengcgen.trLab`/`trJmp` then map them one-to-one onto their Leng
counterparts.


### What a flat layout costs, and how it is paid

Flattening moves declarations out of the branch that used to contain them, and
a declaration in a flat region is reached by *some* paths and skipped by
others while the scope-exit `=destroy` still runs on all of them. Two rules
keep declaration, initialisation and destruction on the same set of paths:

1. **An arm's `(scope …)` spans its condition and its body together.** A
   condition may declare a local (`elif (let d = load(x); d.ok):`); entering
   the scope only on the paths that evaluate the condition is what keeps that
   local's lifetime honest. The guard's `(jmp Lnext)` leaves the scope, and
   `destroyer.trJmp` unwinds it on the way out.
2. **A conditionally-evaluated operand gets a `(scope …)` of its own.** Its
   temporaries must die with the condition, not with the arm: `if a or f():`
   has to destroy `f()`'s result before the branch body runs, which is what
   `tests/nimony/arc/tcontrolflow.nim` pins. The *leftmost* operand of a spine
   always runs, so it is emitted exactly as the old path emitted a condition —
   no extra scope, no hoisting.

Both rely on `destroyer.nim` treating `(scope …)` as a real destructor scope
(it used to be transparent) and on `trJmp` running the destructors of every
scope a jump leaves. The label's owning scope is found by a direct-children
scan (`collectLabels`) done when each scope is entered — sound because `jmp` is
forward-only and scoped, so a jump's target is always a `(lab)` in one of the
enclosing scopes' own statement lists.

The divergence bookkeeping around `jmp`/`lab` uses the shared
`lengc/shoggoth/trackers.nim` `Tracker` (`gotoLabel`/`landLabel`), the same one
`cse` and `copyprop` use, rather than a hand-rolled "jmp sets a bool, lab
clears it".

## Current pipeline

```
nimsem:   … → derefs → finalir → contracts → Shoggoth IR (published module)
shoggoth: iterinliner → desugar → lambdalift → eraiser → duplifier →
          destroyer → cps → vtables → constparams → xelim_final → lengcgen
          → Arkham IR (Leng)
```

Every pass from `finalir` on keeps the multi-join form, `lengcgen`
included: it spells `ite` as a Leng `if` and `loop` as `while true` (dropping
the trailing back-edge), because Leng's back ends and optimizer passes speak
those. Nimony `if`/`while`/`block`/`break` reaching it is a bug. Two producers
are not hexer passes proper and get the lowering by other means: the lifter
runs in `nimsem` too and so emits Nimony IR, and `pipeline.transform` lowers
its hooks with `toFinalIr`; an `{.assembler.}` body is taken verbatim except
that its `if`s are spelled `ite` (`finalir.trAsmStmt`).

- **`finalir`** establishes the normal form (it runs `xelim` in
  `TowardsFinalIr` mode). It is the only pass that *creates* it, and it runs
  in nimsem: a module is published lowered. A body that is re-sem'd elsewhere
  — a generic routine, a template — is published as sem left it.
- **`eraiser`** (`src/hexer/eraiser.nim`) emits the `canRaise` temp and its
  `ite failed(tmp): raise tmp` check as statements.
- **`duplifier`** (`src/hexer/duplifier.nim`) does the same for its owning
  temps — `bindToTemp`/`finishOwningTemp`, `trNewobj`'s decl + OOM check +
  payload assignment, and `genLastRead`'s bitcopy + `=wasMoved`.
- **`mover`** (`src/hexer/mover.nim`), the last-read analysis the duplifier
  asks before it sinks a value, is the one *analysis* in the backend and it
  reads the Shoggoth IR directly. It used to translate the module into
  `controlflow.nim`'s goto form first, which then had to be mapped back: a
  `srcMap` side-channel threaded through every buffer the CF builder moved or
  truncated, plus an inverted index to find the query expression again in the
  copy. All three are gone. What the goto form supplied — "what runs next" —
  now comes from one `parents` array (token position → enclosing tag) and a
  `(lab L)` table, both built once per module: `ite` forks, `loop` closes the
  back-edge, `jmp` is one lookup because it is forward and scoped. Reasoning
  on the same tree the duplifier rewrites also closes the gap the `AndX`/`OrX`
  note in `xelim.isComplex` describes, where the two forms could disagree
  about evaluation order.
- **`xelim_final`** is *not* a repair pass. `LowerCasts` performs two real
  lowerings: it unnests calls (the multi-join "calls are unnested statements"
  rule) and binds a cast's source and result to variables, which the Leng
  backends require. It does still flatten `vtables`/`constparams` temps as a
  side effect — see *Remaining work*. The `and`/`or` it materializes are
  spelled `ite` here, `if` in the `TowardsFinalIr` run (`openIfElse`).

`xelim2` is gone.

Measured on `tests/nimony/stdlib/tjson.nim` and its dependency closure, the
generated Leng got about 2.5% smaller (the largest module, `unicode`, by 5.4%):
the round trip left behind `(stmts …)` blocks that no longer appear.

## Remaining work

In rough dependency order:

0. **Value-position `and`/`or`** still materialises a bool via `trAnd`/`trOr`.
   That is inherent — the value has to land somewhere — and it costs no
   re-test, so it is not the case the optimizers struggled with. The
   `doc`-level refinement is *Value into an existing destination*: `x = a and
   f()` should assign into `x` on both arms rather than into a fresh bool.

1. **`vtables_backend.nim` and `constparams.nim`** still build
   `(expr (stmts …) v)` for their temps (five and two sites), as does the
   duplifier's `bindPendingMoves` — the one place that must run statements
   *after* the expression rather than before it, so it does not fit the
   `hoisted` shape as-is. Converting them leaves `xelim_final` with only its
   own two jobs.

2. **Call unnesting as part of the normal form.** Today `xelim1` runs in
   `ElimExprs` mode, where a call is not "complex" and stays nested. Multi-join IR
   wants calls unnested from the start — it is the precondition that makes
   conditions call-free and pins evaluation order. Moving that from
   `xelim_final` to `xelim1` is the next structural step, and it is the one
   with real consequences: the duplifier and destroyer would see one temp per
   call. The issue's answer is *Calls bind directly to their destination*
   (`(asgn dest (call f args))` stays legal, so no temp is minted where the
   call already sits at its consumption point) plus `lastUse` (a liveness
   assertion emitted by construction on flattening temps, so no liveness
   analysis is needed for them).

3. **Structured control flow in the backend.** `finalir.nim` already lowers to
   `loop`/`ite`/`lab`/`jmp`, but `lengcgen` still spells them as Leng `if`/`while true`
   for the back ends. Having them consume `loop`/`ite` directly is what
   unlocks the exit-summary analyses (one forward pass, no CFG, no fixpoint
   except at loop headers).

4. **A well-formedness verifier.** The invariants above are currently upheld by
   construction and checked only by the test suite. A verifier that rejects a
   `(expr (stmts …) v)` after `xelim1`, and a `lab`/`jmp` that is not forward
   and scoped, would make a violation fail at the pass that produced it rather
   than at the pass that trips over it.

## Regression guard

`tests/nimony/arc/tandor_condjumps.nim` pins the two-target lowering:
short-circuit order across a nested `(a or b) and c`, a `let` declared inside a
condition operand and used in the arm body, and a destructible value built
inside a condition (built once, destroyed once).
`tests/nimony/arc/tcontrolflow.nim` pins where that destruction happens.

`tests/nimony/arc/twhile_cond_temp.nim` pins rule 2. A `while` condition
calling a proc that returns a value with a destructor needs an owning temp; if
the condition stays in the `while`'s slot, that temp is hoisted out of the loop
and the condition is evaluated once. The test counts the evaluations.
