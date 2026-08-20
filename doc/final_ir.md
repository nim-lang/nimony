# Final IR

Design reference for the target form of the hexer pipeline, and for the
statement discipline the pipeline already follows on the way there.

The full specification — control-flow constructs (`loop`, `ite`, `case`,
`lab`/`jmp`, `try`/`finally`/`except`), exit summaries, the Side-PC-IR
scratchpad for destructor injection, and Final IR as a structured assembler —
lives in <https://github.com/nim-lang/nimony/issues/1946>. This document
records the parts the compiler implements today, and *why* they are shaped the
way they are.

Implementation points:

- `src/njvl/finalir.nim` — the lowering to the structured control-flow form
  (`loop`/`ite`/`lab`/`jmp`). Currently reached from `src/nimony/contracts_fir.nim`
  (contract and nil analysis), not yet from the backend pipeline.
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
desugar → lambdalift → xelim1 → eraiser → duplifier → destroyer → cps →
vtables → constparams → xelim_final
```

- **`xelim1`** establishes the normal form. It is the only pass that
  *creates* it.
- **`eraiser`** (`src/hexer/eraiser.nim`) emits the `canRaise` temp and its
  `if failed(tmp): raise tmp` check as statements. It moved *behind* `xelim1`
  in the process: it used to run first precisely because it needed a repair
  pass after it.
- **`duplifier`** (`src/hexer/duplifier.nim`) does the same for its owning
  temps — `bindToTemp`/`finishOwningTemp`, `trNewobj`'s decl + OOM check +
  payload assignment, and `genLastRead`'s bitcopy + `=wasMoved`.
- **`xelim_final`** is *not* a repair pass. `LowerCasts` performs two real
  lowerings: it unnests calls (the Final-IR "calls are unnested statements"
  rule) and binds a cast's source and result to variables, which the NIFC
  backends require. It does still flatten `vtables`/`constparams` temps as a
  side effect — see *Remaining work*.

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
   `ElimExprs` mode, where a call is not "complex" and stays nested. Final IR
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
   `loop`/`ite`/`lab`/`jmp`; the backend pipeline still emits `while`/`break`
   and lets `togoto.nim`/NIFC deal with it. Switching the backend over is what
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
