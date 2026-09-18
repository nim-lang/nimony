# Eliminating run-time checks

The contract pass (`src/nimony/contracts_fir.nim`) decides which run-time checks
are needed, so it also records that decision in the module it publishes. The
backend then emits a check only where the pass left one owed.

Two kinds of checks are affected:

- **Index checks.** `(arrat arr idx hi [lo])` becomes `nimIcheckAB`/`nimIcheckB`
  (and the unsigned pair) in `desugar` and `lengcgen`.
- **`.requires` guards.** `desugar.trRequires` turns a routine's `.requires`
  into an `if not cond: panic` in the callee's prologue. Note that `s[i]` on a
  `seq` or `string` is a `.requires` call, not an `(arrat …)`, so this is where
  most of the benefit is.

Implementation points:

- `src/finalir/finalir.nim`: the lowering to the Final IR.
- `src/nimony/contracts_fir.nim`: the prover; `applyVerdicts` writes its
  verdicts into the lowered module.
- `src/nimony/semmain.nim`: `lowerAndProve` lowers, proves and publishes.
- `doc/internals/final_ir.md`: the Final IR itself.

---

## The published module is the Final IR

`semmain.lowerAndProve` lowers the module right after `derefs`, runs the prover
on the lowered buffer, and publishes that same buffer. There is one artifact per
module, not two: a second one would duplicate the index, the staleness rules and
the dependency graph.

What gets lowered follows one rule:

> **Lower everything except what is re-sem'd somewhere else.** Generic routine
> bodies, template bodies and concept bodies stay in Nimony IR.

These are copied, substituted and sem'd again by the module that uses them, so
lowering them would make sem accept `ite`/`loop`/`jmp` as input. The prover
skips the same set for the same reason. The lowering already passes a
non-concrete routine through verbatim, so the rule needed no extra work.

The prover's `kill`/`unknown` facts are stripped before the write
(`finalir.stripAnalysisFacts`); the backend derives destruction from the `scope`
tags. The validator checks the published module against `phasePostFinalIr`:
the post-sem tags (for unlowered generic bodies) plus the Final IR's own.

Consumers of the published module:

- `indexgen` and `idetools` read top-level declarations and token positions,
  which the lowering leaves alone.
- `renderer` stays Nimony-shaped; diagnostics already rendered lowered trees.
- `semvalidator` needs branches as written, so it reads `<mod>.sem.nif`, which
  `--keepsemtree` writes. The compiler itself never reads that file.
- `exprexec` compiles a program through the normal path, and `expreval` only
  folds constant expressions during sem. Neither sees the lowered form.

Hexer reads the Final IR throughout and lowers nothing at its entry. The one
exception is the hooks the lifter creates in hexer, which it lowers with
`toFinalIr(analysisFacts = false)`. `lengcgen` turns `ite`/`loop` into Leng
`if`/`while true`, because the native back end and most optimizer passes do not
know Leng's own `ite`/`loop`.

## `for` stays in the Final IR

A `for` is lowered to

```
(for <iterCall> <vars> (scope <body> (continue .)))  (lab exit)
```

Only the body is lowered, exactly like a `loop` body: it ends in its only
back-edge, and `break` is a forward `(jmp exit)`. The exit label is emitted even
when nothing jumps to it; otherwise everything after the loop would look
unreachable to the prover.

Iterator inlining stays in hexer (`iterinliner`). Because the body is not
inlined when the prover runs:

- the prover analyses the iterator call's arguments at the `for`;
- `declareForVars` declares the loop variables as initialized, and a `var T`
  or `lent T` binder as a borrow of the iterator's first argument. A binder is
  exempt from the `let`-reassignment check, because tuple unpacking and a
  closure iterator's resume assign to it;
- `forRangeAssumes` emits `(assume …)` for what the iterator's `.ensures`
  promises, which is how `s[i]` under `for i in 0 ..< s.len` is proven.

### Why late inlining is sound

A check decided by the contract pass belongs to the module the code was
**defined** in and travels with the code. So when hexer inlines an iterator
body elsewhere:

- a body from a lax module inlined into a strict one keeps its checks;
- a body from a strict module inlined into a lax one stays clean;
- anything a later pass creates is checked, since nothing downstream removes
  checks.

## Scopes and `continue`

Branch and loop bodies are `(scope …)`. `destroyer` treats a `scope` as a
destructor scope and a `stmts` as transparent, and with the flat `lab`/`jmp`
layout a branch body is a sibling in the enclosing statement list, so a `stmts`
would let a branch-local live to the end of the enclosing region. A
statement-position `stmts` stays transparent: `{.keepOverflowFlag.}: let x = …`
and every declaration `xelim` hoists are used after it.

A source-level `continue` becomes a `jmp` to a label in front of the back-edge,
so `(continue .)` is always the last statement of a loop body. If the body has
such a `continue`, the body gets a scope of its own and the label goes *after*
it: a `jmp` may leave a scope, but it must not skip a declaration inside one,
or the scope's end would destroy an uninitialized value
(`tcontinue_skips_decl`).

## `.requires`: function duplication

Every top-level routine with a `.requires` becomes two:

- the **wrapper** keeps the routine's own symbol and header, so `desugar` still
  turns its `.requires` into the guard. Its body forwards every parameter:

  ```
  (result :r T .) (asgn r (call f`body p1 … pn)) (ret r)
  ```

  This is the shape sem already produces for a one-call routine, so no pass
  needed changes. Shoggoth's `tailcalls` turns it into `(ret (call …))`.
- the **body** (`bodyOfRequires`, named `derivedName(stem, "body")`) carries the
  contract as `(assume …)`, so no guard is emitted for it. Its parameters get
  fresh names (`freshBodyName`); the wrapper keeps the originals because the
  guard's panic message names them.

The call-site verdict picks the symbol:

| verdict | emits |
| --- | --- |
| proven | a call to the **body** |
| unprovable, run-time contracts | a call to the **wrapper** |
| unprovable, `staticContracts` | an error |
| disproven | an error |

### Why not a guard at the call site

A call site exists only for a resolved direct call. A proc value, a closure, a
vtable slot, a hook or a callback stored in a field has none, so the contract
would have to live in the proc type to cover them. With the split, anything
that is not a resolved direct call can only name the wrapper, which has the
guard. The guard also stays in one place instead of being copied to every call
site.

### Rules

- **The body's name is derived by a fixed rule**, so a module that proves a
  call to an imported routine can name the body without a lookup.
- **The wrapper must be cheap.** In practice the inliner folds it into its
  unproven callers and dead-code elimination drops it.
- **Generics split per instance**, in the module that creates the instance,
  which is also where instances are judged
  (`tests/nimony/contracts/tstrictinstances.nim`).
- **A proven recursive call goes to the body.** The prover starts a routine's
  analysis with its own `.requires` as facts, so such calls are provable.
- **Not split:** a `method` (a call to it is the dispatch), an iterator, a hook,
  a routine without a body of its own, and a routine nested in another (its
  copy would have to exist in both the wrapper and the body of the enclosing
  routine).

## `arrat`

There is no callee to split, so the decision stays at the site. A proven index
loses its bounds: `(arrat a i)`, or `(arrat a i . lo)` for an array that does
not start at zero (NIFC arrays do, so `lo` is still subtracted). For an index
that keeps its bounds, `desugar`/`lengcgen` emit the check. The prover marks
rather than emits because emitting means hoisting a call into statement
position, which inside an `and`/`or` operand needs the short-circuit handling
`desugar` already has (`trShortCircuit`).

## Lowering invariants

The lowering's idiom `n = sub(n); <read children>; n = xStart; skip n` does not
check that every child was read, so anything unexpected would disappear
silently. `trIf`, `genIfViaCx`, `trCase` and `trTry` therefore call `bug` on a
child they do not expect. `xelim` nests every `elif` chain into one `elif` plus
`else`, which `trIf` relies on.

Every statement kind that reaches `trStmt` has its own branch.
`-d:firFallbackProbe` prints the statements that fall through to the
operand-lowering fallback; nothing does today.

An `{.assembler.}` body is passed through verbatim (its `if`s spelled `ite`),
and the prover skips it.

## Results

Over `tall`: 1158 of 1279 index obligations and 3851 of 6260 `.requires` call
sites are proven. Against the same compiler without the redirect to the body,
the `matmul` and `nifbench` kernels run 1–6% faster and the generated C is 1.6%
smaller, at the same compile time. Publishing the lowered module grows the
`tjson` closure's nifs by 11.8%.

## Remaining work

1. **Remove the `activeChecks` path.** `--boundchecks:off` and `-d:danger` are
   still a backend flag: `desugar` and `lengcgen` use `activeChecks` to decide
   whether an owed check is emitted at all. The flag should become a third
   contract feature next to `runtimeContracts` and `staticContracts`
   (`features.nim`), under which the prover drops every obligation itself. Then
   `CheckMode`, the `activeChecks` fields and the `--flags` handling can go, as
   can `trRequires` and its helpers in `desugar`. The split can then depend on
   the build mode too: with checks off, no wrapper is needed. `RangeCheck`
   emits nothing today; it either goes as well or gets connected to the
   prover's `checkRangeAssign`.
2. **`was`**: a way to map lowered code back to its source shape, added one
   consumer at a time, error messages first. Notes:
   - In-module macros never see the Final IR: expansion happens during sem,
     before the lowering. `was` is for reflection on imported bodies,
     `renderer.asNimCode` and idetools.
   - `try` and `case` keep their shape. What loses it: `while` (the condition
     becomes a guard in the body), `if`/`elif` chains, `and`/`or`, `break L`,
     `block` and its name, and `xelim`'s temps.
   - `(was STR)` already exists as a LengPragma taking a string. The new one
     carries a tag, so widen that one on purpose or pick another name.
   - A pass that rewrites inside a `was` region drops the annotation, and the
     validator enforces that: a stale `was` is worse than none.
   - Lowering less to need less `was` is not an option: the flat `elif` layout
     is where the Leng size win came from (`final_ir.md`, *What a flat layout
     costs*).
3. **Leng `loop`.** The Leng consumers read the infinite `(loop body)` form, but
   nothing produces a Leng `loop` yet, and `continue` is not a `LengStmt`, so
   its back-edge cannot be written on the Leng side.

## Open questions

- A `.requires` that cannot be checked at run time has no guard to put in a
  wrapper. Such a routine needs no wrapper, and therefore has no safe indirect
  use.
