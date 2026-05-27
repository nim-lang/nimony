# arcopt - ARC specific optimizer

## Goal

Eliminate `=wasMoved + =destroy` pairs to nops when they are equivalent to no-ops.

This is the motivating use case for a small post-lowering IR that keeps optimizer
passes as simple tree traversals.

### The shape we want to handle

The optimizer must eliminate the pair in branched code:

```nim
if cond:
  `=wasMoved`(x)
else:
  `=wasMoved`(x)
`=destroy`(x)
```

Here every path to `=destroy(x)` is preceded by `=wasMoved(x)`, so the entire
trio collapses to nothing.

It must *not* eliminate when a use sneaks in:

```nim
`=wasMoved`(x)
use(x)         # observes the wasMoved state
`=destroy`(x)
```

(Nim 2's lifetime tracking already does the straight-line case. The branched
case is the one that benefits from a small dedicated IR.)

## Design goals

The IR ("TJ", for *tame jumps*) is designed around two principles:

1. **Optimizer passes stay tree traversals.** No CFG construction, no fixpoint
   iteration. The IR's nesting *is* the control-flow scaffolding.
2. **Simplicity.** Fewer concepts than [NJVL](njvl-spec.md): no `cfvar`, no
   monotonic-boolean discipline, no encoding of `return`/`break`/`raise` as
   data flow.

We build TJ on top of NIFC, which has already lowered `try`/`finally` for us.

## Why not NJVL's `cfvar`s?

NJVL transforms control flow into data flow: `return`/`break`/`raise` become
assignments to monotonic boolean `cfvar`s, then guards (`if cfvar: ...`) gate
the rest of the code. This has real charm — every path joins cleanly, and the
SSA-style versioning that follows is uniform.

In practice it has not paid for its complexity:

- Reasoning about "is this `cfvar` already set on this path?" reintroduces the
  control-flow analysis we were trying to avoid.
- Inserting destructors and reasoning about move semantics in cfvar-guarded
  trees is harder than expected.
- The `jtrue → goto` lowering at the end is sensitive ("only valid if no
  interim statements occur"), which limits the optimizations that can run on
  cfvar form.

TJ takes the other branch: keep `goto` and label, but constrain them enough
that tree traversal still works.

## The TJ IR

TJ keeps loops, `if`, and `case` as structured nodes. It adds three things:

- **Forward `goto`s.** A `goto L` always targets a label that appears later in
  the same enclosing structured construct. No back-edges, no jumps out of
  enclosing blocks beyond what the structure already permits.
- **First-class labels with arity.** A `label N L:` declares that `L` is a join
  point with `N` incoming edges from `goto`s. Because loops are still explicit,
  *every* label is a forward-only join.
- **Basic-block IDs at implicit joins.** After an `if` or `case`, the join is
  implicit; we tag each branch with `(bb K)` and write `(join K1 K2 ...)` so
  the join knows exactly which branches contribute. Branches that fall through
  to a `goto` do not appear in the join list.

The two complement each other: explicit `goto`/`label` for `break`/`return`
shaped control flow, `(bb)`/`(join)` for the implicit merges after `if`/`case`.

### Example: `break` becomes a forward `goto`

```nim
while n.kind != ParRi:
  if i > 10:
    break
  inc i

echo "after loop"
```

becomes

```nim
loop:
  if n.kind == ParRi:
    goto Lend
  else:
    if i > 10:
      goto Lend
    inc i

label 2 Lend:        # two incoming edges
  echo "after loop"
```

The label's arity (`2`) lets a tree traversal decide locally whether a
transformation is sound — it knows up front how many predecessors a join has
without walking the tree to count them.

### Example: `(bb)` and `(join)` for implicit joins

```nim
while true:
  case n.kind
  of A:
    (bb 1)
    break
  of B:
    (bb 2)
    handleB()
  of C:
    (bb 3)
    handleC()

  (join 2 3)         # case's implicit join: only B and C fall through

# the only way to exit the loop:
label 1 Lend:        # single incoming edge: the break from branch A
  ...
```

The `(join 2 3)` makes the implicit merge after `case` explicit and listable:
branch `1` (`A`) jumped away and does not contribute. Any pass that needs to
know "what holds at this point?" can intersect the per-branch state for the
listed `bb` IDs.

### "All paths exited"

The same encoding handles the case where every branch jumps:

```nim
while true:
  case n.kind
  of A:
    (bb 1)
    break
  of B:
    (bb 2)
    return
  of C:
    (bb 3)
    raise SomeError

  (join)             # empty join list: nothing falls through to here
```

An empty `(join)` is dead code marker — useful as a sanity check and as input
to dead-code elimination.

## How the wasMoved+destroy pass uses this

The pass is a single forward tree walk that maintains a small per-variable
state: *did we see `=wasMoved(x)` on the current path?*

- At a `(bb K)`, snapshot the current state under key `K`.
- At a `(join K1 K2 ...)`, intersect the snapshots for the listed `bb`s and
  install the result as the post-join state. Missing `K`s (branches that
  jumped away) contribute nothing — they are not on this path.
- At a `label N L:`, intersect the states recorded by each `goto L` that
  reached it (we know there are exactly `N`, and we collected them on the way
  down).
- At `=destroy(x)`, if `x` is in the "moved" set on every contributing path,
  drop the `=destroy` and mark each contributing `=wasMoved(x)` as a no-op
  using NIF's dot mechanism (no buffer reallocation).
- Any *use* of `x` clears the bit; any new assignment to `x` clears the bit.

No CFG, no fixpoint, just a tree walk with a small intersect at joins. This is
the property NJVL was designed to give us, achieved without the cfvar layer.

## `kill` and register allocation

TJ is the IR we hand to native code generation, so the register allocator
needs precise lifetime information. We borrow [NJVL's `kill`](njvl-spec.md):

> `kill(x)` marks the end of `x`'s storage lifetime. It is distinct from
> `=destroy(x)`, which is an actual cleanup call emitted only for non-trivial
> destructors.

After destructor injection, a non-trivial variable typically appears as
`=destroy(x); kill(x)` at scope exit. arcopt may elide the `=destroy(x)`;
the `kill(x)` always stays, because storage still dies.

### Pass ordering

```
destructor injection  →  arcopt  →  kill hoisting  →  codegen
```

This ordering matters:

- **Destructor injection** places one `=destroy(x)` per scope, at scope exit.
  This is the property NJVL specifically argued for: O(n) code with multiple
  resources, not O(n²). Expensive destructors (network/DB/transaction cleanup)
  appear exactly once.
- **arcopt** runs on this form. Its branched-case analysis (§ *How the
  wasMoved+destroy pass uses this*) is exactly the cross-branch reasoning
  the single-`=destroy` form needs. Without this ordering arcopt would face
  multi-kill complications that have nothing to do with its job.
- **kill hoisting** lifts each `kill(x)` (and any surviving `=destroy(x)`
  paired with it) upward to the variable's actual last use, possibly splitting
  it across branches. This is what register allocation wants for precise
  live ranges.

### arcopt does not care about `kill`

`kill` is opaque to arcopt: it is not inspected, does not affect the
per-variable moved-set, is not rewritten. arcopt's only job is the
`=wasMoved + =destroy` pair. `kill` rides through unchanged.

### Multi-kill is a post-arcopt phenomenon

Before hoisting, every variable has exactly one `kill(x)` site (its scope
exit). After hoisting, a variable can have multiple `kill(x)` sites — one
per branch where its live range ends — and the IR must satisfy a
**join-consistency invariant**:

> At every join (label or `(bb)/(join)`), the live-out set must be consistent
> across all contributing branches. If `x` is dead at the join, every
> contributing branch must contain a `kill(x)` before the join.

The simplest enforcement is symmetric hoisting: the hoister inserts a `kill`
in any branch that didn't otherwise need one, so post-join liveness is
uniform. The label-arity field gives a verifier the count it needs to check
this locally, in one tree pass.

Because arcopt runs before hoisting, none of this affects its design. A
verifier and regalloc-prep pass downstream of arcopt handle the multi-kill
shape; they share the same `(bb)/(join)`/label-arity machinery as a forward
tree walk over a small live-set state.

## Relationship to NIFC and migration path

TJ does not need to be a separate IR file format. The "forward jumps only"
property can be retrofitted into NIFC, which already houses the optimizer
pipeline in [opt_unused/](../src/nifc/opt_unused/). What we add to NIFC:

- A `label` form with an explicit arity.
- A rule that every `goto` targets a label that appears later in the same
  enclosing structured construct (verifiable in linear time).
- `(bb K)` markers inside branches of `if` / `case`, and `(join K...)`
  immediately after, listing the contributing `bb`s.

Existing NIFC passes that already assume structured nesting (`copy_propagation`,
`cse`, `induction_variables`, `inlining`) keep working unchanged; the new
arcopt pass simply uses the extra annotations. Try/finally has already been
lowered by the time we reach NIFC, so the `goto`/`label` shape covers
`return`/`break`/`raise` uniformly.

## What TJ deliberately omits

Versus NJVL, TJ does not include:

- `cfvar`/`jtrue` — replaced by `goto`/`label`.
- Location versioning (`v`, `u`, `either`, `split`) — out of scope for arcopt.
  If we later want SSA/SSU for CSE or move analysis, it can be added as a
  separate annotation layer on top of TJ, the same way NJVL stacks `vl` on
  `nj`.
- `unknown`, `assume` — also separate concerns.

`kill` is included (needed for register allocation, see above) but is opaque
to arcopt itself. TJ is the smallest extension that makes the branched
`=wasMoved`/`=destroy` elision a tree traversal while still serving as the
IR we hand to native codegen.
