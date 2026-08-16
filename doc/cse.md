# Path-based invalidation for CSE

This document describes how CSE should decide *"can this event change what my
cached expression reads?"* — and argues that the answer must be built out of
**expression paths and nesting levels**, the way borrow checking already is, not
out of types and alias partitions.

The immediate motivation is the state of the type-based approach. Answering the
question with types requires, for every nominal type in a type slot, a walk of
that type's declaration; a pass that runs where the type world is incomplete then
needs a *second* implementation of that walk over the other world. That is
`src/hexer/pointertypes.nim` (268 lines over Nimony declarations) plus
`src/lengc/pointerbearing.nim` (281 lines over Leng types) plus two resolver
callbacks threaded through every entry point of the latter — and it grew that way
because `funcsummary` runs inside `hexer c`, which is the one pipeline stage that
sees only its own module's Leng output. Borrow checking answers a question of the
same shape with no type information at all, in about eighty lines.


## The question, stated once

CSE caches the value of a memory load and reuses it later. The cache entry is
valid until something writes the memory the load reads. Every hard part of CSE is
one predicate:

> Given a cached read `R` and a write `W` (a store, or a call that may store),
> can `W` change what `R` reads?

Borrow checking asks:

> Given a borrowed lvalue `B` and a mutation `M`, can `M` change what `B` names?

These are the same predicate. It is worth noticing that the safety analysis —
the one that must never be wrong — is the one that answers it *without* types.


## What borrow checking does

See `doc/language.md#borrow-checking` for the user-facing rules and
`src/nimony/contracts_fir.nim` for the implementation. Five lessons carry over.

**1. A path, not a type.** A borrow is a `seq[SymId]`: root, then one symbol per
field selector (`contracts_fir.nim:56`). `extractBorrowPath`
(`contracts_fir.nim:185`) peels the expression — `dot`, `at`, `conv`, `baseobj`,
`haddr`/`hderef`, a call's first argument, an inline temp's defining expression —
and appends a symbol per step. Nothing anywhere loads a type declaration.

**2. Overlap is a prefix test.** `pathsOverlap` (`contracts_fir.nim:277`)
compares element by element up to the shorter length: paths overlap iff one is a
prefix of the other. `a.b` and `a.b.c` overlap; `a.b` and `a.c` do not. Field
precision comes out of the *path length* — the nesting level — for free, with no
knowledge of what `a` is.

**3. Indices collapse; the path stays.** `a[i]` contributes `a` and nothing else:
"recurse into container, don't distinguish indices" (`contracts_fir.nim:214`).
Two different elements of one array are assumed to overlap. This is the one
deliberate imprecision, and it is what keeps the analysis free of arithmetic
reasoning.

**4. A path ends where the compiler stops being able to follow it.** A `deref` in
the middle of a path sets `NotBorrowable`; an explicit `addr` sets `HasAddr`
(`contracts_fir.nim:50-54`). The checker does not *guess* what a pointer points
to and does not consult the pointee's type: it marks the path as unfollowable and
takes the conservative branch. Where a path cannot be followed, the answer is
refusal, not a type-shaped estimate.

**5. What must cross a call boundary is declared, not inferred from types.**
`.establishesBorrow` (`contracts_fir.nim:168`) says "the result keeps aliasing the
first argument". At the call site itself, `borrowCheckForCall`
(`contracts_fir.nim:1000`) needs nothing but the argument *shapes*: an argument
wrapped in `haddr` is a mutable path, everything else is an immutable path, and
the check is `pathsOverlap` over those two lists.


## The model for CSE

### Paths

A path is a root symbol plus a list of selectors:

```
Sel = Field(SymId) | Elem | Deref
Path = (root: SymId, sels: seq[Sel])
```

`Elem` is the collapsed index (lesson 3): `a[i]` and `a[j]` both give
`(a, [Elem])`. `Deref` is a marker, not a step into anything known.

`accessRoots` in `src/lengc/shoggoth/aliasing.nim:89` already performs exactly
this walk over Leng and then throws the selectors away, keeping only the root.
Path extraction is that function with the selectors retained — the same peeling
of `DotC`, `AtC`, `DerefC`, `PatC`, `AddrC`/`HaddrC`, `ConvC`, `CastC`,
`BaseobjC`, and the same treatment of a call (union of operand paths).

### Levels

The **level** of a path is the position of its first `Deref`, or `∞` if it has
none:

* **Direct** (level `∞`): rooted in a symbol, reached only by field selection and
  indexing. `x`, `x.a.b`, `arr[i].f`.
* **Indirect** (finite level): the path passes through a pointer at that position.
  `p[]`, `x.next[].val`, `(deref (dot x p)) .a`.

Two facts follow from the IR alone, with no types:

* A direct path rooted at a **local whose address is never taken** cannot be
  reached by any indirect write, by any callee, or by any pointer at all. Nothing
  can point at it. (`cse.nim:661` `unreachableByCallee` argues exactly this
  today, but derives it from an alias partition instead of from the path.)
* Two direct paths overlap iff one is a prefix of the other. Their roots are
  symbols, and distinct symbols are distinct storage.

### Buckets

The cache is partitioned once, at insert time, by the path of the cached read:

| bucket | membership | killed by |
|---|---|---|
| `direct[root]` | direct paths, per root symbol | a write whose path overlaps (prefix either way); `addr`/`haddr` of that root moves it to `escaped` |
| `escaped[root]` | direct paths whose root has had its address taken | as `direct`, plus every indirect write and every call that can reach the root |
| `indirect` | every path containing a `Deref` | any indirect write; any call that writes indirectly |

Three rules, and none of them asks what a type contains.


## Kill rules

Let `W` be the path of a store's LHS and `R` the path of a cached read.

1. **Direct store, direct read.** Kill iff `overlap(W, R)` — prefix in either
   direction, comparing selectors pairwise, `Elem` matching `Elem`. `o.x = 1`
   leaves a cached `o.y` alive; today `invalidateMentioning` (`cse.nim:609`)
   kills it, because it tests "does the expression mention `o`" and nothing
   finer. Prefix exclusion is *more* precise than the current type-based code
   here, not less.
2. **Indirect store.** Kill the whole `indirect` bucket and every `escaped`
   entry. Leave `direct` untouched: no pointer can name a local whose address
   was never taken.
3. **`addr`/`haddr` of a root.** Move that root's entries from `direct` to
   `escaped`. (`&L` itself is not invalidated by taking `&L`, the existing
   `exempt` handling.)
4. **Call.** Kill:
   * every `escaped` entry whose root the callee can reach (argument roots,
     globals, previously escaped roots);
   * the `indirect` bucket, unless the callee's summary says it performs no
     indirect write;
   * for each argument the summary marks written, the entries whose path is
     prefixed by that argument's path.

   `direct` entries rooted at never-addr-taken locals survive every call. That is
   the case that matters in loops and it needs no summary at all.
5. **Unknown callee.** Rules 4 minus the summary refinements: kill `indirect` and
   all of `escaped`, keep `direct`.

Worked example — the shape this pass exists for:

```
while i < n:
  t = x.field[i]      # direct path (x, [Field field, Elem]) — cached
  p[] = t             # indirect store → kills `indirect`, not `x.…`
  o.other = 3         # direct store, path (o, [Field other]) — disjoint root
  f(y)                # y's root escapes; x is a local, never addr-taken → survives
  u = x.field[i]      # reuse
```

Every decision above is a prefix comparison on symbol lists. The type-based
pipeline reaches the same conclusions only after asking whether `x`'s type can
hold a pointer, which for an imported `x` means loading a declaration out of
another module.


## Calls, in path vocabulary

The summary `(smry …)` stays — lesson 5 says the information that cannot be seen
at the call site must be declared — but its contents change from a type-derived
partition to path facts read off the callee's body:

* per parameter: `reads`, `writesDirect` (the parameter's own slot),
  `writesIndirect` (a store whose path is rooted at that parameter and contains a
  `Deref`);
* whole proc: `writesGlobal`, `writesIndirect`, `callsUnknown`, `raises`;
* parameter classes, when two parameters were joined by an assignment inside the
  callee, computed by unioning parameter *indices* along assignments between
  paths — again a path fact.

The critical simplification: **"can this argument's graph be written?" is
answered by the callee's body, not by the argument's type.** Today the pass asks
whether a parameter's type can hold a pointer, as a proxy for whether a store
through it is possible. The body settles it directly: if the callee never stores
through a path rooted at parameter *i*, no type walk can make it write there; if
it does, the store is visible as a `Deref` in that path.

That single change is what removes the type world from `hexer/funcsummary`, and
with it `hexer/pointertypes.nim`, the two `ForeignSymResolver` /
`ForeignFieldResolver` hooks, and the entire type half of
`lengc/pointerbearing.nim`. The stage-1 placement of the summary pass stops
mattering, because nothing in it needs to resolve an imported *type* any more —
only the module's own body.


## Two answers, intersected

Paths and the alias partition are each a sound over-approximation of the same
predicate, and they are blind in *opposite* directions: the partition knows only
roots, so it cannot separate two fields of one object; a path deliberately stops
at a pointer, so it cannot separate two unrelated pointers. Intersecting two
over-approximations is still one (`A ⊇ T` and `B ⊇ T` ⇒ `A ∩ B ⊇ T`), so the
implementation kills a cache entry only when **both** analyses say the write may
reach it. That is why the type-based analysis stays exactly as `master` has it:
it is not scaffolding to be removed, it is the half that answers rule 2.

`cse.nim`'s self-tests pin both halves. `sibling_field_store_survives` is the
path half (a store to `x.other` leaves a cached `x.fld` alive; the partition
alone drops it), and `store_disjoint_survives` is the partition half (a store
through `qq` leaves a load through `pp` alive; the paths alone drop it).

## What is given up

Honest accounting:

* **Indirect reads still depend on the partition.** Nothing in a path
  distinguishes two pointers, so if the partition is ever removed, rule 2 becomes
  "an indirect store clears the indirect bucket" and loads through unrelated
  pointers stop surviving each other's stores.
* **Whole-aggregate copies.** `x = y` between two structs that both contain
  pointers creates aliasing with no syntactic `deref`. Path discipline does not
  try to prove anything about it: both `x.…[]` and `y.…[]` are indirect paths, so
  rule 2 covers them without a type test. This is precisely where the type-based
  design spent its complexity and where refusal is cheaper than proof.
* **Indices stay collapsed.** `a[i]` and `a[i+1]` overlap. Same trade the borrow
  checker makes; index reasoning belongs to the induction-variable pass, which
  already exists and can feed a stronger `Elem` when it proves two indices
  distinct.

Nothing here is a soundness risk in the dangerous direction: every rule kills a
superset of what a perfect analysis would kill, except rule 1, which is exact for
direct paths.


## What stays type-shaped

One use of `typenav` in CSE is legitimate and unaffected: `getType` to skip
value-CSE of large aggregates (`cse.nim`'s import comment). That is a *cost*
question about this module's own expression — is copying this value into a temp
cheaper than reloading it — not an aliasing question, it needs no cross-module
resolution, and it stays.


## What is implemented

`src/lengc/shoggoth/accesspaths.nim` (the vocabulary) plus the store and call
rules in `cse.nim`, both intersected with the partition as above.
`-d:cseTypeInvalidation` compiles the type/alias-class analysis alone, exactly as
`master` has it, so the two can be measured against each other; nothing else
changes between the two builds.

Measured on this machine, 25 modules of `nimsem` (the largest in the boot) and
four alternating 3-stage native bootstraps:

| | path (default) | type (`-d:cseTypeInvalidation`) |
|---|---|---|
| boot total | 19.11s / 19.70s | 19.65s / 19.47s |
| emitted `.oc.nif` | byte-identical across the two modes | — |
| `cse.N` temps over 25 modules | 398 | 398 |

With `-d:cseSummaryStats`, the path rule keeps **413** cache entries across
stores and **14** across calls that the partition alone would have dropped, out
of 8748 path tests. So the rule is live, but none of those survivals is followed
by a reuse of the same expression — hence identical output and identical time.
The reason is upstream: `scalarizer` explodes non-escaping local objects into
per-field scalars before CSE runs, which is precisely the shape rule 1 exists to
separate. The remaining field loads go through pointers, where the partition
answers.

Two ways to read that, both worth stating: the path rules cost nothing and are
the cheaper description of the problem, but on *this* workload they do not yet
pay for themselves — the case for them is the type machinery they let the summary
producer drop, not a speed-up here.

## Still to do

1. Rewrite the summary producer in `hexer/funcsummary.nim` in path vocabulary —
   "does the body store through a path rooted at parameter *i*" — which is what
   removes the type world from a pass that runs where it cannot see one.
2. Keep the `(smry …)` wire format's field names but drop the pointer-bearing
   derived bits; `doc/tags.md` needs the corresponding edit.
3. Re-measure with the SROA interaction in mind: if rule 1 is to earn its keep on
   compiler-shaped code, the input it wants is field loads that survive
   scalarization (large or escaping objects), so measure a workload that has
   them before concluding either way.
