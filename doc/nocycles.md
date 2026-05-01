# Static Cycle Prevention in Nimony

## Motivation

Runtime cycle collection (YRC and friends) is expensive and complex.
For a wide and useful subset of programs we can eliminate cycle collection
entirely by **statically forbidding the heap mutations that can create
cycles in the first place**. Programs that fit this subset compile to
plain reference counting with no collector; programs that do not fit
opt in explicitly to ref-typed cycles by marking the relevant field as
`.cursor` (non-owning) or by using a separate cycle-collected ref kind.

This document specifies the static check.

## Field Annotations Recap

Three kinds of `ref` fields, as defined in [borrowchecking.md](borrowchecking.md):

| Annotation       | Ownership   | Cycle-safe by construction? |
|------------------|-------------|------------------------------|
| `{.unique.}`     | exclusive   | only with this check         |
| `{.cursor.}`     | non-owning  | yes (does not own)           |
| (plain `ref`)    | shared      | only with this check         |

For the cycle check, **owning fields** are all ref fields that are
*not* `.cursor`. A `.cursor` field cannot keep an object alive, so it
cannot participate in an ownership cycle and is exempt from every rule
below.

## Cycle Construction Theorem

> **A heap mutation `α.f = e` (where `f` is owning) cannot create a
> cycle if `e` is reachable from `α` in the pre-state heap via a
> non-empty path of owning field accesses.**

**Proof.** The assignment changes only `α`'s outgoing `f`-edge. Every
other node and edge is unchanged. So any cycle in the post-state that
did not exist in the pre-state must pass through `α`, which means it
must use the new edge:

    α →[new f] e → … → α

That cycle requires a back-path `e → … → α` in the post-state. The
post-state minus the new edge is a *subgraph* of the pre-state (we
lost the old `α.f` edge, gained no others), so any back-path
`e → … → α` would have already existed in the pre-state. Combined
with the hypothesis that `α → … → e` already existed, that is a
cycle in the pre-state — contradicting acyclicity by induction. ∎

The theorem holds with the side condition that `e ≠ α` (the
zero-length case `α.f = α` is a literal self-loop). The syntactic
form of the rule excludes this case by requiring at least one field
projection on the right-hand side.

## The Rule

For an assignment `α.f = e` where `f` is an owning field, **the
assignment is accepted iff at least one of the following holds**:

1. **Self-projection.** `e` is syntactically of the form
   `α.g₁.g₂.….gₙ` with `n ≥ 1` and every `gᵢ` an owning field
   access. (The receiver path on the RHS must start with the *entire*
   LHS receiver path `α`, not merely share a leftmost binding.)

2. **Fresh allocation.** `e` is a freshly allocated value — a
   `(newobj …)` or `(oconstr …)` expression — or a node already
   proven fresh by the per-binding freshness analysis (see
   *Freshness tracking* below).

3. **Nil literal.** `e` is `nil`.

4. **Receiver rooted at `result`.** The receiver `α` is rooted at the
   enclosing proc's `result` variable. The result is allocated by the
   caller and is not aliased into other reachable graphs before this
   point, so any write into it is cycle-safe (subject to the
   to-be-implemented freshness analysis tracking escapes).

5. **Disjointness obligation discharged.** The enclosing routine has
   a `requires: disjoint(α-root, e-root)` precondition that is
   satisfied at this call site (see *Disjointness propagation* below).

Otherwise the compiler emits a contract-violation diagnostic.

`.cursor` field writes are not subject to this rule. Compiler-synthesised
hooks (`=dup`, `=copy`, `=trace`, …) discharge cycle-safety via rule 4
because their writes are rooted at the hook's `result`.

## Worked Examples

### Accepted

```nim
proc construct(head: var Node) =
  head = newNode(link: head, data)        # rule 2: RHS is fresh

proc traverse(head: Node) =
  var current = head
  while current != nil:
    current = current.next                # not a heap write — read-only

proc removeNext(x: Node) =
  x.next = x.next.next                    # rule 1: RHS is x.next.next,
                                          # starts with `x`, owning fields

proc shortcutLeft(x: Node) =
  x.left = x.left.right.left              # rule 1: any depth works

proc clearTail(x: Node) =
  x.next = nil                            # rule 3: nil

# Surprising accept: both sides are projections from `head`, so
# re-pointing within head's own subgraph cannot close a cycle even
# though the field names differ. The proc name is intentionally
# misleading — the rule correctly accepts this.
proc reLink(head: Node) =
  head.last = head.first                  # rule 1: both rooted at `head`

proc combineOk(a, b: Node) {.requires: disjoint(a, b).} =
  var last = a
  while last.next != nil:
    last = last.next
  last.next = b                           # rule 5: precondition discharged
```

### Rejected

```nim
proc createsSelfLoop(x: Node) =
  x.next = x                              # not a non-empty projection;
                                          # this is the literal self-loop case

proc createsCycle(n: Node; root: Node) =
  n.parent = root                         # different roots, no `disjoint`
                                          # annotation → rejected unless
                                          # `parent` is `.cursor`

proc combine(a, b: Node) =
  var last = a
  while last.next != nil:
    last = last.next
  last.next = b                           # different roots → rejected unless
                                          # caller has discharged `disjoint(a, b)`
```

## Freshness Tracking

A binding `x` is **fresh** from the moment it is initialised by a
fresh-producing expression — `(newobj …)`, `(oconstr …)`, or
`(nil)` — and remains fresh until it escapes. Calls are not assumed
fresh: even a 0-arg call could return a global, and they are rare
enough that no special-casing is warranted. Two sources of freshness
are tracked:

- **Local vars** initialised with a fresh-producing expression.
- **The proc's `result`.** Allocated by the caller, not aliased into
  other graphs at proc entry.

**`sink` parameters are NOT fresh.** `sink ref T` only transfers one
RC count from caller to callee; the caller may still hold other
aliases that reach the same heap node, so a sink-ref parameter is
not necessarily disjoint from other parameters. Treating sink as
fresh would be unsound. The disjointness obligation for cross-root
writes from sink-ref parameters needs an explicit
`requires: disjoint(...)` precondition or a future `.unique`
parameter pragma.

A read of a fresh binding on the right-hand side is treated as
fresh-allocation for the cycle rule — the cross-root write is then
accepted because the freshly-owned tree is disjoint from the
receiver's reachable set.

The current implementation marks a local fresh on initialisation and
keeps it fresh for the rest of the proc; a follow-up will track
escapes (passing the binding to a non-`disjoint` parameter, storing
it into a non-fresh field) by invalidating the freshness flag.

Once full union-find lands, the rule extends to the case below: two
locally-built structures linked together.

```nim
var a = newNode()                # class {a}
var b = newNode()                # class {b}, disjoint from {a}
a.next = b                       # cross-root, but both fresh-disjoint
                                 # → accepted; classes merged to {a, b}
b.next = a                       # cross-root within {a, b}; no longer
                                 # disjoint, no projection form
                                 # → rejected (genuine cycle)
```

Freshness flows through ordinary control flow (`if`, loops) by taking
the union of classes at join points; freshness is lost on any escape
that is not itself `disjoint`-annotated.

## Receiver-Type Restriction

The rule applies *only* when the receiver of `α.f = e` is reached
through a ref or ptr — i.e. when the assignment writes through the
heap. Writes to a ref field of a plain value-typed receiver are not
subject to the rule, because the receiver itself is on the stack (or
embedded in another object) and therefore has no incoming ref edges
that could close a cycle.

```nim
type
  Plain = object
    other: Node          # ref field, but Plain is not a ref
  Heaped = ref object
    other: Node

var p = Plain()
p.other = makeNode()     # exempt: Plain is value-typed, no cycle possible

var h = Heaped()
h.other = makeNode()     # subject to the rule (handled by freshness on `h`)
```

## Known Loophole: Whole-Value Assignment of Aggregates Containing Refs

The rule fires on writes to ref-typed fields. It does **not** fire
on whole-value assignments of value-typed aggregates (`object`,
`tuple`, `array`, …) that happen to contain ref fields. Such
assignments can re-plumb ref edges without ever writing to a ref
field directly, and could in principle be used to construct cycles:

```nim
type
  Wrap = object        # value type containing a ref
    r: Node

proc evade(target: Node, w: Wrap) =
  target.wrap = w      # writes a Wrap, not a ref. No cycle check fires.
                       # If `w.r` reaches `target`, this closes a cycle
                       # via target → w.r → … → target.
```

We deliberately leave this unchecked. Two reasons:

1. **Practice.** Stuffing an aggregate value into a ref's field as a
   way to attach refs to a graph is exceedingly rare — nearly every
   real cycle constructor in Nim/Nimony goes through a direct
   `α.field = ref` write. The audit of `lib/pure/collections/lists.nim`
   (Nim 2 stdlib) found zero instances.
2. **False-positive cost.** Extending the rule to "any assignment of
   any value containing a ref field" would flag a lot of perfectly
   benign code — copying tuples, returning structured results,
   passing aggregates by value. The signal-to-noise ratio collapses.

The escape valve `{.cast(uncheckedCycle).}:` is also available for
the rare aggregate-write case if a future iteration tightens this.

## Disjointness Propagation

`requires: disjoint(a, b)` is a precondition that the reachable
subgraphs rooted at `a` and `b` share no nodes. At call sites the
caller must discharge it. The discharge cases, in order of preference:

1. One of the arguments is freshly allocated (freshness analysis).
2. The arguments are in different freshness classes.
3. The caller has its own `requires: disjoint(α, β)` that implies the
   callee's obligation, and the obligation propagates upward.
4. None of the above — the call is rejected.

The propagation never requires whole-program shape analysis; it is a
local check at each call site, plus a local accumulation of
preconditions on each routine.

## Override: `{.cast(uncheckedCycle).}`

Sometimes the caller knows a precondition the analysis cannot
prove — for instance, "this newly-allocated node is not already in
the tree." For those cases the static check can be locally
suppressed inside a cast block:

```nim
proc bstInsert(root: Node, n: Node) =
  {.cast(uncheckedCycle).}:
    root.next = n          # caller guarantees `n` is not in `root`
```

The block disables the cycle check for the statements inside it. As
with any `cast(...)` escape, the responsibility shifts to the
programmer; misuse can leak memory through cycles. Future work that
formalises `requires: disjoint(...)` will let most of these blocks
be replaced with a precondition that the compiler verifies at call
sites.

## Refinements

Two cases are accepted by the soundness theorem but rejected by the
purely syntactic rule:

### Aliasing-driven splice-out

```nim
prev.next = n.next       # safe iff `prev.next == n` (the loop invariant
                         # that established `prev` made this true);
                         # after rewriting to `prev.next = prev.next.next`
                         # the rule accepts trivially.
```

The compiler may either:

- accept the textual form by lifting the equality `prev.next == n`
  from local control flow (loop exit, `if` guards), substituting it,
  and re-running the projection check, or
- emit a hint suggesting the canonical rewrite.

Both options keep the rule sound; the first is friendlier, the
second is simpler.

### Doubly-linked invariants

```nim
n.prev.next = n.next     # safe in a doubly-linked list because
                         # `n.prev.next == n`; rewrite is
                         # `n.prev.next = n.prev.next.next`.
```

Same treatment as above. Until a fact is lifted into the analysis,
the user is required to either rewrite or to add a `disjoint`
precondition.

## What This Buys

- **No cycle collector for any type whose owning ref fields all pass
  the check.** Plain RC suffices.
- **Compile-time detection** of accidental cycle construction, which
  today silently leaks under non-collected RC.
- **Documented escape valves** (`.cursor` and `requires: disjoint`)
  that map onto language features that already exist.

## What This Does Not Buy

- Doubly-linked rings and graphs are not expressible under the rule
  with all-owning fields. Either:
  - mark the back-edge field `.cursor` and arrange for the container
    to keep the nodes alive some other way, or
  - keep the type as a cycle-collected `ref` — these types opt into a
    collector only for that specific subgraph.
- Fully shape-aware reasoning (separation logic). Disjointness is
  carried as annotations, not inferred globally.

## Case Study: `lib/pure/collections/lists.nim` (Nim 2 stdlib)

Applying the rule to the existing module shows what is already
compatible and what would need attention:

- **~50% of writes pass for free** because `prev` and `tail` are
  already `.cursor`. The rule never looks at them.
- **~40% are cross-root writes** of the form `L.head = n` or
  `L.tail.next = n`. They need `requires: disjoint(L, n)` on the
  node-taking overloads. End users hitting `a.add(value)` never see
  the obligation; the disjointness is discharged because
  `newSinglyLinkedNode(value)` is fresh.
- **~5% are splice-outs** in non-canonical form
  (`prev.next = n.next`). Either rewrite to `prev.next = prev.next.next`
  or rely on the equality-lifting refinement.
- **~5% are explicit cycle creators**: `n.next = n` (singleton ring),
  `L.tail.next = L.head` (re-close cycle in `remove`), and self-`addMoved`.
  These are correctly rejected — they are precisely the cases where
  the cycle collector earned its keep, and ring types must opt out
  via `.cursor` on `next` (with the container owning the chain) or
  via a cycle-collected ref kind.

The verdict is that `lists.nim` already encodes most of the
discipline: someone deliberately put `.cursor` on every back-edge.
The remaining gap is largely formalising the implicit "this node is
not already linked elsewhere" precondition into `disjoint`.

## Opt-in Abstract-Interpretation Pass

The syntactic rule above is fundamentally limited: it cannot replace
runtime cycle collection in the presence of generic containers,
aggregate value writes, `seq.add`, `arr[i] = ref`, and other indirect
edge constructions. Closing those gaps with more syntactic special
cases creates teachability problems (false positives in code that
isn't even under the user's control — generic library
instantiations) without ever fully replacing the collector.

For users who want a stricter analysis, an opt-in
abstract-interpretation pass runs **as the last step of
`contracts_njvl.nim`** when the module enables it:

```nim
{.feature: "checkcycles".}
```

The pass uses **allocation-site abstraction**: each `(newobj …)` /
`(oconstr …)` site, and each ref/ptr parameter, gets a unique
abstract identity. Variables map to *sets* of abstract identities.
Each ref field of each abstract object holds a set of identities it
might point to. Statement-by-statement, the pass updates this
abstract heap. After every owning-ref-field write, it scans the
field-edge graph for cycles reachable from the modified receiver and
reports each new cycle once.

Properties of the AI pass:

- **No annotations required on generic library code.** The seq's
  `add` is analyzed in the calling context; if the appended argument
  is a fresh abstract identity, no cycle is reported. The seq source
  itself stays annotation-free.
- **Catches what the syntactic rule misses.** A self-loop like
  `result.next = result` is accepted by the syntactic rule (rule 4,
  result-rooted) but rejected by AI because the abstract heap shows
  `result` reachable from itself.
- **Honest framing.** Reports are "potential ownership cycle through
  …", not hard errors. The pass is an over-approximation in the
  sound direction (no missed cycles when the analyzer is precise
  enough) and an under-approximation when call summaries are
  imprecise.
- **`{.cast(uncheckedCycle).}` suppresses both** the syntactic rule
  and the AI pass — useful when you genuinely intend a temporary
  cycle that you break before destruction.

Limitations of the current implementation (deliberate MVP scope):

- **No inter-procedural summaries.** A call's result is treated as a
  fresh isolated abstract identity. Cycles created by procedures
  that return aliases of their arguments are not caught.
- **Loop bodies are visited once**, not iterated to fixpoint. Cycles
  that only manifest after several loop iterations may be missed.
- **Aggregate value reads** (tuple/array/object literals) collapse
  to an external "unknown" identity. The `seq.add` / `arr[i] = ref`
  cases need element-write modelling that's not in the MVP.

These are the natural follow-up extensions. The pass is structured
so each can be added without rewriting the core.

## Implementation

Implemented in [src/nimony/contracts_njvl.nim](../src/nimony/contracts_njvl.nim)
in the `traverseStore` path. The check uses the existing `extractPath`
to obtain LHS and RHS as `seq[SymId]` paths, splits the LHS into
receiver path + final field, walks the receiver and projection types
to look up `.cursor` annotations on each field, and applies the rule
above. The receiver-type restriction is checked via `getType` on the
inner expression of the dot.

The freshness analysis tracks per-local "init was fresh" via a
`freshLocals` set populated in `traverseLocal`. The
`disjoint`-propagation is scaffolded: rule (5) is discharged only if
the caller has a literal `requires: disjoint(...)` naming the same
roots. Tracking escapes from the freshness set, inferring freshness
across calls, and union-finding bindings are left for a follow-up
that lives in the same file.

Tests live in [tests/nimony/nocycles/](../tests/nimony/nocycles/).
