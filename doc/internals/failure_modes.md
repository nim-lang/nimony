# Failure modes

Which run-time failures Nimony recovers from in place, which ones kill the
process, and why the line falls where it does.

The motivating requirement is a server that stays up: a single malicious request
must not cause a teardown and a restart. That rules out fail-fast as the
universal answer. It does not rule out fail-fast as the answer for the failures
that have no other one, and this document is about telling the two apart.

None of the backends has table-based unwinding, so the recoverable half is
deliberately a poor man's exception mechanism: a sticky error slot, in the shape
the IEEE-754 flags have for floats and that `system/memory.nim` already has for
out-of-memory.

Implementation points:

- `lib/std/system/panics.nim` — `panic`, `raiseIndexError3`, `nimIcheckAB`,
  `nimInvalidObjConv`: the fatal class.
- `lib/std/system/memory.nim` — `missingBytes`, `continueAfterOutOfMem`,
  `threadOutOfMem`: the recoverable class as it exists today.
- `lib/std/system.nim` — `localErr` (the *in-flight* error of the `.raises`
  mechanism, not a sticky slot), `overflowFlag`.
- `src/lengc/cprelude.nim` — `_Qlengc_div_*_overflow`: the division
  continuations, already written.
- `src/nimony/langmodes.nim` — `CheckMode`, the per-module check flags.
- `src/nimony/contracts_fir.nim` — the prover; the only mitigation the fatal
  class has.
- `doc/internals/leng-spec.md` — `(keepovf …)` / `(ovf)`, the backend's checked
  arithmetic.

Status markers below: **[impl]** in the tree today, **[part]** partly there,
**[dsgn]** designed here and not yet built.

---

## The criterion

> A failure is **recoverable in place** iff the operation has a continuation
> value that is
>
> 1. **total** — defined for every input, including the failing one;
> 2. **invariant-preserving** — inside the range its own static type promises;
> 3. **non-misleading** — no downstream analysis can derive a false fact from
>    it.
>
> Fail any of the three and there is no value to hand back. The space dimension
> then becomes the time dimension: the program dies.

There is no location for `a[0]` when `a.len == 0`. Every candidate answer is
either a dangling reference or a lie about which element was addressed, so the
only honest report is termination.

Integer overflow is the opposite case. `a + b` wrapping is total, an `int` holds
the wrapped value so no type invariant breaks, and — given the obligation in
[Wrapping and the prover](#wrapping-and-the-prover) — nothing downstream is
misled. The answer is wrong, but it is a *wrong number*, not a wrong pointer.
Memory safety never depended on the arithmetic check; it depends on the bound
checks and `.requires` guards, which stay.

Conditions 2 and 3 carry more weight than they look like they do. Plenty of
failures have a total continuation that the compiler will nonetheless believe a
lie about — see the range-check case below.

## Taxonomy

| failure | continuation | class | status |
|---|---|---|---|
| `+` `-` `*` on signed ints | wrap | recoverable | **[dsgn]** unchecked today: `AddI` → `(add …)` → C `+` |
| `div`/`mod` by zero | `0` | recoverable | **[impl]** `cprelude.nim`, flag already returned |
| `low(int) div -1` | `low(int)` | recoverable | **[impl]** same helpers |
| `succ`/`pred`/`inc`/`dec` | wrap | recoverable | **[dsgn]** |
| narrowing conversion to a machine-range type | truncate | recoverable | **[dsgn]** `RangeCheck` is in `CheckMode` and in `DefaultSettings`, but nothing reads it |
| assignment into `range[a..b]` | **clamp**, not truncate | recoverable | **[dsgn]** see below |
| `alloc` out of memory | `nil: pointer` | recoverable | **[impl]** `missingBytes`; `-d:nimMaxHeap=N` injects it |
| `new` out of memory | none: `ref T` is not-nil | **fatal**, or reported | **[impl]** tiers 1-2; tier 3 behind `{.feature: "strictnew".}`. See below |
| `a[i]` on array, `s[i]` on seq/string | none | fatal | **[impl]** `die 1` |
| `.requires` violation | none: the body assumes it | fatal | **[impl]** `die 1` |
| invalid object conversion | none | fatal | **[impl]** `die 1` |
| stack exhaustion | none | fatal | **[impl]** |
| `Table.[]`, key absent | none | fatal *or* total accessor | **[part]** raises `KeyError` today; see below |
| constant-folded overflow | none needed | compile-time error | **[impl]** `expreval.nim` |

Two entries in that table are the criterion disagreeing with the code as
written, and both are discussed below: `new` under OOM, and `Table.[]`.

## The recoverable class: one sticky error slot

### The slot

One `ErrorCode`, **first-error-wins**, plus a site for diagnostics:

```nim
var pendingErr: ErrorCode              # Success when clean
var pendingSite: (cstring, int32)      # file/line of the first error; debug builds
```

Names are provisional. `ErrorCode` rather than a bool because it then subsumes
the whole recoverable class as one mechanism — `OverflowError`,
`OutOfMemError`, `RangeError` — and `missingBytes` / `threadOutOfMem()` collapse
into it. First-wins keeps the root cause instead of the last symptom, and it
makes the update a well-predicted `if pendingErr == Success` rather than an
unconditional store.

`pendingSite` earns its keep. A wrapped value surfacing at a request boundary
with no location is the main practical objection to deferred detection, and two
stores on an untaken path buy the location back.

**This is not `localErr`.** `localErr` is the in-flight error of the `.raises`
mechanism — `raise x` lowers to `localErr = x; return` (`controlflow.nim`) — set
and immediately consumed. An arithmetic op writing it would clobber a live error
mid-propagation.

### Scope: the continuation, not the thread

`missingBytes` and `localErr` are `{.threadvar.}`. For a sticky slot whose
lifetime is *a request*, thread-local is wrong: per `doc/passive_procs.md`, the
continuation `delay(call)` produces may be scheduled on another thread. So

- a request overflows, suspends, resumes on another thread: the flag is stranded
  and the request's own boundary check sees `Success`;
- the original thread then serves another request, which observes a foreign
  `OverflowError` and rejects perfectly good work.

Both are silent and load-dependent — the worst failure shape for the workload
this design exists to serve. The slot belongs in the continuation frame
(`CoroutineBase`), with the threadvar as the fallback for code not running under
a scheduler. The accessor hides which one answered.

A consequence for naming: `threadOutOfMem()` describes the wrong scope once this
lands.

### Three lowerings, one IR node

The operation is one node in the Nimony IR and the Final IR; only the backend
lowering differs, selected per module by `CheckMode`:

| mode | lowering |
|---|---|
| `flag` (default, and `release`) | `__builtin_add_overflow` → wrapped result, `if (ovf) setPendingErr(OverflowError)` |
| `trap` (debug, CI) | same intrinsic → `if (ovf) panicOverflow()` with line info |
| `off` (`-d:danger`) | wrapping add, no bit |

`trap` is not optional. Production wants on-error-continue; a test suite wants
the failure at the point of overflow. Because the three share a node, `trap`
costs almost nothing to implement, and CI should run in it.

**Wrapping has to be *defined*, so plain C `+` is out** at every optimization
level: signed overflow is UB in C, and a compiler entitled to assume it cannot
happen is entitled to delete the check that observes it. Use
`__builtin_add_overflow`, whose wrapped result is defined and which the backend
already emits (`lengc/genstmts.nim`); on LLVM, `add` without `nsw` or
`llvm.sadd.with.overflow`. `-d:danger` must still avoid bare signed `+`.

### Do not route default arithmetic through `keepovf`

`(keepovf …)` is a *statement* with a sticky flag that must be reset
(`leng-spec.md`). Making it the lowering for all arithmetic would flatten every
arithmetic expression into statements with temporaries through `xelim` and cse,
and it already defeats the inliner (`tests/nimony/opt/topt_keepoverflow_inlined.nim`).
The trapping/flag-setting form stays an *expression* in the compiler's IR;
temporaries appear only in the C backend, where they appear anyway.

### The optimizer rule

`lengc/shoggoth/cse.nim` tracks `writesGlobal`. If a flag update counts as one,
every arithmetic expression becomes an optimization barrier and integer code
loses CSE, LICM and folding — a far larger cost than the branch. The sticky,
first-wins, order-insensitive design is what buys the way out, and the rule is
the one IEEE `fenv` uses:

> **A flag write may be freely reordered, duplicated and eliminated. Only a flag
> *read* is a barrier.**

So a loop of adds may accumulate overflow bits in a register and store once
after the loop. That is legal precisely because the flag is sticky and nobody
reads it in between, and it is an advantage the trap model does not have: a trap
pins the failure point and cannot be hoisted.

### `{.keepOverflowFlag.}` blocks are exempt

`seqimpl.nim` computes capacities inside `{.keepOverflowFlag.}` blocks and
*expects* the overflow. If those ops also set the sticky slot, every large-but-
legal `setLen` poisons the request's error state. Inside such a block the sticky
slot is not touched: the programmer took responsibility by reading `(ovf)`.
Prover-discharged operations set nothing either, which is consistent.

### Clamp, don't truncate, where a type says so

Condition 2 of the criterion decides this. For `int32(x)`, truncation is fine —
`int32` holds the truncated value and promises nothing narrower. For
`range[0..10]`, a truncated value sits *outside its own type*, `inferle` will
use `x <= 10` as a fact about it, and `applyVerdicts` will then delete a bounds
check on the strength of that fact. Total, but a lie, so condition 3 fails too.

Clamping is total *and* invariant-preserving, so the facts stay true and the
failure stays recordable. Rule: truncate where the type's range is the machine
range, clamp where the type carries a narrower invariant. Both are recoverable.

### Who reads the slot

A sticky flag nobody reads is a no-op, so the read points are part of the
design:

1. **Explicit, at the request boundary** — `if pendingError() != Success: …`,
   mirroring `threadOutOfMem()`. No magic, and the server author is the one who
   knows where a boundary is. This is the primary mechanism.
2. **Opt-in auto-merge at `.raises` returns** — a module feature under which a
   `.raises` routine's epilogue folds the sticky slot into the `ErrorCode` it
   already returns. Non-raising code pays nothing, no ABI changes, no virality;
   overflow deep inside plain `func`s surfaces at the nearest *existing* error
   boundary and is caught by an ordinary `try`/`except ErrorCode`. The routine's
   `result` is discarded in favour of the error, which is on-error-continue by
   construction.

What this model costs, stated plainly: between the overflow and the read, a
wrong value flows. It can be stored in a field, written to a socket, or used as
a loop bound. Wrapping is *safe* but not *correct*, and the window is as wide as
the distance to the nearest boundary check. For a server that is the right
trade; it is not Nim 2's.

## Strings and seqs: the recoverable class in practice

Both are values with a representation for "this did not work", which is what
condition 1 asks for, and both are hardened so that a failed allocation leaves a
usable object behind.

**A string becomes the OOM cookie.** `strOom` installs `"\nD^OOM\0"` packed
inline -- an ordinary 7-byte short string -- and `isOom` detects it. Every
allocation site in `stringimpl.nim` is guarded, and the site that grows an
existing heap string releases the old block through `arcDec` before installing
the cookie: dropping the pointer would leak a block at the one moment memory is
known to be scarce. The cookie is a normal value, so appending to it simply
grows a new string out of it; code that wants to *notice* the failure has to
ask `isOom` (or `threadOutOfMem()`) at the point of the operation.

**A seq keeps what it had.** A failed `realloc` leaves the original block
allocated and untouched, so `resize` and `=copy` keep it: the growth did not
happen and the seq is still exactly what it was. They previously overwrote
`data` with `nil` and set `len = 0`, which leaked the block *and* silently
truncated the caller's elements -- an out-of-memory during `s.add` destroyed the
whole sequence. Because failure is now invisible in `data`, callers read the
refusal off the **length**: `grow` and `setLen` test `s.len < newLen`, not
`s.data == nil`, or they would fill `newLen` slots into storage that never grew.

The asymmetry with `new` is condition 1 again: `seq` and `string` have a valid
empty-ish representation, a not-nil `ref` does not.

**A Table refuses the insert.** A `Table` grows two seqs -- `data` for the pairs
and `hashes` for the open-addressing index -- and the probe loops in
`tables.nim` (`rawGet`, `emptySlot`) walk until they find a free slot, which is
sound only while the index has one. `mustRehash` guarantees that in normal
operation; an index that cannot be *resized* breaks it, and `data` growing on
past it used to fill the index completely, at which point a lookup for an absent
key never terminated. So `rawPut` refuses the insert when the index cannot
absorb it: an allocation failure like any other, leaving a table that is short of
what was asked for but wholly consistent. Two related rules fall out:

- `fillHashPart` is the only thing that builds an index from `data`, and it
  sizes for what `data` already holds. `resize` only grows an existing index, so
  using it to create one would leave an index that is non-empty and yet missing
  every key already stored -- which `rawGet` would trust.
- `rawGet` falls back to a linear scan when there is no index at all. Slow, but
  answering "absent" for a key that is present would make `[]=` store a second
  copy of it.

`mgetOrPut` has to notice the refusal too: it takes `data.len-1` as the index of
what it just inserted, which on a refusal names the *previous* pair and would
hand back a `var V` into someone else's value.

## The fatal class

No continuation exists, so the operation cannot return and the process exits via
`panic` → `die 1`. No unwinding, no destructors, no `finally`.

The consequence for a server is not that these failures are handled — they
cannot be — but that they must be made **impossible on the request path**. The
mitigation is static, not dynamic:

- `contracts_fir` discharges `.requires` and index obligations, and
  `applyVerdicts` removes what it proved.
- `{.feature: "staticContracts".}` turns an undischarged obligation into a
  compile-time error, module by module.

So "this server does not restart" is a compilable property: *the request path
compiles under `staticContracts`*. That makes the feature load-bearing
production infrastructure rather than an aspiration, and it is the reason the
sticky slot and the prover are not competing designs — they are the two halves
of the taxonomy, split along the criterion.

### The residue

Not every fatal failure is provable. Object conversions and `new`-under-OOM are
in principle reachable by the prover; stack exhaustion is not, and neither is
any failure in code the prover skips (generic and template bodies are re-sem'd
elsewhere and analysed at their use sites, but foreign code, `cast` and `addr`
escape hatches are not analysed at all). A deployment therefore still needs a
supervisor for the residue. Better to say so here than to discover it in
production.

## Wrapping and the prover

Specifying wrapping creates a soundness obligation on `contracts_fir`.
`inferle`'s facts (`LeXplusC`: `a <= b + c`) are statements about mathematical
integers. `checkRequires` discharges `seq`/`string` index guards from such facts
and `applyVerdicts` then **deletes the bounds check**. If `+` wraps, a fact
derived from an add holds only when that add did not wrap.

> The prover may use a fact derived from an arithmetic operation only if it has
> also proven that operation cannot wrap. Otherwise the result is `ivUnknown`.

The interval it needs for that proof is usually the one it already computed, so
the cost is small. Today, with arithmetic unchecked and effectively UB, the hole
is equally present; the point is that "wrapping is defined behaviour" is exactly
the moment it becomes reachable by ordinary reasoning rather than by accident.
It belongs to this work, not to a follow-up.

## Escaping the fatal class: change the type

"The space dimension becomes the time dimension" also names the way out. A
partial operation becomes recoverable when the failure is moved out of the space
dimension and into the value domain — that is, by changing the operation's
*type*:

| partial | total sibling |
|---|---|
| `s[i]` | a checked accessor returning `(ErrorCode, T)`, or an iteration form the prover understands |
| `Table.[]` | `getOrDefault`, or a `(bool, V)` / `nil V` accessor |
| `new T` | an allocation returning `nil T`, narrowed by the caller |

So the stdlib owes a total sibling for every partial operation that might appear
on a request path, and the request path owes it to itself to use them.
`getOrDefault` over `[]` is already this pattern (`lengc/llvmdebug.nim` uses it
for exactly this reason); it is just not systematic yet.

### `Table.[]`

Missing key has no continuation, yet `tables.nim` raises `KeyError`, which makes
every lookup `.raises` and drags the returned-tuple ABI through every caller.
Two places in the tree document working around precisely that
(`lengc/shoggoth/vmrewriter.nim`, `lengc/llvmdebug.nim`).

By the criterion the raise is the wrong tool — but so is a panic, for a
different reason: for a `Table`, absence is usually an *expected condition*
rather than a violated invariant. The cure is therefore the total accessor, with
`[]` demoted to the "I assert presence" spelling that panics like `s[i]`. Same
failure class as seq indexing, same answer, and one documented `.raises`
infection vector leaves the stdlib.

### `new` under OOM

`doc/lenientnils.md` makes `ref T` not-nil by default, so **`nil` is not a valid
continuation for `new`** — conditions 1 and 2 both fail, and the continuation
would be a dangling reference rather than a wrong number. `alloc` is unaffected:
it returns a nilable `pointer`, so it stays in the recoverable class where
`missingBytes` already puts it. The split runs between the two, not between
allocation sizes.

Both `new(T)` and `T(...)` therefore behave in one of three ways, in this order
of priority:

| | when | what happens |
|---|---|---|
| 1 | the enclosing routine is `.raises` | the `nil` is mapped to `ErrorCode.OutOfMemError` and raised |
| 2 | otherwise, in a `lenientnils` module | the compiler guards the field stores it generates; the rest is the programmer's, who may check for `nil` or live with a segfault |
| 3 | otherwise | the same guard, and the contract pass makes the caller narrow the result before using it -- behind `{.feature: "strictnew".}` for now |

**Tier 1** is what makes allocation-heavy code inside an error-reporting routine
readable: the routine already has a channel, so the failure travels on it and the
body may treat the result as not-nil. `duplifier.trNewobj` emits the raise, and
`contracts_fir` *assumes* not-nil for a `newobj` in such a routine
(`procCanRaise`). That assumption is about a pass that has not run yet —
`genOutOfMemCheck` is hexer, the prover is nimsem — and it is sound because both
read the same `RaisesP` pragma to decide.

**Tier 2** is what `lenientnils` means for allocation, and it is the Nim 2
behaviour: pointers are `unchecked`, the prover demands nothing, and a `nil` that
reaches a dereference faults. The compiler still guards its *own* stores — the
`rc` and payload writes `trNewobj` emits for the construction — because a
construction that faults before returning gives the programmer nothing to check.
Past that point the consequences are theirs.

**Tier 3** is behind `{.feature: "strictnew".}` and off by default. Every
diagnostic it produces is correct; what stands in the way of making it the
default is that as the default it would stop `T(...)` from being usable as an
*expression*. Measured with the switch forced on, 141 sites across 50 files and
16 test directories fail, and they split in two:

| | count | shape | what the port costs |
|---|---|---|---|
| *cannot prove* | 78 | a local holding an allocation, then dereferenced | one `if p == nil` per local |
| *cannot analyze* | 63 | a construction nested inside another expression: `f(T(...))`, `Pair(a: Leaf(...), b: Leaf(...))`, any tree literal | there is no syntax that narrows a subexpression, so each one must be hoisted into its own `let` first |

The second class is the real obstacle, and it is not a porting cost — it is a
language change. Nor is it a library problem: `lib/std` contributes exactly one
site (`newStringTable`). The 141 are overwhelmingly the closure, method, arc and
borrow tests, i.e. code with nothing to say about allocation failure that pays
the price for constructing a `ref` at all.

For a *constructor* the answer is to DECLARE a `nil T` result, so each
construction site narrows once with no error channel, no ABI change and no
`try`. `system.new` needs that regardless: being generic, its `out T` takes its
nilability from the instantiation site, so no module-level opt-out can reach it.
`.raises` is not that answer -- it puts a `try` on every call site, and
`newHttpTags` has module-level `let` callers where one does not fit.

For the nested-expression class the answer has to come from the compiler.
`wantNotNil` in `contracts_fir.nim` already lets a bare `NewobjX` through when
`procCanRaise`, on the grounds that the compiler maps the `nil` itself. Making
the *non*-raising case emit a runtime `nil` check there rather than a diagnostic
would retire the switch with no source churn at all, and it is strictly better
than either of today's two states: someone who narrows still pays nothing, and
someone who does not gets a panic where they currently get a segfault. That is
the same trade `runtimeContracts` already makes for `.requires`.

It needs no new checking machinery, because the nilability markers and their
enforcement already exist. There are three, not two:

| marker | `wantNotNilDeref` | meaning |
|---|---|---|
| `notnil` | trusted | dereferences are free |
| `nil` | fires | the prover demands a proof, which `if p != nil` supplies |
| `unchecked` | does not fire | no proving; this is what `lenientnils` stamps |

So tier 3 is a single change of *what `new` claims*: sem reports the result as
`nil ref T` instead of `notnil ref T`, and `wantNotNilDeref`, `checkNilMatch` and
the existing flow narrowing do the rest. A type that is already `unchecked` is
left alone, which is how tier 2 falls out of the same code — the marker is a
property of the type as declared, so a `lenientnils` module's `ref` arrives
already unchecked and nothing at the allocation site needs to know about the
feature.

`trNewobj`'s split is therefore on `.raises` alone: raise-and-return, or guard the
store. There is no `lenientnils` arm, because tiers 2 and 3 emit identical code
and differ only in what the prover then demands.

One place does need to know: a sum type's branch constructor. `Leaf(n: 4)` has no
expected type of its own, so `semSumTypeObjConstr` supplies the sum type -- which
is not-nil -- and `commonType` then converts the allocation's `nil` result
straight back to it. Without relaxing that expectation too, a sum type has no
spelling under tier 3 that narrows at all, not even `let x = Leaf(n: 4)`.
`tests/nimony/notnil/tstrictnew.nim` is where both halves of the feature are
exercised, `tstrictnew_errors.nim` where the four rejected shapes are.

All three tiers are exercised by `tests/nimony/oom`, which compiles under
`-d:nimMaxHeap=1 -d:nimHardenOutOfMem`. `nimMaxHeap` is Nim's heap cap, with
Nim's unit and Nim's consequence — `raiseOutOfMem` aborts — and
`nimHardenOutOfMem` turns it into a refusal, so allocation answers `nil` and
these paths run instead of the process dying. Both live in `system/alloc.nim`,
checked against the occupancy the allocator already tracks: no second layer of
accounting, and the `rawAlloc` level is where the check sits because `nil` is
already what its callers expect (`allocPages`' callers dereference immediately).

`toom_raises.nim` catches the `OutOfMemError` (tier 1). `toom_lenient.nim` takes
the `nil` and checks it (tier 2), which also asserts what the compiler still owes
there — that the construction RETURNED rather than faulting partway through its
own stores. `toom_string.nim` and `toom_table.nim` cover the recoverable
neighbours described above. The segfault a lenient module gets by declining the
check is deliberately not a test: a signal's exit status asserts far less than
those do. Neither is tier 3, which no test can assert while it is behind a
feature that nothing in the tree turns on.

The cap is on memory **held**, not on bytes ever handed out. That matters for
more than fidelity to Nim: a cumulative counter makes recovery unobservable,
since once tripped nothing can allocate again — not even the `echo` that would
report what happened. A live cap lets a string release its buffer, become the
cookie, and leave room for the program to carry on, which is the behaviour under
test.

Making allocation able to fail at all turned up three places in the ported
allocator that assumed it could not: `rawAlloc0` and `alloc0` zeroed a `nil`
result, and `realloc` copied into one — and worse, freed the original block on
the way, so `seqimpl.resize` would have lost the elements it is careful to keep.

Note also that `genOutOfMemCheck` raises without clearing `missingBytes`, so a
caught `OutOfMemError` leaves `threadOutOfMem()` true for the rest of the
thread's life. The sticky slot needs a take-and-clear accessor for the same
reason.

`setOomHandler` governs `alloc`, not `new`: a handler that calls `quit` makes a
failed `new` abort too, but only as a side effect of the allocator never
returning. `doc/stdlib.md` says which of the two allocators the policy covers.

## Divergence from Nim 2

Nim 2 raises `OverflowDefect` at the point of overflow. Because `Defect` is
excluded from `.raises` tracking, Nim 2 does *not* make `+` a raising operation
either — `func f(a, b: int): int {.raises: [].} = a + b` compiles there — so the
annotation burden is not what differs. What differs is the timing and the
recoverability:

- Nim 2 (`--panics:off`, the default) fails at the operation and lets
  `try`/`except OverflowDefect` catch it, running `finally` and destructors.
  Nimony records and continues; the wrong value flows to the next boundary
  check.
- Nim 2 (`--panics:on`) traps fatally at the operation. Nimony's `trap` mode
  matches this, and is the recommended CI setting.
- Nim 2 code that catches `OverflowDefect` around a specific expression must be
  rewritten to the `{.keepOverflowFlag.}` + `overflowFlag()` form, which is the
  explicit, local, recoverable spelling.
- Compile-time folding agrees: both reject a constant expression that overflows.

Unsigned arithmetic wraps in both, with no error (`doc/language.md`). The
signed-wrapping rule and the sticky slot need a paragraph in
`doc/differences.md`, which today says nothing about arithmetic.

## Open questions

1. Where exactly the slot lives for a thread-affine continuation versus one
   produced by `delay` — one field in `CoroutineBase`, or a save/restore at each
   suspension point.
2. Whether the `.raises` auto-merge is a module feature, a routine pragma, or
   neither.
3. Whether `RangeCheck` (already in `CheckMode`, read by nothing) is implemented
   as clamp-and-record from the start, or first as a panic and relaxed later.
4. Whether `Table.[]` is demoted in this work or on its own schedule — it is a
   breaking stdlib change and an independently worthwhile one.
5. Whether the fatal residue gets a documented supervisor story in the stdlib
   (a restartable worker abstraction) or stays the deployment's problem.
