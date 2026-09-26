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
| `alloc` out of memory | `nil: pointer` | recoverable | **[impl]** `missingBytes` |
| `new` out of memory | none: `ref T` is not-nil | **fatal** | **[impl]** raises `OutOfMemError` inside a `.raises` routine, panics elsewhere |
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

The rule is therefore: **raise `OutOfMemError` where a `.raises` signature can
carry it, panic otherwise.** `duplifier.trNewobj` emits one of the two right
after the `allocFixed` call, before the `rc` store that would otherwise go
through the `nil`:

```nim
# A not-nil `ref` has no value to stand in for a failed allocation: raise where
# a `.raises` signature can carry the error, terminate where it cannot.
if CanRaise in c.flags:
  genOutOfMemCheck(c, ow, info)
else:
  genOutOfMemPanic(c, ow, info)
```

`CanRaise` is set iff the *enclosing routine* carries the `raises` pragma, so the
choice is made per allocation site, not per call chain. `genOutOfMemPanic` calls
`system/panics.panicOutOfMem`, which is `noreturn` and allocation-free (a string
literal is a static const, not an allocation).

Before that `else` existed the pointer went untested in a non-raising routine and
the `rc` store went through the `nil`: the process died by SIGSEGV at a faulting
address, with no message and no `die 1` — accidentally fatal rather than
deliberately fatal, and on a target without memory protection a wild store to a
low address instead.

Two loose ends remain:

- `genOutOfMemCheck` raises without clearing `missingBytes`, so a caught
  `OutOfMemError` leaves `threadOutOfMem()` true for the rest of the thread's
  life. The sticky slot needs a take-and-clear accessor for the same reason.
- `setOomHandler` governs `alloc`, not `new`: a handler that calls `quit` makes a
  failed `new` abort too, but only as a side effect of the allocator never
  returning. `doc/stdlib.md` now says which of the two allocators the policy
  covers.

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
