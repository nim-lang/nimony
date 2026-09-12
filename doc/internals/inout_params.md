# `var T` parameters as in/out values

Status: design, not implemented. Measured on 2026-08-31 with a hand-rewritten
nifasm of `bench/nifbench.nim` (numbers in the appendix).

## Summary

A `var T` parameter of a proc under a Nim-controlled calling convention
(`nimcall`, `closure`, `inline`) is lowered to a value passed *in* and a value
passed *out*, not to a pointer:

```nim
proc parse(s: string; i: var int): bool
# lowers to
proc parse(s: string; i: int): (bool, int)
```

and a call `ok = parse(s, i)` to `(ok, i) = parse(s, i)`. For an object
parameter only the fields marked in the type declaration travel this way; the
rest stay behind a pointer. The lowering is done in hexer, so both backends
receive ordinary Leng: value parameters and a tuple result. Arkham needs no
change, and gcc gets a transformation it cannot do itself (IPA-SRA only lifts
parameters that are *read*, and only under LTO).

Procs under a foreign convention (`cdecl`, `stdcall`, `importc`, `exportc`,
anything a C compiler may see) keep `var T` = `ptr T`; too much code depends
on that mapping.

## Motivation

The compiler's hot loops all have the shape `proc skip(n: var Cursor)`: a
small object whose two fields `p` and `rem` are read and written on every
path, reached through a pointer. Arkham reloads `n.p` from memory on every
token access and stores `p`/`rem` back at the end of every `skip`, so the loop
is a chain of store-to-load-forwarding round trips through the same 16 bytes,
once per call. The instruction count is fine; the latency is not.

Rewriting `skip`/`enterScope`/`leaveScope`/`walk`/`walkOne` by hand so that
`p` and `rem` travel in registers, in and out, gave:

| bench | arkham today | in/out registers | gcc -O2 |
|-------|--------------|------------------|---------|
| walk  | 2.68 ms      | 2.35 ms (−12.5 %) | 3.05 ms |
| json  | 0.62 ms      | 0.59 ms (−5 %)    | 0.45 ms |

Instruction count was unchanged (540 M → 537 M for `--only:walk`); what
changed is memory operands per proc: `skip` 34 → 8, `enterScope` 28 → 15.

`parse`-shaped procs (`i: var int` advanced by a scanner) are the same problem
with a scalar, and they are everywhere in the stdlib.

## The rule is a language rule

`i = parse(s, i)` is equivalent to the pointer version only if nothing can
observe `i` while `parse` runs. So this is a semantic commitment, not an
optimisation:

> A `var` parameter of a proc under a Nim-controlled calling convention has
> no alias for the duration of the call. Reads of the location through any
> other path during the call, and writes to it, are undefined.

That is the strict-`var` semantics `lent`/view types already assume. What the
compiler can diagnose locally it should reject: the same location passed to two
`var` parameters (`f(x, x)`), and a global passed as `var` to a proc whose
summary reads that global. Aliasing through pointers taken earlier is UB by
declaration, like the existing view rules.

Observable consequences worth spelling out:

- A callee never sees another `var` param's writes through its own — `swap(x, x)`
  still ends with `x` unchanged, but `f(a, b: var int)` called as `f(x, x)` ends
  with whichever writeback is last.
- A callee raising after modifying the param still publishes the modification
  (see "Exceptions" — this is a requirement on the lowering, not a change).

## The signature follows from the declaration alone

Callers in other modules never see the body, so whether a `var` param is lifted
must be decidable from the proc header: Nim-controlled convention plus a
liftable `T` ⇒ lifted, always. A body that needs the parameter's *address* —
`addr n`, passing `n` on to a proc that takes it as a pointer (a `cdecl`
callee, a proc taking `ptr T`), capturing it in a closure — brackets locally:

```
var shadow = n          # scalars → memory
use(addr shadow)
n = shadow              # memory → scalars, only if the callee may write
```

This is what a C compiler does with a register variable, and it keeps the
summaries and the interface format untouched. Passing `n` on to another
lifted `var` param needs no bracket: it becomes `n = callee(n)`, registers all
the way — which is how `walk → walkOne → skip` chains.

## Liftable `T` and the budget

Every lifted `var` competes with the declared result for the register-return
capacity: 16 bytes on x86-64 SysV and on AArch64. Beyond that the tuple goes
through a hidden result pointer and the win evaporates. Hence:

- The result type and the lifted parameters are packed left to right while the
  tuple stays within the budget; the remaining `var` params stay pointers.
- Liftable `T` in the first phase: scalars, pointers, enums, and objects that
  are trivially copyable and at most two words.
- Larger objects lift only their **marked fields** (next section). An unmarked
  large object is never lifted.
- `out T` is the same lowering with no input half. `sink`, `lent`, `var
  openArray` (a slice is already a pair; it is not reassigned as a whole) and
  types with destructors or non-trivial `=copy` are not lifted in phase one.
  (A `var seq`/`var string` would be a move in and a move out, with no hook
  calls, and is a candidate for a later phase.)

Windows: the C ABI returns anything larger than 8 bytes through a hidden
pointer. lengc must respect that, so the budget for the C backend on Windows
is 8 bytes. Arkham is not bound by the platform ABI for `nimcall` procs and can
keep `rax:rdx`; whether it should is an open question below.

## Field marks for partial lifting

A field annotation in the type declaration names the fields that are the
object's in/out state:

```nim
type
  Cursor* = object
    owner: CursorOwner
    p {.inout.}: ptr NifToken
    rem {.inout.}: int
```

For `n: var Cursor` under a Nim-controlled convention the marked fields are
lifted (`p`, `rem` in, `(p, rem)` out) and the object pointer is passed
alongside for the unmarked ones; if every field is marked the pointer is
dropped. The compiler checks that the marked set fits the budget and that
no access to a marked field goes through the pointer.

Why a field mark and not a proc-level one: which fields are hot is a property
of the type's use, not of any one proc — `skip` never reads `owner`, `symId`
does, `beginRead` writes all three — and a mark on the type gives every proc
the same lowered shape, so the chain composes. Why not "all fields": `Cursor`
is 24 bytes and `owner` is first; any declaration-order rule picks wrong.

The mark does not affect the ABI of `cdecl` procs and is not visible to C.
(The name is open; `.register` was the first candidate. It should not suggest
a storage class, since it is a lowering rule for parameter passing.)

## The by-value half

`proc symId(c: Cursor)` — a small object taken by value — is passed today as a
hidden const pointer (`constparams`), so a caller holding `p`/`rem` in registers
has to store them back before every such call: nine of them per `walkOne`
iteration in the benchmark. The same field marks apply: marked fields of a
by-value object param travel as scalars, nothing is returned. With that, the
memory `Cursor` is not touched in the hot loop at all. This is a separate,
smaller step and can land after the `var` half.

## Lowering in hexer

A dedicated pass, tentatively `inoutparams`, in `pipeline.nim`:

- **Signature.** `(param :n (var T))` → `(param :n T)` (or `(param :n (ptr T))`
  plus `(param :n.f F)` per marked field). The result type becomes
  `(tuple R F1 F2 …)` in budget order.
- **Body.** `n` (or `n.f`) reads and writes become the scalar. Every `ret` and
  the implicit fall-off return build the tuple. `result` assignments are
  unaffected: the declared result is element 0.
- **Call sites.** `f(a, x)` → `(r, x) = f(a, x)` when `x` is a location; when
  `x` is itself a lifted param or scalar, no memory is involved. The
  destructuring assignment is emitted as statements, so the pass must run
  before `xelim`'s normal form is relied upon or emit that form directly.
- **Brackets.** As above, for `addr n`, pointer-taking callees, closure
  captures. Whether the reload after a pointer-taking call is needed comes
  from the callee's `funcsummary` (`writes` on that param); without a summary
  it is emitted.
- **Convention gate.** The callconv is visible at this point; after the pass it
  can be dropped as it is today (no `(nimcall)`/`(cdecl)` reaches the `.c.nif`
  arkham reads, and that is fine — the lowered Leng needs no convention).

### Exceptions

`eraiser` turns `proc p(): T {.raises.}` into `proc p(): (ErrorCode, T)` and a
`raise` into a return of that tuple. The writeback must be part of that
tuple, so `inoutparams` runs **before** `eraiser`: the lifted result is
`(ErrorCode, R, F1, …)` and a raise path returns the current `F1, …` like any
other exit. A lowering that only wrote back on normal return would change the
behaviour of every scanner that advances `i` and then raises (callers read `i`
for the error position). `defer`/`finally` bodies run before the writeback for
the same reason: the writeback is the last thing before leaving the frame.

### Closures and proc types

The rule applies to every Nim-controlled convention, so `proc (i: var int)
{.closure.}` is lifted too. A proc type's identity already includes its
convention, so `cdecl` and `nimcall` variants of one signature are distinct
types and cannot be assigned to each other; the ABI difference is invisible at
the language level. `lambdalifting` runs before the pass; a `var` param
captured by a closure environment is an address-taker and gets a bracket.

### Generics and `importc`

Instantiation happens before hexer, so a generic `var T` is decided per
instance by the same rule. `importc`/`exportc`/`dynlib` force a foreign
convention and are never lifted; a `nimcall` proc that is `exportc`'d is an
error today for other reasons and stays one.

## Backends

- **Arkham.** Nothing: a 16-byte tuple result already travels in
  `(regs (rax) (rdx))` under the by-value aggregate rule, two scalar params are
  two registers, and SROA/copyprop dissolve the tuple in the caller. The
  hand-written nifasm in the appendix used exactly these features.
- **lengc.** A struct return; gcc returns ≤ 16 B structs in `rax:rdx` under
  SysV and dissolves them. On Windows see the budget note above.

## Staging

1. Scalars and pointers (`i: var int`, `p: var ptr T`), with brackets, the
   raise path and the two diagnostics. This alone covers `parse*`.
2. Small trivially-copyable objects (≤ 2 words) whole, and field marks for
   larger ones. This covers `Cursor`.
3. Marked fields on by-value object params (the `symId` case).
4. Move-in/move-out for `var seq`/`var string` if the numbers justify it.

## Open questions

- Arkham on Windows: follow the platform's 8-byte return budget for `nimcall`
  procs (uniform behaviour, C-comparable) or use `rax:rdx` (faster, but a
  `nimcall` proc's frame layout then differs from what a C debugger expects
  for the same signature)?
- Should the diagnostic for a global passed as `var` to a proc that reads it
  be an error or a warning in the first release?
- Pass placement relative to `constparams` (which introduces the hidden const
  pointers the by-value half interacts with) — before it, so that step 3 can
  reuse the same code path, or after, keeping phase one independent.

## Appendix: the hand-rewritten experiment

`bench/nifbench.nim` compiled with `nimony n -d:danger`, then the emitted
`.asm.nif` rewritten by script and relinked with `nifasm`:

- `skip`, `enterScope`, `leaveScope` (nifcore) and `walk`, `walkOne` (main)
  received `p`/`rem` as two register params and returned them as a two-register
  result `(result (ret :rp (rax) (ptr (u 32))) (ret :rrem (rdx) (i 64)))`, the
  cursor pointer kept for `owner`.
- Around the one cold call (`combinedPayloadSlow`, takes the cursor by
  pointer): store `p`/`rem`, call, reload.
- Before `symId`/`intVal`/`strVal` (by-value cursor, i.e. const pointer):
  store `p`/`rem`.
- `main`'s three `walk` call sites load `p`/`rem` before and store after.

Two variants — in via `rdi/rsi`, out via `rax/rdx`; and in-place in
callee-saved `r14/r15` — were identical in speed. The first is the one the
design above reproduces: it is an ordinary calling convention and costs the
caller nothing when its registers are busy.

Checksums matched the unmodified binary. Best-of-30, interleaved runs:

```
              walk               json
baseline      2.68 ms 130 Mtok/s 0.62 ms
in/out regs   2.35 ms 148 Mtok/s 0.59 ms
gcc -O2       3.05 ms 114 Mtok/s 0.45 ms
gcc tuned     2.79 ms 125 Mtok/s 0.43 ms
```

Static size of the lifted procs (x86-64 instructions / memory operands /
push+pop): `skip` 121/34/10 → 116/8/6; `enterScope` 81/28/10 → 76/15/6.
