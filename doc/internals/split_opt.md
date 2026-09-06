# Variable live-range splitting

A plan for a Shoggoth pass that renames a local's independent value ranges
apart, so that a value which merely *shares a name* with a value living across
a call does not pay for that call.

Status: **implemented, measured, and deleted.** Phase 1 was written, passed its
own self-tests and `hastur tiers native`, and moved 5 prologue pairs out of
1187 — because the payoff it was designed for had already been collected by a
cheaper change in arkham, the death-point exemption this document predicted
(nativenif `a42f158`). It is not in the tree. The implementation, its eleven
self-tests and the driver wiring are in nimony commit `9fc87ab7`
(`src/lengc/shoggoth/livesplit.nim`), one `git show` away should the balance
ever shift. **Read "The verdict" below before spending time here.** The design
that follows is unchanged and still correct; only its value estimate was
wrong.

Numbers below are from the nifbench corpus (42 `.oc.nif` modules) on
2026-09-05 (design, 670 procs) and 2026-09-06 (verdict, 682 procs — the corpus
was rebuilt in between, so compare within a row, never across the two dates),
AArch64.

Related: `doc/internals/final_ir.md` (the normal form these transformations
assume), `doc/internals/cse.md` (the path model), `src/lengc/shoggoth/scalarizer.nim`
(the closest existing pass — same shape, different axis).

---

## The verdict (2026-09-06)

Phase 1 was implemented exactly as specified below, with one simplification that
turned out not to cost anything measurable: instead of a `Tracker`-based web
analysis, a split point is any whole-variable assignment that is
**unconditionally executed** with respect to its own declaration — same region
(same conditional arm, same `scope`; `stmts` is transparent, it emits no braces
in the C backend and shares arkham's scope frame), not jumped over by any
`jmp`/`lab` span, and not reading the variable it assigns. Then the rewrite is
positional and needs no reaching-definition merge at all. Everything before the
assignment is the old name, everything from it onwards the new one, on every
path.

It works, it is sound, and it does not pay.

### The 2x2

Prologue `stp` pairs over the corpus, 682 procs, all four cells emitted by the
same arkham:

|                          | no split | split      |
|--------------------------|---------:|-----------:|
| **no death-point**       |     1310 | 1298 (−12) |
| **death-point exemption**|     1187 | 1182 (−5)  |

`framed` is 501 in all four cells.

nifbench `walk`, callgrind: 480,297,999 → 480,297,757 instructions. That is
−242 out of 480 million, five thousandths of a percent — noise with a sign.
The checksum is unchanged, and `hastur tiers native` is 27/27 with the pass on,
so this is a correct transformation that transforms nothing worth transforming.

### Why the estimate was wrong

Two reasons, and the document above predicted both without drawing the
conclusion.

1. **The death-point exemption gets there first.** It was listed above as "a
   neighbouring defect, measured on the way, that is not this pass", worth 127
   of the 838 denied locals — with the warning that the 82/31-pair estimate was
   computed before that breakdown and should be re-derived. It should have
   been: the exemption took 123 pairs, and the values it took are largely the
   same values splitting would have freed. Splitting a range so that its dead
   tail can use a volatile buys nothing once the *call that kills the value* has
   stopped denying that volatile in the first place. The exemption is one
   interval test in the analyser; the split is a whole pass.

2. **`framed` was never reachable.** The gate below asked for `pairs` to drop
   *and* `framed` to drop. A proc that makes any call must save `x30`, and `x30`
   is saved in a pair with a callee-saved register — so every proc with a call
   has at least one pair no register-allocation change can remove, and a proc
   without a call was never denied `AllRegs` to begin with. `framed` can only
   move for a leaf that needed callee-saved registers for reasons other than
   calls. Splitting cannot move it by construction, and asking it to was asking
   for the wrong number.

### Where the candidates go

Instrumentation in that implementation (a per-gate counter dumped once per
body) over every module compiled for a nifbench build — 114 shoggoth processes,
62,802 candidate locals:

| gate | count | |
|---|---:|---|
| single assignment — nothing to split | 24,878 | 40 % |
| a whole-variable def, but not in the declaration's region | 21,816 | 35 % |
| an occurrence inside a loop (phase 2) | 13,714 | 22 % |
| address-taken | 944 | 1.5 % |
| the cost model refused (no range comes out call-free) | 955 | 1.5 % |
| jumped over | 17 | |
| **split** | **478** | 0.8 % |

The two big buckets are the ones phases 1 and 2 leave on the table, and they
are what a full web analysis with branch merges would reach. But the ceiling
they are measured against is the 12-pair column, not the 1310: the whole
opportunity, before the exemption, was 12 pairs from 42 splits. Doubling or
tripling the number of splits against that ceiling is not worth a
reaching-definition analysis, and against the 5-pair column it is worth
nothing at all.

### What would change this

Nothing about splitting. The remaining prologue pairs are not values whose
ranges merely *look* live across calls — they are values that genuinely are,
and the lever for those is the one this document already named and did not
take: **shrink-wrapping** (sink the save into the region that uses the saved
register), which is an arkham change and not a Shoggoth one. See the
`arkham-shrinkwrap-measured-dead` note before starting, since a previous
attempt at it measured dead for its own reasons.

If the balance ever shifts — a change that creates many more multi-value locals,
or an arkham allocator that makes a call-free range worth more — start from
`9fc87ab7` rather than from this document: the mechanism there is finished and
its self-tests are the specification of what each gate refuses. What that
revival would have to beat is the 5-pair column, not the 1310.

---

## The problem, stated once

Arkham decides per local whether it may live in a *caller-saved* register. The
rule is an interval test (`src/arkham/core/analyser.nim`):

> A local gets `AllRegs` — the licence to use a volatile register — iff no call
> position lies within `[liveStart, freeAfter]`.

`liveStart` is the declaration's token position. `freeAfter` is the *textually
last* occurrence of the name, extended over any enclosing loop the declaration
sits outside of. That is one interval per **name**, and it has no holes.

So this local is denied a volatile register:

```
(var :x . (i +32) (call f))     ; x live from here …
(asgn y x)                      ; … last read of THIS value
(call g)                        ; a call inside [liveStart, freeAfter]
(asgn x (call h))               ; a NEW value, unrelated to the old one
(asgn z x)                      ; … read here
```

`x`'s interval spans `(call g)`, so `x` gets a callee-saved register and the
proc pays a `stp`/`ldp` pair on entry and exit — even though no value of `x` is
ever live across `g`. The two assignments define two independent values that
happen to share a slot because the frontend reused the name.

The fix is to give them different names:

```
(var :x . (i +32) (call f))     (var :x . (i +32) (call f))
(asgn y x)                      (asgn y x)
(call g)                   ⇒    (call g)
(asgn x (call h))               (var :split.0.M . (i +32) (call h))
(asgn z x)                      (asgn z split.0.M)
```

Both intervals are now call-free, both values get volatiles, and the proc's
callee-saved demand drops by one. Nothing was copied, nothing was spilled: the
transformation is a **renaming**.

## Why this belongs in Shoggoth and not in arkham

Arkham is a pure-emit code generator: one analysis pass, one emit pass, no
rewriting of its input. It cannot rename, because it has no IR to rename in —
it emits as it walks. Its analyser deliberately keeps *one coarse interval per
name* because that is what a single forward walk can compute; a def-use web
needs a second pass and a union-find, which is the shape of an optimizer, not
of an emitter.

Shoggoth already is that optimizer, and it already has every piece:

- `patchsets.nim` — position-keyed rewrites applied in one rebuild pass.
- `trackers.nim` — the branch-aware flow-sensitive state `cse` and `copyprop`
  share, including `gotoLabel`/`landLabel` for `jmp`/`lab`.
- `scalarizer.nim` — the precedent for minting fresh locals and substituting
  every occurrence of an old one. SROA splits a variable along the *field*
  axis; this pass splits it along the *time* axis.
- The escape/address-taken gate all three passes already use.

And one Shoggoth pass serves every arkham target (x64, AArch64, Cortex-M,
RV32) plus the C backend, where a split is simply an extra local that the C
compiler's own allocator coalesces back if it does not want it.

## What is at stake — the measurement

Emitted prologues over the corpus (`bin/arkham -a:linux_arm64`, counted by
leading `stp`/`fstp` runs):

```
procs 670   framed 437   pairs 1139   mean 2.61   leaf 287   leaf-framed 89
pairs histogram: 0:233  1:137  2:81  3:107  4:55  5:46  6:8  7:3
```

Arkham's own analyser, built with `-d:arkhamPeakLive`, reports per proc the
peak number of simultaneously-live values denied `AllRegs` (`XCALLPEAK`).
Summed over the corpus that is 1861 cross-call values in 366 procs, and **128
procs exceed the 5-register callee-saved pool** — those are forced to spill to
the stack, so splitting there does not merely save a pair, it removes a
store/load per value.

A scan of the corpus `.oc.nif` for the pattern above (script: scratchpad
`split_scan.py`; a *textual* model — no CFG, no loop reasoning) gives:

| | count |
|---|---|
| local, non-address-taken, denied `AllRegs` today | 838 in 252 procs |
| …denied *only* by the call that consumes the last use | 127 (59 of them a `=destroy`) |
| …denied by that call **and** a mid-range call | 116 |
| …denied by a genuine mid-range call — **this pass's target** | 595 |
| …of the 838, with ≥2 definitions (the ceiling for renaming) | 277 |
| …with at least one textual split point | 216 (133 outside loops, 83 involving a loop) |
| …conservatively splittable into all-call-free segments | 82 in 70 procs |
| procs that lose **every** cross-call value | 8 |
| procs that lose some | 62 |

Converting the surviving intervals to a peak-demand pair count: **31 of 337
modelled pairs, 9.2%**, with 30 procs pushing strictly fewer pairs.

Two cautions, both of which the shrink-wrapping post-mortem earned
(`arkham-shrinkwrap-measured-dead`):

1. The "conservatively splittable" row excludes every variable with an
   occurrence inside a loop. That is 83 of the 216 candidates — phase 2 is
   worth roughly as much again as phase 1, and the estimate above is a
   **lower** bound only if the loop rule is the sole conservatism.
2. Vars are not pairs. Fixing one of three cross-call values in a proc saves
   zero pairs. The only number that counts is the emitted `stp` census, and
   that is what the gate below measures.
3. The 82 and the 31 pairs were counted before the denial-cause breakdown and
   do not exclude the death-point class, so some of them would be fixed by the
   arkham-side exemption below instead. Re-run the split model against the 595
   alone before treating either figure as this pass's yield.

For scale: the previous cut in this area (letting a call-free local home in an
argument register, nativenif `63e6aef`) moved 1243 → 1139 pairs, −8.4%, and
bought −1.15% instructions retired on nifbench's walk benchmark. This is the
same order of magnitude, from a different direction.

### A neighbouring defect, measured on the way, that is not this pass

The interval test is `p > lo and p <= hi` over call positions, where `hi` is the
value's last occurrence. In Leng a call's tag precedes its arguments, so for

```
(call =destroy x)          ; the `call` tag's position < x's position
```

that call sits *inside* `x`'s own interval and denies it `AllRegs` — even though
`x` is dead the instant the call executes and its home is exactly where the
marshalling wants it. Any local whose last use is a call argument is in this
class, destructible locals inevitably so.

Arkham already has the mirror-image exemption at the other end: `initClass ==
icCall` moves `lo` to just inside the initializer, because a `let x = f(…)`'s own
call precedes the value's existence. The **death-point exemption** is the same
argument run backwards, and it would clear 127 of the 838 outright with no IR
change at all.

It is not free, and it is not claimed here: a value homed in a volatile that is
also an argument register can be overwritten by the marshalling of a *later*
argument before its own is staged. That is what `stagedArgs` and
`releaseArgDest` exist for (`src/arkham/risc/emit.nim`, wired up in
`risc/value.nim`, with an x86-64 twin), and whether they already cover it needs
checking rather than asserting.

It belongs in arkham, not Shoggoth, it is cheaper than this pass, and it is
listed here so that its 127 are not later credited to splitting. The 595 in the
table above are what remains for splitting after it, and are the number this
plan should be judged against.

## The transformation

### Webs

A **web** is a maximal set of definitions and uses of one name connected by
reaching-definition edges: every use joins the web of every definition that
reaches it, and two definitions that reach a common use are in the same web.
Two webs of the same name are independent values and may be renamed apart.
This is Chaitin's renaming step; SSA construction would hand it to us for free,
but Shoggoth is not in SSA and does not need to be for this.

Concretely, per proc body, per candidate local:

1. Walk the body with a `Tracker[SymId, WebId]` holding "the web currently
   reaching this name". A definition opens a fresh web; a use joins the
   tracker's current web. `closeBranches` merges the arms — a name whose arms
   left different webs unions them.
2. Union-find over the web ids.
3. If more than one web survives, and splitting removes a call from at least
   one web's span, rewrite: the first definition of each non-original web
   becomes a `(var :split.N.<bodySuffix> <pragmas> <type> <value>)` and every
   occurrence in that web is substituted, via `Patchset`.

### What the pass does *not* do

**It never inserts a copy.** The textbook second half of live-range splitting
cuts a genuinely-live range at a program point and inserts `x2 = x1`. That buys
nothing here: if the value is live across a call, so is the copy, and the copy
target needs the same callee-saved register the original needed. Arkham already
spills when it must; a Shoggoth-level spill would only take the decision away
from the allocator that has the register file in front of it. Splitting pays
exactly when the value is **dead** across the call, and then no copy is needed.

**It does not sink declarations.** `liveStart` is the declaration position, so
a `(var :x T .)` at the top of a scope whose first store is after a call would
also over-extend. Scanning the corpus for that shape found it does not occur:
hexer emits declarations at their initialisation. Nothing to do here.

**It does not rematerialize.** A cross-call value whose defining expression is
pure and cheap could be recomputed after the call instead of carried. That is a
real and separate lever, it needs a cost model rather than a legality rule, and
it is out of scope for this document.

## Legality

The pass reuses the gate `copyprop` and `scalarizer` already share, plus the
control-flow constraints that renaming (unlike copying) introduces.

1. **Address-taken disqualifies.** If any `addr`/`haddr` spine roots at the
   local (`copyprop.addrRootOf` is the exact test — it stops at a `deref`/`pat`,
   because addressing a *pointee* does not address the pointer's storage), the
   local's storage escapes and a rename would split one object into two.

2. **Destructible locals are candidates, not exclusions.** `=destroy(x: T)`
   takes the object, not a `var T`, and hexer passes it that way: over the
   corpus the first argument of a destroy call is `(dot x f …)` 229 times and a
   bare symbol 136 times, against 22 `haddr` and 3 `deref`. The `haddr` minority
   is `constparams.nim` deciding a large or inheritable type travels by const
   ref — those *are* address-taken and rule 1 removes them; the other 365 are
   not.

   So the pass must handle destructors rather than assume them away — and that
   is the good case, because it is destructible 16-byte locals (strings) that
   most often occupy a callee-saved pair. A destroy call is an ordinary **use**
   of the web live at that point, and the transformation stays sound for free:

   - ARC destroys the old value before a reassignment, so `(call =destroy x)`
     precedes the redefining `(asgn x …)`. The destroy therefore lands in the
     *old* web and each web ends with its own destruction. The rename copies
     that structure one-to-one.
   - A scope-exit destroy reached from several arms is a use reached by several
     definitions, so `closeBranches` unions those webs and no split is offered.
     Conditional reinitialisation protects itself.
   - A local that is moved from is written through `=wasMoved(haddr x)` — every
     `=wasMoved` in the corpus takes `haddr` — so it is address-taken and rule 1
     already removed it.

3. **A web must not cross a back edge.** A definition in a loop body and a use
   on the next iteration are one value; the textual order does not show it. The
   `Tracker`'s loop handling must union across the back edge, or — phase 1 —
   the pass simply skips any local with an occurrence at loop depth > 0. Note
   that arkham's `freeAfter` already extends over the enclosing loop for exactly
   this reason, so a wrong answer here is *not* caught by the allocator.

4. **`jmp`/`lab` merge like branches.** `trackers.gotoLabel`/`landLabel` already
   model the forward, scoped jumps `xelim` emits (`final_ir.md`); a web reaching
   a `jmp` must union with the web live at the landing `lab`. `jumpsAreWellScoped`
   is the precondition; a body that fails it is skipped.

5. **`onerr` / `errv` edges.** A call that may raise transfers to the `onerr`
   action. Treat it as a branch out of the current point into the action's
   target, or — phase 1 — skip bodies containing `onerr`.

6. **Whole-variable definitions only.** `(asgn x <expr>)` with a bare symbol
   LHS opens a new web. `(asgn (dot x f) …)`, `(asgn (at x i) …)`, and
   `(store <expr> x)` are partial or reversed-operand writes: they are *uses*
   of the existing value as far as webs are concerned, and must not open one.

7. **`result` is a name the backend knows.** Nimony's `result.N` locals feed
   the return path; leave the web containing the final `(ret)` under the
   original name so no later pass has to chase a rename.

8. **Types must match.** The new declaration copies the original's type and
   pragmas verbatim. If the original is a `(var)` with pragmas the pass does not
   model, it bails rather than guesses.

The transformation is a renaming of a *provably dead* value's storage: nothing
observes the identity of a non-address-taken local, so if the web analysis is
right the change is unobservable. Rule 1 is what guarantees "nothing observes";
rules 3–5 are what makes the web analysis right.

## Where it runs

```
rewrite → ctorproj → scalarize → copyprop → unswitch → rewrite → indvars →
cse → SPLIT → vectorize → sinkret/tailcall
```

After `cse`, before the encoding passes. The reasoning:

- **Every earlier pass wants fewer names.** `copyprop` collapses `y = x` chains
  and `cse` shares subexpressions across names; splitting first would hand them
  more names to reason about and more chances to bail. Splitting last means it
  operates on the final name set, which is also the set arkham sees.
- **Splitting is not undone by anything downstream.** It inserts no copies, so
  `copyprop` has nothing to propagate away even if it ran again, and dead-store
  elimination sees the same stores under new names.
- **Before the vectorizer**, whose `(instr …)` output is selection-final, and
  before `tailcalls`, which rewrites `ret`s.

Plumbing, mirroring the existing passes:

```nim
# src/lengc/shoggoth/livesplit.nim
proc runLiveSplit*(buf: var TokenBuf; bodySuffix: string; m: ptr MainModule)
```

called from `optdriver.optimizeBody` under `if passOn("split")`, and listed in
the `SHOGGOTH_DISABLE` name list in `optdriver.nim`'s docstring. The bisection
name matters as much as the pass: an optimized build that misbehaves must be
answerable with one run.

## Cost model

Split **only** when it removes a call from at least one resulting web's span.
An unconditional renaming would grow the IR, grow the C backend's local count,
and buy nothing for a value that was already call-free. The test is cheap: the
pass has the call positions from its own walk, so it is an interval check per
candidate web, done before any patch is recorded.

Deliberately *not* in the cost model: a register-pressure estimate. Splitting
does not increase the number of simultaneously live values — the webs were
already disjoint in time — so there is no case where it makes allocation
harder. It can only move a value from the callee-saved class to the volatile
class.

## Phases

1. **Straight-line webs.** No loop-depth occurrences, no `onerr`, `jmp`/`lab`
   handled by the tracker. This is the 82-variable / 8-frameless-proc row of
   the table, and it is enough to validate the mechanism end to end.
2. **Loops.** Union across back edges properly. Reaches the other 83 textual
   candidates; the arkham-side payoff is larger than phase 1 because a
   loop-carried false web currently extends `freeAfter` over the whole loop.
3. **`onerr` bodies.** The exception edge as an ordinary branch in the tracker.

Phase 1 is the deliverable; 2 and 3 are only worth starting once phase 1's
emitted-prologue delta is on the table.

## Gates

- **Correctness, nimony side:** `hastur all` at the documented baseline
  (626/643 here — see `nimony-preexisting-test-failures`), and the
  `--boot-backend:native` fixed point, with and without
  `SHOGGOTH_DISABLE=split`.
- **Correctness, nativenif side:** `tests/tester` green on all targets, and
  `hastur native` at 110/116. The pass changes arkham's *input*, so the
  byte-identical gate (`arkham-byte-identical-gate`) does not apply as an
  equality check — but it does apply as a *diff review*: emit the corpus for
  all six configurations before and after, and read the difference rather than
  count it.
- **Payoff:** the `stp` census above, re-run on the corpus. The pass is worth
  keeping iff `pairs` drops materially. (This gate originally asked for `framed`
  to drop too; see "Why the estimate was wrong" — that number is unreachable for
  any register-allocation change, since a proc with a call must save `x30` in a
  pair.) A version that renames widely but does not move `pairs` is a loss and
  should not be tuned. The measured answer is 5 of 1187, which is why the pass was
  deleted rather than tuned.
- **Performance:** nifbench, `valgrind --tool=callgrind` instruction counts on
  the walk benchmark, checksum unchanged at 929433840.

## Open questions

- **Does the win survive arkham's own lowering?** The corpus scan reads
  `.oc.nif`; arkham introduces its own temps below that, and some of the 1139
  pairs belong to values that never had a Leng name. The phase-1 measurement
  answers this and nothing before it does.
- **Aggregates.** A ≤16-byte aggregate local occupies a register pair; the web
  analysis is the same, but the payoff is two registers per split. Strings are
  the common case and they are destructible, so rule 2 is what decides how many
  of them the pass can reach. Above the const-ref threshold `constparams.nim`
  makes the local address-taken and rule 1 removes it — which caps the payoff
  at exactly the sizes where a register pair was possible anyway. Worth a
  separate count once phase 1 exists.
- **Should the split be expressed with `(scope …)` instead of a fresh name?**
  Leng's `scope` bounds a lifetime and arkham gives each `scope` its own frame.
  It would need no new symbols — but it needs the two webs to be textually
  nested, which webs in general are not. Renaming is the general mechanism;
  scoping is at best an occasional prettier spelling of it.
