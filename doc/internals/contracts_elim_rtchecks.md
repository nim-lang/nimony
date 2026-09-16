# Eliminating run-time checks

Design reference for moving run-time check *emission* out of hexer and into the
pass that already decides whether a check is needed.

Today the two halves are separated by an IR boundary that discards the answer.
`contracts_fir.nim` lowers the module to the Final IR, proves or disproves every
`.requires` and every array index, and returns **only an error buffer**
(`analyzeContractsFinalIr`, `semmain.nim:542` and `:621`). The Final IR is
thrown away. Hexer, which never saw any of it, then emits a check at every site
that could possibly need one, gated on a whole-build flag.

The fix is to stop throwing the IR away: the contract pass's output *is* the IR
the rest of the pipeline consumes, and it emits the checks it could not
discharge. Hexer stops knowing that checks exist.

Implementation points:

- `src/nimony/contracts_fir.nim` — the prover; gains emission.
- `src/finalir/finalir.nim` — the lowering; becomes non-lossy.
- `src/hexer/desugar.nim`, `src/hexer/lengcgen.nim` — lose their check emitters.
- `doc/internals/final_ir.md` — the Final IR itself, and remaining work item 3
  ("structured control flow in the backend"), which this finishes.

---

## Where the checks are today

Only two emitters, both gated on `BoundCheck in c.activeChecks` — a **whole-build**
set threaded from `--flags` (`langmodes.nim:18`, `deps.nim:844`):

1. `desugar.nim:1186,1206` — `nimIcheckAB`/`nimIcheckB` (and the unsigned pair)
   from `(arrat arr idx hi [lo])`. Mirrored in `lengcgen.nim:1539,1556`.
2. `desugar.nim:183` `trRequires` — the `if not cond: panic` guard emitted into
   the **callee's** prologue from its `.requires` pragma.

`RangeCheck` emits nothing anywhere; the name survives only in
`llvmgenstmts.nim`'s case-range check, which is a different thing.

Worth keeping in view: `s[i]` on a `seq` or `string` is a `.requires` call, not
an `(arrat …)`. The second emitter is where the payoff is; the first is the
easy half.

## One artifact, not two

The contract pass's output replaces the module's published nif. `semmain.nim:178-181`
writes exactly one file and indexes it, and that stays true: a second artifact
for the backend would duplicate the index, the staleness rules and the
dependency graph, for a distinction that does not need to exist.

What makes that work is a rule about *what gets lowered*, not about how many
files there are:

> **Lower everything except what is re-sem'd somewhere else.** Generic routine
> bodies, template bodies and concept bodies stay in Nimony IR. Everything else
> is Final IR.

Those declarations are never executed as written. An importing module copies
them, substitutes, and runs sem over the result — so lowering them would mean
sem has to accept `ite`/`loop`/`jmp` as *input to type checking*, and the same
code would be lowered once in the generic and again in every instance.

The rule is not new: it is exactly the set the prover already skips
(`contracts_fir.nim:4391-4443`, `isGeneric` and extern), for exactly the same
reason. A well-formedness verifier can enforce it in both directions — a
lowered region contains no re-sem'd declaration, and a re-sem'd declaration
contains no lowered code.

With that rule, "nimsem understands Final IR" is smaller than it sounds.
Declarations, signatures and pragmas are untouched by the lowering. The real
consumers are:

- `exprexec.nim` — it `tryLoadSym`s imported bodies and executes them at
  compile time, so it has to run Final IR. This is the largest single piece.
  Executing Final IR is easier than executing Nimony IR: no control flow hides
  inside an expression, so there is no evaluation order to reconstruct.
- `renderer.asNimCode` — error messages already render Final IR fragments
  today (every `checkRequires` diagnostic does), so this is completion, not
  invention.
- `idetools.nim` and `indexgen.nim`.

## `for` stays in the Final IR

`trFor` used to `skip` the iterator call *and* the loop variables and emit a
bare `(loop body)`. That is fine for an analysis that throws its input away and
fatal for one whose output is compiled. It now emits
`(for <iterCall> <vars> (stmts …))` — the body lowered exactly as a `loop`'s
is, ending in `(continue .)`, with `break` a forward `(jmp …)` to the trailing
exit label. A `for` is a loop construct that also says what it iterates.

Two things the prover gained for free, because the operands are no longer
thrown away: the iterator call's own arguments are now analysed (a contract on
`items(s)`'s argument is discharged at the `for`), and the loop variables are
declared and marked initialized (`declareForVars`) instead of being symbols
nothing had ever seen.

Iterator inlining therefore stays in hexer (`elimForLoops`, `pipeline.nim:50`),
and the two workarounds that exist because the body is not inlined when the
prover runs stay with it:

- `forRangeAssumes` and the `(assume …)` statements it emits, which is how
  `s[i]` under `for i in 0 ..< s.len` is discharged at all (see
  `doc/internals/final_ir.md`, *`assume`*);
- `trLoopFromBody`'s `forceExitLabel = true`, which stops everything after a
  `for` from reading as unreachable.

### Why late inlining is sound once checks are materialized

This is the part that changes. A check emitted by hexer belongs to whichever
module hexer happens to be compiling; a check emitted by the contract pass
belongs to the module the code was **defined** in, and travels with the code.

So an iterator body carries its own checks, decided by its own module's
prover, judged standalone against its own parameters. When hexer inlines it
somewhere else:

- a body from a lax module inlined into a strict one keeps its checks —
  correct, nobody proved them;
- a body from a strict module inlined into a lax one stays clean — correct, it
  was proven;
- anything a later hexer pass synthesizes is checked by default, because
  nothing downstream knows how to elide.

Cross-module inlining is sound by construction rather than by a flag that has
to be right. That is what licenses leaving `elimForLoops` where it is.

### Dead shapes to remove first

- `doc/tags.md:168` documents `(loop S X S S)` with a `before-cond`/`cond`/
  `body`/`after` shape. That form is dead. `trLoopFromBody` emits the infinite
  `(loop (stmts … (continue .)))` and nothing else does.
- `store` did not pull its weight and is **gone**. `finalir.trAsgn` emits an
  ordinary `(asgn dest value)`, and the consumers that special-cased the
  reversed operand order — `contracts_fir`, `coro_transform`, `funcsummary`,
  `intramodinliner`, the two lengc back ends and five shoggoth passes — kept
  only their `asgn` half. Leng's own `store` shared the tag id and had no
  producer at all, so it went with it; `ithaqua`'s `codegen_wasm` lost the one
  consumer outside this repo.

## `.requires`: function duplication

Every routine with a `.requires` becomes two:

- the **wrapper** keeps the routine's own symbol. Its body is the guard and a
  tail call to the body function. This is what the outside world sees.
- the **body** is a derived symbol that only the contract pass may name. It
  carries no guard and assumes its precondition.

The call-site verdict picks a symbol:

| verdict | emits |
| --- | --- |
| proven | a call to the **body** |
| unprovable, contracts run-time | a call to the **wrapper** |
| unprovable, `staticContracts` | an error |
| disproven | an error |

### Why this and not a guard at the call site

Emitting the guard at each unproven call site is the obvious alternative and it
loses on one point: a call site only exists for a *resolved direct call*. A
proc value, a closure capture, a vtable slot, a hook slot, a `bindSym`, a
callback stored in a field — none of those has a call site to emit at, and the
contract would have to live in the proc **type** and survive matching and
assignment to cover them.

Function duplication covers them without enumerating them. The invariant does
the work:

> The routine's own symbol is the wrapper. The body is a derived symbol only
> the contract pass may name.

Anything that is not a resolved direct call can only reach the name that has
the guard. No type-system change, no case analysis, no list of indirect uses to
keep complete as the language grows.

It also keeps the guard in one place. Call-site emission duplicates it per call
site and leans on the inliner and `condelim` to fold the redundancy back out;
this emits it once and lets the inliner spread it only where it is used.

### Two pieces already exist

- `contracts_fir.nim:4402` seeds a routine's body with its own `.requires` as
  facts. That *is* the "the body may assume what the wrapper checked"
  semantics; it was written for the analysis and the split inherits it.
- `contracts_fir.nim:3270` reads the contract from `fnType` at the call site.
  The per-call-site decision already has everything it needs — the verdict now
  picks a symbol instead of choosing between silence and a diagnostic.

### Rules the split has to obey

**The wrapper must be free on the unproven path.** Guard plus tail call,
unconditionally inline-eligible. It is the smallest function the compiler will
ever emit, so this is achievable by construction — but if it is missed, every
unproven call grows a stack frame and the change is a regression against the
inline guard it replaced. This is the single way the design loses to call-site
emission.

**The body's name is derived mechanically**, so an importing module that proves
a contract can name it without a lookup. Its declaration still has to reach the
index, for the signature and for inlining.

**Generics split at declaration.** The existing instantiation machinery then
duplicates both without knowing about any of this, and the instantiating
module's contract pass picks per instance — which is already where instances
are judged (`tests/nimony/contracts/tstrictinstances.nim`).

**Both declarations carry the contract, meaning different things.** On the
wrapper it is checked; on the body it is assumed. The body says so with
`{.assume}` rather than reusing `.requires` with an implied inversion.

**A proven self-call goes to the body.** The body is self-referential while the
wrapper is what the outside world sees. `4402` already supplies the facts that
make a recursive call provable, so this needs nothing new — but the
name-mangling has to be written knowing it.

**Methods, vtables and hooks hold the wrapper.** This falls out of the
invariant rather than needing its own rule, and is the reason the invariant is
phrased in terms of symbol identity.

**The split is build-mode dependent.** With contracts off entirely there is no
reason to emit a wrapper at all, so the symbol set is a function of the build
mode. Harmless — nimony rebuilds the world — but it is a stated property, not
something to discover from a link error.

## `arrat`

No callee to split, so this stays a direct decision at the site:

- proven → `(arrat arr idx)` when `lo == 0`, `(arrat arr idx . lo)` otherwise.
  The dot in the `hi` slot says "no obligation"; the `lo` operand stays because
  NIFC arrays are zero-based and a `lo..hi` array still indexes at `idx - lo`.
- unprovable, contracts run-time → the contract pass emits the
  `nimIcheckAB`/`nimIcheckB` call itself.
- unprovable under `staticContracts`, or disproven → an error.

`desugar.nim:1164-1216` and `lengcgen.nim:1539-1557` keep only the subtraction.

## What dies

The whole `activeChecks` path: `CheckMode` and `DefaultSettings`
(`langmodes.nim`), `hexer_context.nim:53`, `pipeline.nim:57`,
`desugar.nim:1378`, `lengcgen.nim:2798`, the `--flags` plumbing in
`deps.nim:844` and `nimony.nim:333-342`, and `trRequires`/`emitRequires`/
`emitRequiresGuard` in `desugar.nim`.

`--boundchecks:off` and `-d:danger` stop being a backend flag and become a
third contract feature alongside `runtimeContracts` and `staticContracts`
(`features.nim`): one knob, decided in one place, by the pass that knows what
the knob means.

`RangeCheck` either dies with them or finally gets wired to the prover's
`checkRangeAssign`, which has been there the whole time with no emitter behind
it.

## `was`

The Final IR loses source shape, and something has to map back. Two facts
narrow the requirement usefully.

**In-module macros never see Final IR.** Expansion happens during sem; the
lowering happens after it (`semmain.nim:542`/`:621`). A `typed` macro in the
module being compiled gets pre-lowering trees exactly as it does today. `was`
is for *reflection on imported bodies*, for `renderer.asNimCode`, and for
idetools — not for macro expansion in general.

**`try` and `case` need nothing.** `trTry` (`finalir.nim:665`) keeps
`try`/`except`/`fin` as regions and `case` is kept as-is. What actually loses
shape is `while` (the condition moves into the body as a guard), `if`/`elif`
chains (flattened to `Cjmp`/`lab`), `and`/`or`, `break L` → `jmp`, and xelim's
temps for expression-position control flow.

Note that `(was STR)` already exists as a LengPragma (`doc/tags.md:140`, tag
138) taking a string. The annotation wanted here carries a tag, not a string;
either widen that one deliberately or pick a different name, but do not let the
two collide.

**The staleness rule.** A transparent annotation rots: every later pass either
preserves it or invalidates it, and a stale `was` is worse than none, because
it claims a shape the code no longer has. The rule is therefore structural
rather than a matter of discipline:

> A pass that rewrites inside a `was` region drops the annotation.

The verifier enforces it. This costs provenance exactly where it was going to
lie anyway.

**The rejected alternative** is to annotate less by lowering less — keep the
source shape wherever the lowering buys nothing, so `was` is needed only in the
minority case. It is rejected because the flat `elif` layout is where the 5.3%
Leng size win came from (`doc/internals/final_ir.md`, *What a flat layout
costs*). Trading that for provenance is the wrong direction.

## Remaining work

In rough dependency order:

0. **Done.** `store` is out of `doc/tags.md` and both enums; `loop` is
   documented as the infinite form it has always been emitted as, and the Leng
   consumers that still parsed the old `before-cond`/`cond`/`body`/`after`
   slots — `genstmts.genLoop`, `llvmgenstmts.genLoopLLVM`,
   `induction_variables.loopBodyCursor`, and `trLoopBody` in `copyprop` and
   `cse` — read the single body operand now. Nothing produces a Leng `loop`
   yet, so this is about the tag having one meaning when step 5 gives it a
   producer. What is still missing for that: `continue` is not a `LengStmt`,
   so an infinite loop's back-edge has no spelling on the Leng side.
1. **Done.** `trFor` keeps the iterator call and the loop variables; the
   prover analyses `(for …)` directly.
2. Publish the Final IR as the module nif, with the "unlowered iff re-sem'd
   elsewhere" rule and a verifier check for it. Teach `exprexec`, `renderer`
   and `indexgen` to read it. Hexer gets a Final-IR → Nimony-IR normalizer at
   its entry so the backend is untouched for now — a bridge with a scheduled
   death, removed in step 5.
3. Move emission into the contract pass: `arrat` first (small, self-contained,
   measurable), then the `.requires` split. Delete everything under *What
   dies*.
4. `was`, driven by one consumer at a time. Error messages are the cheapest and
   the most immediately visible.
5. Teach the hexer passes the Final IR directly, one at a time, and delete the
   normalizer. The surface is about nine passes — `coro_transform` (43 sites
   matching `if`/`while`/`block`/`break`/`asgn`), `xelim` (39), `iterinliner`
   (25), `desugar` (23), `lengcgen`/`intramodinliner`/`lifter` (16 each),
   `lambdalifting` (13), `duplifier` (12). `lab`/`jmp` are already Nimony
   statement tags that every pass sees in its ordinary `case n.stmtKind`, and
   `destroyer` already treats `(scope …)` as a real destructor scope, so the
   remainder is `ite`/`itec` vs `if`, `loop` vs `while`, and `continue`/`kill`.

## Measurement

Numbers to take before committing to the `.requires` split, on
`tests/nimony/stdlib/tjson.nim` and its dependency closure — the same closure
`doc/internals/final_ir.md` measures against, so the results are comparable:

- **Symbol and index growth.** Every `.requires` routine doubles, and
  `system`'s `[]` family means that is close to every accessor.
- **Whether dce drops the wrapper** for routines whose every call site was
  proven. If it does, most of the growth above evaporates; that is an
  assumption worth confirming rather than believing.
- **Generated Leng size and link time**, against the current per-callee guard.

## Open questions

- A `.requires` that is not run-time checkable has no guard to put in a
  wrapper. Today's behaviour should carry over unchanged, but the split makes
  the case explicit for the first time: such a routine needs no wrapper, and
  therefore has no safe indirect use.
