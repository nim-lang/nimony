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

- `renderer.asNimCode` — error messages already render Final IR fragments
  today (every `checkRequires` diagnostic does), so this is completion, not
  invention.
- `idetools.nim` and `indexgen.nim`.

Neither compile-time evaluator is on that list, contrary to an earlier draft of
this document. `exprexec.nim` does not interpret anything — it builds a program
and `selfExec`s the compiler on it, so it is a client of the normal compile
path and inherits whatever that path accepts. `expreval.nim` is "an expression
evaluator for simple constant expressions, not meant to be complete" and has no
statement dispatch at all; a `const` is folded during sem, before this lowering
runs, and an imported one arrives as a literal.

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
2. **Done.** The lowering's lossiness — see *The lossiness punch list* below.
   The principle it applied: *not analysed* must stop meaning *not emitted*,
   and its converse, *analysis-only facts do not belong in the IR*.
3. **Teach hexer the Final IR, before publishing it.** The lowering moves into
   `pipeline.transform` as its first step; hexer lowers its own input and its
   passes are converted one at a time, with the published format still Nimony
   IR and the suite green throughout. This is `doc/internals/final_ir.md`'s
   remaining work item 3. The surface is about nine passes —
   `coro_transform` (43 sites matching `if`/`while`/`block`/`break`/`asgn`),
   `xelim` (39), `iterinliner` (25), `desugar` (23),
   `lengcgen`/`intramodinliner`/`lifter` (16 each), `lambdalifting` (13),
   `duplifier` (12). `lab`/`jmp` are already Nimony statement tags that every
   pass sees in its ordinary `case n.stmtKind`, and `destroyer` already treats
   `(scope …)` as a real destructor scope, so the remainder is `ite`/`itec` vs
   `if`, `loop` vs `while`, and `continue`/`kill`.
4. Publish the Final IR as the module nif — the lowering moves from hexer's
   entry back to nimsem — with the "unlowered iff re-sem'd elsewhere" rule and
   a verifier check for it, and `renderer`/`idetools`/`indexgen` taught to read
   it.
5. Move emission into the contract pass: `arrat` first (small, self-contained,
   measurable), then the `.requires` split. Delete everything under *What
   dies*.
6. `was`, driven by one consumer at a time. Error messages are the cheapest and
   the most immediately visible.

**No normalizer.** An earlier draft had hexer converting Final IR back to
Nimony IR at its entry, so the backend could stay untouched across step 4.
That does not work: `iterinliner` splices in iterator bodies loaded from *other
modules'* published nifs (`tryLoadSym`, `iterinliner.nim:612,699`), which an
entry-side normalizer never sees. Converting on load instead would put the
bridge inside `programs`, which is worse. Doing step 3 before step 4 removes
the need for one — hexer already speaks Final IR by the time the published
format changes, and a cross-module body arrives in exactly the form it wants.
The transitional cost is that the lowering runs twice, once in nimsem for the
prover and once in hexer; that is compile time, not correctness.

## The lossiness punch list

From an audit of `finalir.nim` done once `trFor` showed what the failure mode
looks like. The structural cause behind several entries: the file's idiom is
`n = sub(n); <consume some children>; n = xStart; skip n`, and unlike `into`,
that resync never asserts the children were consumed — so anything the body did
not handle disappears silently.

Two are fixed:

- **`trFor`** dropped the iterator call and the loop variables.
- **The fabricated borrow declarations.** `extractForBorrow` /
  `addForBorrowDecls` emitted a *second* declaration of each `mut`/`lent` loop
  binder, with a deliberately fake `(haddr firstArg)` initializer, so the prover
  would treat the binder as a borrower. It worked only because it replaced the
  real declaration that `trFor` was throwing away; once `trFor` kept that, the
  binder was defined twice. The fact is derived in `contracts_fir.declareForVars`
  now, from the iterator call the `for` node carries. Two consequences worth
  knowing: a `for` binder has to be exempted from the `let`-reassignment check,
  because the binding protocol (tuple unpacking, a closure iterator's resume)
  assigns to it and the binder is marked initialized at the `for`; and the path
  had **no** test — `tborrow_errors.nim` only *simulates* it with a `var`
  parameter — so `tborrow_lifetime_errors.nim` now pins it.

- **`{.assembler.}` bodies** (`trProcDecl`) used to be replaced by a bodyless
  declaration: `skip n; dest.addDotToken()` threw away hand-written machine
  code. The lowering passes the body through verbatim now — deliberately
  *un*-normalized, since source order is the contract for such a body — and
  `traverseProc` skips it, alongside the generic and extern cases it already
  skipped. Note this one is unobservable by any test until step 4: the
  lowering's output is still discarded, so nothing downstream can see the
  difference. It was verified by lowering a module with an `{.assembler.}` proc
  through `finalir.nim`'s standalone driver and reading the result. The path has
  no positive test either way — every test in `tests/nimony/assembler/` is a
  negative one, because the C backend refuses such a proc by name and only the
  native backend compiles it.

Also fixed, in the order they were worked:
- **`trStmt`'s fallback** is now explicit rather than accidental. Measured with
  `-d:firFallbackProbe` (one line per statement that lands there, spelled like
  `-d:contractStats`) over the `tjson` closure, what actually reached it was
  `jmp` (163), `lab` (122), `import` (14), `yld` (11), `destroy` (11),
  `comment` (8), `pragmas` (6) and `incl` (5) — each of which the fallback
  handled correctly, but by accident. They have their own branches now, saying
  why: `lab`/`jmp` are already this pass's own vocabulary coming back at it,
  the module bookkeeping is not code, and the rest are statements whose
  children are plain expressions. Nothing reaches the fallback any more.

  `CoroforS` — a loop whose body the fallback would leave un-lowered — is the
  case that matters, and it is **not reachable today**: its only producer is
  hexer's `iterinliner`, downstream of every caller of this pass. It becomes
  reachable at step 3, when the lowering moves into hexer's pipeline. Re-run
  the probe then; that is what it is for.
- **`trIf`/`genIfViaCx` silently dropped branches past the first elif+else.**
  The `n = ifStart; skip n` resync consumed whatever the proc did not read, so
  a third branch vanished from the generated code with nothing said — and the
  `assert` that stood for the invariant compiles out of a `-d:danger` build,
  after which a second `elif` was lowered *as* an `else`, its condition treated
  as a statement body. Both procs `bug` on it now, which is this file's idiom
  for an invariant it relies on (`bug "cfvar in Final IR input"` and friends).
  The whole suite passes with the invariant enforced, so `xelim` does nest
  every elif chain as the precondition claimed.
- **`trAsgn`'s two paths are one path.** A non-symbol destination sent the value
  through `trExpr`, which rejects a call outright, where a symbol destination
  used `trBoundExpr` and bound the call directly to its destination. That held
  only because `xelim` hoists such a call into a temp first — and
  `final_ir.md`'s remaining work item 2 is precisely about changing when it
  does that. The asymmetry also cost the path its `callIsOver` markers. Both
  use `trBoundExpr` now; the lowered output of 340 modules is byte-identical,
  so this is a simplification today and a removed trap later.
- **Analysis-only nodes in the statement stream.** `(unknown …)` after every
  call with a `haddr` argument and `(kill …)` at every scope exit are emitted
  unconditionally; hexer will have to consume or tolerate them. `(assume …)`
  stays — it is a declared Final IR construct with a documented job.
- **`trCase` and `trTry` silently dropped unexpected trailing children** — the
  same resync class as `trIf`, and worse in one respect: the `addParRi` has
  already closed the node, so there is nowhere to put such a child even if it
  were noticed. Both `bug` on it now.
- **Close-paren line info** is preserved at the six sites that close a node
  taken from the input — `trLocal`, both `trAsgn` paths, `trRet`, `trRaise`
  and `trFor` — where a bare `dest.addParRi()` used to drop it, against
  `takeInto`'s stated contract. Each captures `n.endInfo` at a point where the
  cursor provably sits at the node's `)`; that was checked with temporary
  assertions over 436 modules (zero failures) and the assertions then removed,
  since an unreachable state does not need a permanent guard. The remaining
  bare `addParRi()` calls close nodes this pass *synthesizes* — `kill`, `lab`,
  `ite`, `loop`, the `for` body's `stmts` — which have no input close to keep.

Closed as intentional, not deferred:

- **`(scope …)` normalised to `(stmts …)`** and **`block` and its source name
  dropped**. Both are by design — a `block` has no Final IR construct of its
  own (`body` plus `(lab blockExit)` is the lowering), and scope-ness is
  re-expressed as the emitted `(kill …)` set. Neither is textually
  reconstructible, so if `was` is to render a `block` back for diagnostics it
  will have to carry the source name; that is a `was` requirement, recorded
  there rather than a lossiness bug here.

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
