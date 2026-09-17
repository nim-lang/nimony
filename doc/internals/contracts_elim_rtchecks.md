# Eliminating run-time checks

Design reference for moving run-time check *emission* out of hexer and into the
pass that already decides whether a check is needed.

Today the two halves are separated by an IR boundary that discards the answer.
`contracts_fir.nim` lowers the module to the Final IR, proves or disproves every
`.requires` and every array index, and returns **only an error buffer**
(`analyzeContractsFinalIr`, `semmain.nim:542` and `:621`). That Final IR is
thrown away. (The lowering itself is not unused: `hexer/coro_transform.nim`
runs `toFinalIr` on a coroutine wrapper and walks the result, so it is a real
consumer of this pass's output today — and the first one any change to the
lowering has to answer to.) Hexer, which never saw any of it, then emits a check at every site
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
3. **Done: hexer speaks the Final IR.** The lowering is the first step of
   `pipeline.transform`, ahead of every pass — `elimForLoops` included — and
   the old `xelim1` run is gone. It was brought up behind an environment
   switch, pass by pass from the back of the pipeline to the front, and made
   the only path once the whole suite, the native tier and the self-host boot
   (stages 1 = 2 = 3) passed with it. It emits no
   `kill`/`unknown` (`toFinalIr(analysisFacts = false)`): they name locals
   `lambdalifting` may move into an environment, and nothing in hexer needs
   them. The original estimate of
   about nine passes was too high: `cps` converted the Final IR back
   (`ite`→`if`, `loop`→`while true`, `kill`/`unknown` dropped) for *every*
   routine, not just coroutines, so at first only the passes between the
   lowering and `cps` had to learn it. What that took:
   - `controlflow.nim` (the duplifier's last-read analysis): `ite`, `loop`,
     and skipping `kill`/`unknown`.
   - `destroyer`: `ite`, `loop` and `continue` — the back-edge ends the body's
     scope, so the destructors run *before* it; `collectLabels` looks through
     transparent `stmts`. The loop body is walked in place, because `cps`
     recognizes the back-edge only as the body scope's last child.
   - `eraiser`: a `jmp` owes the `finally` of every `try` region that does not
     declare its label (the Final IR spelling of `break`); a replicated
     `finally` renames its labels, and pushes a frame so a `jmp` inside the
     copy unwinds nothing.
   - `eraiser`/`duplifier` emit `ite` for the checks they synthesize.
   - `duplifier`: an `{.inline.}` call temp *is* its call. `ensureMove x[0]`
     and the self-assignment check (`result = result.kids[0]`, a segfault in
     the `parsegen` plugin) both judge the call, not the temp.
   - `cps` and `lambdalifting`'s closure-iterator transform no longer lower
     a routine themselves (`coro_transform.treIteratorBody`).
   - `lambdalifting`: `lab`/`jmp` operands are labels, not captures; a
     closure call's callee temp and its env==nil dispatch, and a capturing
     iterator value's frame setup, go in front of the statement (`hoisted`,
     `treStmt`) instead of into an `(expr …)`/`if` expression; the `corofor`
     trampoline is spelled `loop`/`ite`/`jmp` (`emitWhileBegin`'s `exitLab`).
   - `desugar`: the same `pre`/`trStmt` pattern for what its expansions
     need first (set operations, runtime set constructors, string-concat
     chains, and the `arrat` bound check, bound to an `{.inline.}` temp as
     `xelim` used to); its loops, `if`s and `break`s are spelled
     `loop`/`ite`/`jmp`; an `and`/`or` whose right operand needs statements is
     materialized (`trShortCircuit`). `xelim` leaves a `string.&` chain
     nested (`trConcatChain`) so `desugar` can still fold it into one
     allocation — without that, the folding silently stopped.
   - `iterinliner`: a `for` arrives as `(for call vars (scope body
     (continue .))) (lab exit)`, every `break` already a `jmp exit`. Each
     `yield` gets a copy of the body without its back-edge. The iterator body
     comes from `tryLoadSym`, i.e. from a nif that is not lowered yet, so it is
     lowered on the spot, together with the parameter declarations so the
     types resolve (a local iterator was published from the lowered module
     and is used as is). Every copy, and every lowered body, declares its own
     names: a lowering run restarts its temp and label counters. `xelim`
     leaves the `for`'s iterator call in place instead of binding it to a
     temp, and registers the loop variables.
   - The lowering spells a source-level `continue` as a `jmp` to a label in
     front of the back-edge, so `(continue .)` is only ever the last
     statement of a loop body, as this document says it is. That label sits
     *after* a scope of the body's own: a `jmp` may leave a scope but never
     skip a declaration inside one, whose destructor the scope's end would
     then run uninitialized (`tcontinue_skips_decl`; it crashed the `parsegen`
     plugin).
   - `cps`'s escape analysis pins the first argument of an
     `.establishesBorrow` call: `borrowFromLocal` in `tpassive_openarray`
     only ever passed because the scope-end `kill buf` counted as a use in a
     later state.
   - typenav's `crossedProc` counts the routine boundaries between use and
     declaration. It counted only those above the first, which is 0 for a
     local of a `scope` inside the enclosing routine — and every `block` is
     one now (`tclosure_block_capture`).

   Found on the way and fixed for the default build too: `xelim` bound a call
   in an aggregate to a `cursor`, leaking its result (`taggregate_call_temp`);
   the lowering turned a statement-position `stmts` into a scope; a replicated
   `finally` duplicated its labels.

   `lengcgen` reads the Final IR too, so `cps` no longer converts it back:
   `ite`/`loop` become Leng `if`/`while true` there (Leng's own `ite`/`loop`
   are unknown to the native back end and most optimizer passes), and the
   Nimony `if`/`while`/`block`/`break` handlers are gone. The last Nimony
   producers were the lifter (it runs in nimsem too, so hexer lowers its hooks
   with `toFinalIr`), `cps`'s trampoline and frame code, `vtables`,
   `xelim_final`'s `and`/`or`, and an `{.assembler.}` body's `if`.

   Left: retire `xelim`'s `ElimExprs` goal, which nothing uses any more. Measured
   against the old pipeline: on `tjson` the C text is 11% larger (temps and
   labels) and the executable 0.08%; `tall` compiles in 4.4 s against 4.2 s.
   `json.$` is inlined at six call sites in `tjson` by the old pipeline and
   not by the new one; its body is one statement
   larger there (an aggregate's call temp moved through a snapshot), but
   removing that statement did not change the decision — the inliner spends
   its per-caller budget on different callees first. Removing it by
   delaying the temp's `=wasMoved` to after the statement is unsound, by the
   way: a `ret` of the constructor destroys the temp before the delayed
   reset runs. Once step 4 publishes lowered nifs, the iterator inliner's
   on-the-spot lowering goes away.
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

- **`(scope …)` is kept, not normalised to `(stmts …)`.** This was first
  closed as intentional — scope-ness being re-expressed as the emitted
  `(kill …)` set — and that was wrong. Much of hexer keys off `scope`, not off
  `kill`: `destroyer` treats `(scope …)` as a real destructor scope and
  `(stmts …)` as transparent, and its own comment says the flat `lab`/`jmp`
  branch layout *depends* on that, because a branch body is a sibling in the
  enclosing statement list rather than a child of an `(elif …)`. Emitting
  `stmts` would have let a local declared in a branch live to the end of the
  enclosing region. The lowering emitted **zero** `scope` tags before this;
  it now emits one wherever it opens a scope (`trScopedBody`, and the loop and
  `for` bodies, whose locals die each iteration).

  The `kill`s stay for the prover, which is what reads them, and are dropped on
  the way out rather than published: hexer re-derives destruction from the
  scope rules, so carrying both says the same thing twice. Measured, that is
  11,986 instructions and 2.0% of the lowered IR — modest, but it buys nothing
  downstream. `coro_transform` already drops them (`of KillV, UnknownV:`), so
  no `kill` reaches generated code today; the general strip belongs in
  `derefs.nim` at step 4, when the lowered buffer is what gets published.

- **A statement-position `(stmts …)` became a scope.** The previous entry
  over-applied: `trStmt` sent *every* nested `stmts` through `trScopedBody`,
  but in Nimony IR a `stmts` in statement position is transparent — its locals
  belong to the enclosing scope. `{.keepOverflowFlag.}: let x = …` arrives as
  `(pragmax … (stmts (let x …)))` and `x` is used after it, as is every
  declaration `xelim` hoists into such a list. The lowering emitted
  `(kill x)` *before* those uses. The prover forgave it (a `kill` only
  forgets); hexer did not, and it was the first thing step 3 hit — the
  duplifier could not find `newSize` in `syncio`. Only a branch or loop body
  opens a scope now, because only there does the lowering itself remove the
  construct that delimited it.

- **`block` and its source name dropped** stays closed as intentional: a
  `block` has no Final IR construct of its own, `body` plus `(lab blockExit)`
  is the lowering. It is not textually reconstructible, so rendering one back
  for diagnostics is a `was` requirement, recorded there.

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
