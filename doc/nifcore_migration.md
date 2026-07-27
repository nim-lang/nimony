# The nifcore migration: regression causes, findings, gotchas

A field report from porting nimsem + hexer (and lengc before them) from the
classic NIF stack (`nifstreams`/`nifcursors`, physical ParRi tokens, per-token
line-info fields) to **nifcore** (4-byte tokens, bounded cursors, virtual
ParRi, sparse `LineInfoLit` suffixes, binary `.bif` transport). This is the
distilled "why did that break" knowledge: every defect class we hit, with its
symptom, root cause and the grep-able fix pattern. Companion docs:
`modern_nif.md` (the design/plan), `nifcore_shim.md` (the historical compat
shim, since dissolved).

The migration ran in phases: bounded-scope conversion of the whole nimsem
import graph, flipping `-d:virtualParRi`, killing the `enterScope`/
`leaveScope`/`CursorScope` API (`sub` + `into` + child-cursor handoff), the
real port/renamefest onto nifcore proper (compat shim dissolved), and finally
the `.bif` binary transport (lengc, then the plugin protocol end-to-end).
Full test suite, boot fixpoint (stages 2 == 3 byte-identical) and the tool
suites were held green at each phase boundary.


## 1. The model shift, in one page

What actually changed underneath the code:

| Classic | nifcore | Consequence |
|---|---|---|
| Physical `ParRi` token closes every tree | `addParRi` **seals** the open `ParLe` with a jump and elides the token (overflow scopes keep a physical one) | any code that *counts* or *reads* ParRi tokens is wrong |
| Cursor freely walks token array | Cursors are **bounded** (`rem` counts remaining values in scope); a virtual ParRi is reported at `rem == 0` | descend/drain/skipParRi walks become `into:`/`sub`; `.kind` at `rem == 0` asserts |
| `firstSon` = position + 1 | head + 1 may be a **line-info suffix** | "first child" must be `childCursor` (sealed) or `pos + tokenWidth(head)` |
| `litId`/`intId` fields on every token | short strings/syms stored **inline** in the token; ints inline with suffix extension | raw `strId`/`symId`/`intId` reads on arbitrary tokens return garbage |
| `token.info` is a field | line info is a *suffix token*; `dest[len-1]` may be that suffix | `.tag`/`.info` on "the last token added" reads suffix bits that alias TagIds |
| One global pool, always | pools are per-buffer (`b.pool`/`b.tags`), the frontend keeps a global one in `nifpools` | cross-pool copies must re-intern (`addSubtree` does); raw token copies across pools corrupt |

The endgame invariant that made the migration verifiable at every step:
**boot must reach a byte-identical fixpoint** (stage 2 == stage 3). Nearly
every real bug below was ultimately caught by boot, a golden diff, or the
validator — not by unit tests.


## 2. Defect classes in ported code (apply on sight)

These nine patterns caused essentially all port regressions in the renamefest
phase. Each was hit multiple times; treat them as review checklist items for
any code touching nifcore buffers.

1. **head + 1 is not the first child.** A head token may carry a line-info
   suffix. Use `childCursor` on sealed trees, `pos + tokenWidth(head)` on open
   ones. Symptom: reading the *name* of a suffix token — e.g.
   `tryForLoopPlugin` probed the callee at head+1 and every for-loop plugin
   silently no-op'd; `reorderInnerGenericInstances` matched the wrong token
   and never moved instances under their owner.
2. **`len - 1` is not the last value.** Track `lastValueStart` explicitly
   (sempragmas header pragmas broke threads tests this way).
3. **Raw `buf.add <TagLit>` never registers an open scope.** The tree is never
   sealed; downstream `beginRead` asserts unbalanced. Use
   `addParLe(tag, info)`.
4. **Classic sentinel ParRi on flat buffers → delete it.** Fragment buffers
   used a trailing physical ParRi as an end marker; nifcore consumers are
   `hasMore`-bounded and the stray close corrupts the seal bookkeeping
   (bindSym choices, explicit generic instantiation, `bindSubsInvokeArgs`).
5. **Bare `.kind` at `rem == 0` asserts.** Use the `isSymbol`/`isTagLit`/
   `isDotToken` predicates when a scope may be exhausted.
6. **Negative int28 payloads sign-extend.** Mask with `and PayloadMask` when
   reassembling (`soperand` is signed).
7. **`dest.insert buf, 1` splits a head from its suffix.** Insert at
   `tokenWidth(head)`.
8. **`addBufferSamePool` compares *effective* pools.** A raw-filled buffer
   keeps `pool == nil` and silently takes the cross-pool path (or vice
   versa).
9. **Never `.tag`/`.tagId` on `dest[dest.len-1]`.** Line-info suffix bits
   alias TagIds; combine `lastValueStart` with a `kind == TagLit` check.

Also in this family: **cursor teleports inside bounded scopes**. Reassigning a
cursor variable inside an `into:`/`copyInto` body (`n = ri`) breaks `rem`
bookkeeping even when positions match classic behavior; only reassign from a
same-scope copy (`n = start; skip n`).


## 3. ParRi-elision (`-d:virtualParRi`) hazards

The recurring traps when the physical closes disappeared. **The single most
recurring one:** raw-index walks that count `ParLe`/`ParRi` to track nesting
(`inc nested`/`dec nested` over `c[i].kind`). Under elision `nested` grows
monotonically. This produced three distinct disasters from one cause:

- `controlflow.codeListing`: nifbuilder "unpaired" assert.
- `mover.buildFindStartIndex`: `int16` overflow **wrapped negative**, made
  `isLastRead` spuriously true → a *silent move-semantics miscompile* on any
  large module (small test buffers stayed under 32768 and were merely wrong).
- `semmain.pruneMatchedForwardDecls`: nested-ParRi counting never rebalanced
  and dotted out **the entire rest of the module** after the first forward
  decl — the module "compiled" but was empty, and importers reported
  undeclared identifiers for everything declared after it.

The fix idiom is always the same, **closeStack**: on a sealed ParLe
(`jump != MaxJump`) push `i + span - 1`; after each index,
`while closeStack[^1] == i: close/dec; pop`. Overflow scopes keep their
physical ParRi and close via the normal branch. Grep for
`inc nested`-style counters over raw token indexing.

The rest of the elision failure classes, tersely:

- **`.info` reads after consuming a subtree** (`rem == 0`) assert → capture
  the info *before* the consuming call, or use `n.endInfo`. This class alone
  hit a dozen files (semuntyped's `semMixinStmt` was the boot-stage crash).
- **Unmatched `addParRi` + `shrink` rollback**: `addParRi` irreversibly seals
  the enclosing open scope; `shrink` cannot undo the seal and everything
  after is mis-sealed. Never emit a close you might roll back.
- **`insert` into an already-sealed enclosing scope** leaves the enclosing
  jump stale → `widenSealed`/`widenEnclosingSealed` after the insert
  (semTup/semBracket/semCurly late-type insert, semmain import-list insert,
  semcall generic-args replace, lifter `{.error.}` pragma insert).
- **In-place retag** `dest[i] = parLeToken(...)` resets a sealed jump → use
  `setTag` (safe on open *and* sealed). lifter's MethodS retag produced
  empty `(method)` decls.
- **Token-index copy loops** `for i in a..<b: buf.add dest[i]` re-open tags
  whose closes were elided → later `addParRi` seals the wrong ParLe. Use
  `addRaw` for verbatim balanced spans.
- **Loose `inc`-descent over params/pragmas with a `kind == ParRi` sentinel**
  spins forever at a virtual ParRi — this is the signature of the
  100 %-CPU-for-40-minutes hang (destroyer.registerSinkParameters and ~10
  friends). Bound the walk.
- **"Close the root, assert, shrink, append, re-close"** is impossible under
  elision — leave the root `(stmts` open and let the caller append + close
  (destroyer/desugar pipelines), or use `reopenLastTree`.
- **Position-shift accidents were load-bearing**: classic's 2-token export
  marker (`(magic)` pair) accidentally disabled `hookThatShouldBeMethod` for
  magic routines; the elided 1-token form re-enabled it. When behavior
  changes after a mechanical port, check whether classic behavior was an
  accident the code depended on.
- **Goldens encode buffer indices.** Elision shifts every `L<idx>` label in
  controlflow goldens and line-info drift shifts nosystem goldens. Verify a
  regeneration is benign with a normalized diff
  (`sed -E 's/L[0-9]+/L#/g'`) before accepting it.
- `checkSeals` reports **false positives on incomplete buffers** (a ParLe
  copied from elsewhere legitimately carries a stale jump while still open
  here). Only trust it on completed buffers.


## 4. Genuine compiler bugs the migration exposed

The most valuable finds — pre-existing nimony bugs that the new code patterns
flushed out. These were *not* porting mistakes.

### 4.1 `x = f(x)` self-assign miscompile (mover, fixed)

The conversion pattern `n = sub(n)` (call argument and destination are the
same variable) miscompiled under nimony whenever a **live alias** of `n`
existed. Boot stage 2 asserted with no location; `tests/nimony` was 602/606
green throughout. Root cause in `mover.isLastReadImpl` (AsgnS): on `x = f(x)`
the scan saw the LHS redefine the root and `break`ed *without checking the
RHS* — but the RHS reads the old value, so the earlier `let alias = x` is not
a last use. Generated C showed `alias = n; =wasMoved(&n); sub(n)` — `n`
zeroed before the call. Minimal repro needs **all four**: an RC value type
with `=copy`+`=dup`+`=destroy`; a callee doing `result = c` then mutating;
the self-assign `x = f(x)`; a live alias at that point
(`tests/nimony/lastuse/tselfassign.nim` is the regression test).

Diagnosis method worth keeping: the **3-way host/codegen matrix** on identical
sources — nim-hosted nimsem runs the stage green (⇒ source logic correct),
nimony-hosted asserts (⇒ codegen), per-module reverts bisect the trigger
files. Note `bin/nimony c` exiting 0 proves nothing; you must *run* the
produced binary in a clean stage.

### 4.2 Missing `=dup` synthesis (open)

nimony does not synthesize `=dup` from a user `=copy`: copy-init of such a
type does a raw blit and drops the refcount. Latent only because `Cursor`
declares an explicit `=dup`.

### 4.3 StringView interning miscompile (boot, fixed on its branch)

Self-hosted nimony miscompiled `getOrInclFromView`/`hash(StringView)` so the
NIF parser interned `(stmts` as the wrong TagId. Repro was as small as
`const x = @[1]`. Moral: when "the parser reads garbage", suspect the
*compiler that compiled the parser* before the parser.

### 4.4 Nimony-defaults surprises (by design, but they bite)

- **Templates resolve body identifiers eagerly** — a template referencing a
  `let` declared later in the same proc fails under self-host while host Nim
  accepts it. Declare the template after its captures (derefs
  `finishCallArgs`).
- **Iterators (and `func`/`converter`) default to `.noSideEffect`**
  (derefs.nim `trProcDecl`). The first nimony compile of `bif.nim` failed on
  an iterator calling `cursorAt` (a cursor-owner refcount write). Mark such
  iterators `{.sideEffect.}` — the stdlib idiom (`envPairs`, `getopt`).
- `@[]` fails inference in some generic positions → `newSeq[int]()`.
- Nimony does not zero-init `result` — a custom `=wasMoved` must be
  `{.nodestroy.}`.
- The nimony-compiled modules (`nifcore`, `bif`, `plugins`, everything the
  plugin exes pull in) are the canary for dialect regressions: they compile
  under both host Nim and nimony on every plugin build.


## 5. Latent classic bugs fixed in passing

The mechanical conversions found real, silent bugs in the *old* code:

- renderer `gcallComma`: the named-arg close was never consumed — arguments
  after a named argument were dropped from rendered output.
- derefs `trFor` inspected only the first loop variable; `trReturn`'s
  borrow-error path missed a `skip`.
- `tcopy_error_*`/`tmove_only2` were silently *passing compilation* because a
  lifter insert corrupted the `{.error.}` hook diagnostic they were supposed
  to trigger.
- The validator's known-call-name lists contained stale names after a rename
  sed hit string literals — see §7.


## 6. The Windows-only defect class: leaked mmaps lock files

`nifreader.open` mmaps the file via `vfs.VfsBlob`, which is **explicit-close
by design** (no destructor — same lifecycle as the MemFile it replaced). The
port dropped several `close s` calls when converting `nifstreams` reads to
`nifreader.open` + `parse`. On POSIX a leaked mapping is invisible; on
Windows it **locks the file**, and the failure appears far away: repeated
const-eval folds of the same expression hash to the same `tco<hash>.p.nif`,
the first fold's `createIndex` checksum-read leaks the mapping, every later
rewrite of that file fails, surfacing as "I/O error while evaluating …" from
a catch-all `except`. The tell in the CI log: the error fired on every fold
*except the first*.

**Audit rule:** every `nifreader.open` needs a matching `nifreader.close`
unless the reader is deliberately resident for lazy per-symbol jumps
(`programs.nim` module readers, `foreignmodules`). Beware overload capture:
a bare `r.close()` can resolve to `nifbuilder.close(Builder)` — qualify it.


## 7. Validator and rename-tooling interactions

- The validator's effect graph derives per-proc "what it adds to dest"
  contracts. The port turned unanalyzable single-token adds
  (`dest.add callNode`) into analyzable `addParLe` — and the graph counted
  each `addParLe` as one child with **no nesting awareness**, producing false
  "expected 1..1 children, got 2" on `(else)`. Fix: depth-track
  `addParLe`/`addParRi` in `effect_graph.analyzeStmtsBody` — a balanced
  region is one child, a net-opening body is unknown (skip). General lesson:
  making code *more* analyzable can surface latent checker naivety as
  phantom violations.
- **Renaming seds hit string literals.** The effect graph and validator hold
  known-call-name lists as strings; a global `takeToken` → `takeTree` sed
  produced duplicate case labels there. After any renamefest, grep the
  validator/effect-graph name tables for the old and new names.
- Comment-only `else` branches lose their sole `discard` to cleanup passes →
  parse errors far from the edit.
- `nim check` **misses `when defined(...)` blocks** (sealCheck,
  `-d:validatePasses`-only code). Only a build with hastur's exact flags
  proves the tree compiles.


## 8. Determinism traps (boot fixpoint killers)

- **Hash-order emission**: `(kill …)` sets in nj/finalir were emitted in
  scope-table hash order, which depends on `SymId` interning order → goldens
  flapped and boot wasn't a fixpoint. Emit name-sorted (the insertion sort is
  dependency-free so nimony can self-host it).
- **Pool ids depend on compile history.** Anything content-addressed
  (checksummed bif bytes, cache filenames) must be built against a **private
  pool** re-interned in tree order; a global-pool buffer serializes
  differently run to run and drags the whole pool into the file.
- Golden regeneration is only acceptable when the diff normalizes away
  (label renumbering, benign line-info drift) — verify, don't assume.


## 9. bif (binary NIF) — the invariants that matter

- **Zero-patch invariant**: the raw token words embed pool ids that are only
  reproduced by interning into **empty, fresh pools** from id 1. `bif.load`
  therefore always mints fresh pools; loading into a shared/pre-populated
  pool is silent corruption. Cross-pool transfer goes through
  `nifcore.addSubtree`, which re-interns tags, literals *and* line-info
  filenames.
- **Pool identity is API-visible.** The plugin API dispatches every kind
  query on `pluginTags` *identity* (`n.tags != pluginTags → Invalid`), so a
  bif-loaded buffer must be copied into the plugin pools — the fresh-pool
  buffer would make every `stmtKind`/`exprKind` return `No*` with no error.
- **Directives have no bif channel.** The one directive the pipeline needs —
  the plugin protocol's `(.unusedname X)` gensym hint — travels as a leading
  `(unusedname X)` tree (`bif.UnusedNameTag`) that binary readers peel off;
  `niftools nif2bif` lifts the directive into the tree.
- **Format is sniffed, never named**: files keep their `.nif` names;
  `isBifFile` compares only the 6 magic-name bytes so a wrong-*version* bif
  still reaches `load`'s precise error instead of the text parser.
- Absolute line infos ride in the tokens, so the bif read paths need none of
  the text side's `parentSeed`/`denseLineInfo` resolution — that machinery
  now exists only in the text fallbacks. When *converting* text plugin
  inputs, parse dense (`nif2bif --dense`) to be token-faithful.
- `bif.load` mmaps and the mapping is deliberately never unmapped
  (process-lifetime cache load). Do not "fix" that into the Windows defect
  class of §6 — it is resident by design, like the module readers.
- `storeToString` is the single source of truth for the byte layout
  (`storeToFile` dumps it); it exists because content-addressed callers need
  the bytes before the filename.


## 10. Process gotchas (tooling, testing, environment)

- **`hastur build all` exits 0 even when a tool fails** — grep the log for
  `FAILURE`. Piping build output through `tail` also swallows exit codes;
  use `set -o pipefail` and a log file.
- `hastur test <dir>` does **not** rebuild the toolchain; after source edits
  always `nim c -r src/hastur build all` (stale binaries have eaten multiple
  debug cycles, including one where a stale `bin/hexer` made native `echo`
  silently swallow output).
- **Two hasturs sharing a tree clobber each other** (shared `nimcache/`).
  Don't run tests while boot runs.
- `./src/hastur tests/nimony` does **not** cover the tool-test dirs
  (`tests/{nj,vl,contracts,validator,controlflow,boot,incremental,…}`); CI
  does. Two vpr bugs lived only there.
- **nimcache pollution makes standalone repros lie**: stale dependency
  `.s.nif` files produce phantom asserts in `nimsem m <mod>.p.nif` runs.
  Always reproduce via in-order nifmake or a full compile into a fresh cache.
- lastuse tests emit their report only on a **cold** backend cache; a warm
  `nimcache/` yields blank output and false failures.
- Plugin exes are gated by `needsRecompile(plugin source, exe)` only — a
  change to the *plugin API library* does not rebuild cached exes. Clean the
  test nimcache after touching `plugins.nim`.
- Expected environmental failures on this machine: `tdynlibs`(2) exit 134
  (dlopen of a missing libiconv), `tghast`(2) need unmerged
  araq-custom-backends wiring.


## 11. Debugging recipes that worked

- **Balance crashes**: print `unclosedTagPositions(buf)` before the asserting
  `beginRead`, plus a per-token kind/tag dump. Cracked every seal/balance
  bug in the renamefest.
- **Pass-level bisection**: hexer `-d:logPasses` dumps the tree after each
  pass; combine with a jump-consistency scanner (every sealed scope must end
  within its parent) to catch wide seals near their cause instead of three
  passes later.
- **Error-driven fix loop** for mechanical ports: delete the shim →
  `nim check --errorMax:400` → patch by error class → repeat. Two caveats:
  see §7 (string literals, `when` blocks).
- **Codegen-vs-logic split**: the 3-way matrix of §4.1. Cheap, decisive.
- The failure-pattern *shape* is evidence: "every fold except the first"
  pinpointed the Windows mmap leak from macOS without a Windows machine.
