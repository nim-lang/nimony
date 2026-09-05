# Development Guidelines

This repo is a Nim compiler reimplementation built around [NIF](https://github.com/nim-lang/nifspec). The compilation
pipeline is split into tools and phases:

- Nifler: parses Nim to NIF.
- Nimony: semantic checking and front-end lowering.
- Hexer: lowering passes and Leng generation.
- Lengc: C/C++ backend based on NIF.

When debugging compiler behavior, assume `nifler`, `nifmake`, and `lengc` are
stable. Most problems tend to be in `Nimony` or `Hexer`.

## Quick Debug Workflow

1. Build the Nimony toolchain (Nimony + Hexer):
   - `nim c -r src/hastur/hastur build nimony`
2. Produce `nimcache/` artifacts:
   - `bin/nimony c mybug.nim`
   - Or use the convenience command:
     - `nim c -r src/hastur/hastur debug mybug.nim`
3. Inspect `nimcache/` for `.nif` artifacts (for example `.s.nif` and
   other lowered NIF files). These show the transformations across phases.

## Where to Look

- `src/nimony/` for semantic analysis and front-end phases.
- `src/hexer/` for lowering passes and Leng generation steps.
- `src/nifler/` and `src/lengc/` only when evidence points there.
- `src/hastur/` for test/build tooling and command behavior (`hastur.nim` is the
  CLI; the logic lives in its sibling modules).

## Debugging Tips

- Reproduce with the smallest input file possible and use `nimcache/` diffs.
- Use `hastur test <file>` or `hastur test <dir>` to validate a regression.
- Many tests live in `tests/nimony/` and are a good source of minimal cases.
- Use `hastur bug` and `hastur rep` for quick turnaround times during development.

## Style guidelines

See `src/lengc/shoggoth/vectorizer.nim` for good style:

- Prefer explicit state objects (`Matcher`, `Emitter`) over many parallel
  local maps/sets. Thread one mutable object through helpers instead of
  widening function signatures.
- Avoid closures. Use flat procs.
- Keep matching strict and grammar-driven: if a loop construct is not fully
  understood, reject it instead of partially vectorizing it.
- Track local symbol roles explicitly (pointer pending/bound, index, value)
  rather than inferring roles repeatedly from ad-hoc sets.
- Preserve loop-invariance checks and disjointness guards as first-class logic;
  correctness checks must remain obvious in code structure.
- Keep helper procs single-purpose (`matchGuardCmp`, `matchPtrBind`,
  `collectBroadcasts`, `emitSlot`) so refactors stay behavior-preserving and
  reviewable.
- Prefer structured control over early-return mazes; but do not take it too extremes, use good judgement.
- Temporary-name generation must follow the NIF standard which has clear rules. Do not make up your own rules. **DO NOT GET CONFUSED BY TRAILING DOTS**, these are completed to the current module suffix by the NIF API.


## Tests results overwrite

Tests often include large amounts of produced NIF code. Use `hastur --overwrite` to overwrite all test results or `hastur --overwrite test ...` to overwrite a specific test case. The resulting diffs are always part of the code review process.

## Benchmarks and the stdlib coverage check

`hastur all` sweeps `bench/` alongside `tests/` and `examples/`, so a benchmark
that stops compiling is caught by the suite rather than by whoever next reaches
for it. `bench/hastur.mode` puts the directory in the `bench` category, which
compiles with `-d:benchSmoke`: each benchmark shrinks its workload to something
that costs milliseconds and prints only deterministic values (checksums,
digests), which is what its `.output` golden holds. The suite's question there
is "does it still build and still compute the same numbers" — never "how fast".
Add a benchmark the same way: a `when smoke:` shape for the workload and the
reporting, then `hastur --overwrite bench` or a hand-written `.output`.

`tests/nimony/stdlib/tall.nim` imports every module in `lib/std` and is checked
against that directory on every run that reaches it (`src/hastur/coverage.nim`).
A new stdlib module must be added there: `tall.nim` is the only test that
compiles the stdlib modules together, and `dagon` walks it as the aggregator
driver for the website's documentation, so a module missing from it is a module
missing from the docs. Subdirectories count too (`std/http/httpmsg`) — the
check carries a deny list of the ones that hold no importable module
(`system/`, `private/`, `posix/`, …), so a new *public* subdirectory fails the
check rather than being silently skipped, and a new internals one needs a line
on that list with its reason.

## Joined tests

A test directory's plain tests (no `.msgs`, no golden `.nim.c`/`.nif`, exit code
0, no valgrind run, no `isMainModule`) are compiled into ONE program: a
generated `_hastur_joined.nim` imports them and their module-init code runs in
import order, so the program prints the members' `.output` files back to back.
That is what keeps the suite fast where process creation is expensive
(Windows), because the cost of a test is the toolchain process tree behind it,
not the test itself.

Consequences when writing a test:

- Give a test that prints an `.output` file. `hastur --overwrite` writes one
  for you. Without it the test is expected to print nothing, and its group
  fails.
- A test that must be a program of its own — unstable output, a subprocess
  writing to the shared stdout, a race with a neighbour over a build artifact —
  opts out with an empty `<test>.nojoin` sidecar next to it.
- Nothing is lost on failure: a group that diverges is automatically re-run
  test by test, so the report still names the single test that broke. Use
  `--joined:off` to rule the grouping out entirely, or `hastur joined <dir>` to
  run one group by hand.
