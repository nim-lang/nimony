# Development Guidelines

This repo is a Nim compiler reimplementation built around NIF. The compilation
pipeline is split into tools and phases:

- Nifler: parses Nim to NIF.
- Nimony: semantic checking and front-end lowering.
- Hexer: lowering passes and Leng generation.
- Lengc: C/C++ backend based on NIF.

When debugging compiler behavior, assume `nifler`, `nifmake`, and `lengc` are
stable. Most problems tend to be in `Nimony` or `Hexer`.

## Quick Debug Workflow

1. Build the Nimony toolchain (Nimony + Hexer):
   - `nim c -r src/hastur build nimony`
2. Produce `nimcache/` artifacts:
   - `bin/nimony c mybug.nim`
   - Or use the convenience command:
     - `nim c -r src/hastur debug mybug.nim`
3. Inspect `nimcache/` for `.nif` artifacts (for example `.s.nif` and
   other lowered NIF files). These show the transformations across phases.

## Where to Look

- `src/nimony/` for semantic analysis and front-end phases.
- `src/hexer/` for lowering passes and Leng generation steps.
- `src/nifler/` and `src/lengc/` only when evidence points there.
- `src/hastur.nim` for test/build tooling and command behavior.

## Debugging Tips

- Reproduce with the smallest input file possible and use `nimcache/` diffs.
- Use `hastur test <file>` or `hastur test <dir>` to validate a regression.
- Many tests live in `tests/nimony/` and are a good source of minimal cases.
- Use `hastur bug` and `hastur rep` for quick turnaround times during development.

## Tests results overwrite

Tests often include large amounts of produced NIF code. Use `hastur --overwrite` to overwrite all test results or `hastur --overwrite test ...` to overwrite a specific test case. The resulting diffs are always part of the code review process.

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
