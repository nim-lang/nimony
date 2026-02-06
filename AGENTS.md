# Development Guidelines

This repo is a Nim compiler reimplementation built around NIF. The compilation
pipeline is split into tools and phases:

- Nifler: parses Nim to NIF.
- Nimony: semantic checking and front-end lowering.
- Hexer: lowering passes and NIFC generation.
- NIFC: C/C++ backend based on NIF.

When debugging compiler behavior, assume `nifler`, `nifmake`, and `nifc` are
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
- `src/hexer/` for lowering passes and NIFC generation steps.
- `src/nifler/` and `src/nifc/` only when evidence points there.
- `src/hastur.nim` for test/build tooling and command behavior.

## Debugging Tips

- Reproduce with the smallest input file possible and use `nimcache/` diffs.
- Use `hastur test <file>` or `hastur test <dir>` to validate a regression.
- Many tests live in `tests/nimony/` and are a good source of minimal cases.
