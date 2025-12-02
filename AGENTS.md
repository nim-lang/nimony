# Repository Guidelines

## Project Structure & Module Organization
- `src/` hosts compiler subsystems: `nimony/` (core pipeline), `nifc/` (IR + grammar), `hexer/` and `nifler/` (lexers), `hastur.nim`, and shared `lib/`. Keep related code together so incremental builds stay fast.
- `tests/` mirrors each subsystem with `.nif` fixtures plus `.expected.*` outputs; `tests/tester.nim` drives the diff-based checks.
- `doc/` contains specs (`design.md`, `language.md`, `nifc-spec.md`, `njvl-spec.md`), while `tools/` houses helper generators and `vendor/` holds pinned dependencies.

## Build, Test, and Development Commands
- `nim c -r src/hastur build all` compiles every tool (nimony, nifc, hexer, nifler, nifmake, nj, VL) into `bin/`.
- `nim c -r src/hastur all` runs the full regression matrix; replace `all` with `nimony`, `nifc`, or another command to scope the suite.
- `nim c -r src/hastur test tests/nimony/tracked/foo.nim` targets a single file or directory, forwarding extra flags to the compiler.
- `nim c -r tests/tester` drives the fixture comparators (nifindexes, nifgram, hastur self-tests) and accepts `--overwrite` to refresh expected outputs.

## Coding Style & Naming Conventions
- Stick to Nim’s two-space indentation, UTF-8 files, and `lower_snake_case` module names. Types stay `PascalCase`, procs `camelCase`, and exported symbols need a trailing `*`.

## Testing Guidelines
- Refresh fixtures with `nim c -r src/hastur --overwrite test <path>` only when you fully understand the delta, because reviewers rely on `.expected` diffs.
- Run `nim c -r src/hastur all` before pushing and call out skipped suites or flakes in the PR description.

## Commit & Pull Request Guidelines
- Follow the existing log style: `fixes #1582; adjust VL range typing (#1583)` — imperative summary plus issue references, PR number optional.
- PRs should state motivation, identify touched subsystems, and list the exact test commands run; only add screenshots when visual proof is essential.
- Keep commits focused and separate refactors from behavioral patches so bisecting remains straightforward.
