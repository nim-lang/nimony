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
- Group imports (`import std / [syncio, parseopt]`), keep shared constants in `src/lib`, and compose features through the existing `gear2` helper modules instead of ad-hoc globals.
- `src/config.nims` enables `nimPreviewSlimSystem`, `--experimental:strictDefs`, and treats `Uninit`, `ProveInit`, and `StdPrefix` warnings as errors, so prefer explicit stdlib prefixes and exhaustive branches.

## Testing Guidelines
- Every behavioral change needs a regression under the matching `tests/` directory; use marker comments (`#[  ^errorId ]#`) so Hastur can assert diagnostic ranges.
- Refresh fixtures with `nim c -r src/hastur test <path> --overwrite` only when you fully understand the delta, because reviewers rely on `.expected` diffs.
- Run `nim c -r src/hastur all` before pushing and call out skipped suites or flakes in the PR description.

## Commit & Pull Request Guidelines
- Follow the existing log style: `fixes #1582; adjust VL range typing (#1583)` — imperative summary plus issue references, PR number optional.
- PRs should state motivation, identify touched subsystems, and list the exact test commands run; only add screenshots when visual proof is essential.
- Keep commits focused and separate refactors from behavioral patches so bisecting remains straightforward.
