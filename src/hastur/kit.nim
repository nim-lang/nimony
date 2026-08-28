## The hastur test kit: what a `tests/<dir>/setup.nim` custom runner imports.
##
## A `setup.nim` OWNS its directory: hastur compiles and runs it, hands it the
## context on argv (`--dir`, `--bindir`, `--cachedir`, `--overwrite`,
## `--forward`) and takes its exit code as the directory's verdict. Everything
## such a runner needs — the toolchain resolution and run-wide flags
## (`toolchainDir`, `nimcacheDir`, `skipBuild`), the builders, the counters and
## the suite runners — is re-exported here, so a runner is one import.
##
## The CLI itself lives in `hastur.nim` and is deliberately NOT part of this
## surface: importing the kit must not drag in a command-line parser.

import context, counters, category, joined, compile, parallel, runner, walk,
       builders, deps, tiers, boot, native, llvmdebug, suites, incrementaltests
export context, counters, category, joined, compile, parallel, runner, walk,
       builders, deps, tiers, boot, native, llvmdebug, suites, incrementaltests
