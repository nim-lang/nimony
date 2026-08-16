#
#
#        Inter-Module Inliner bridge (global-pool world → optdriver)
#        (c) Copyright 2026 Andreas Rumpf
#
#    See the file "license.txt", included in this
#    distribution, for details about the copyright.
#

## Lets the `optdriver` reuse the existing inter-module inliner
## (`intermodinliner` → hexer's 910-line `intramodinliner`) without forking it.
##
## Both sides speak **nifcore** — `nifpools` re-exports it, so a `TokenBuf` is a
## `TokenBuf` and the buffer crosses this boundary as itself. What does not
## cross is the *namespace*: the inliner is written against `nifpools`' one
## process-global literal pool (`pool.syms.getOrIncl`, argument-less
## `createTokenBuf`), while `optdriver` threads pools explicitly per module. So
## the buffer that comes back carries the global pool in `.pool`/`.tags`, and
## the caller builds its type context in that same namespace (`nifmodules.load`
## takes `sharedPool`/`sharedTags`) rather than minting a second one.
##
## The module stays a one-proc firewall for a plainer reason: `intermodinliner`
## `include`s `nifprelude`, so importing it directly would pull the whole
## `nifpools` surface — `parseFromFile`, `toString`, `createTokenBuf` — into
## `optdriver`, where every one of those names already means the nifcore
## version. One `import` here, one proc out, no ambiguity there.

import std / assertions
include "../../lib" / nifprelude
import nifpools
import intermodinliner   # runInterModuleInliner (nifpools)

proc runImi*(input, suffix, xnifDir: string; changed: var bool): TokenBuf =
  ## Parse the `.c.nif` at `input`, run inter-module inlining, and hand the
  ## module back as a buffer in the global pool/tag namespace — read
  ## `result.pool` / `result.tags` to load anything else into it. `changed`
  ## reports whether the inliner altered anything.
  result = parseFromFile(input, 4000)
  changed = runInterModuleInliner(result, suffix, xnifDir)
