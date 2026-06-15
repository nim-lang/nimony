#
#
#        Inter-Module Inliner bridge (nifcursors → nifcore)
#        (c) Copyright 2026 Andreas Rumpf
#
#    See the file "license.txt", included in this
#    distribution, for details about the copyright.
#

## Lets the nifcore `optdriver` reuse the existing inter-module inliner
## (`intermodinliner` → hexer's 910-line `intramodinliner`) without forking it
## to nifcore. This module lives entirely in the **nifcursors** world (built
## `-d:virtualParRi`, like `shoggoth.nim`); it exposes a single **string→string**
## entry point so the two NIF APIs never share a `Cursor`/`TokenBuf` type across
## the boundary — only serialized NIF text crosses.
##
## The cost is one parse→inline→serialize round-trip; `optdriver` then reparses
## the result into nifcore for the per-body passes.

import std / assertions
include "../../lib" / nifprelude
import nifstreams, nifcursors
import intermodinliner   # runInterModuleInliner (nifcursors)

proc runImi*(input, suffix, xnifDir: string; changed: var bool): string =
  ## Parse the `.c.nif` at `input`, run inter-module inlining, and return the
  ## resulting module as a **header-less** canonical NIF string (line info kept,
  ## symbols fully expanded) for nifcore to reparse. `changed` reports whether
  ## the inliner altered anything.
  var buf = parseFromFile(input, 4000)
  changed = runInterModuleInliner(buf, suffix, xnifDir)
  result = toString(buf)
