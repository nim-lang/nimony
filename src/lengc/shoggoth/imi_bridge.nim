#
#
#        Inter-Module Inliner bridge (nifcursors → nifcore)
#        (c) Copyright 2026 Andreas Rumpf
#
#    See the file "license.txt", included in this
#    distribution, for details about the copyright.
#

## Lets the nifcore `optdriver` run hexer's inliner at its `shoggothStage`,
## across modules. The inliner is written
## against the `nifprelude`/`nifpools` surface — nifcore plus the process-global
## `pool`/`globalTags` — which `optdriver` does not import, so this module is
## the one place the two surfaces meet. Both are nifcore underneath: the
## `TokenBuf` itself crosses, interned in nifpools' global pools, and
## `optdriver` builds its type context on those same pools.

include "../../lib" / nifprelude
import ".." / ".." / hexer / inliner

proc parseModule*(input: string): TokenBuf =
  ## Parse the `.c.nif` at `input` into nifpools' global pools, with dense line
  ## info (what the inliner reads; `optdriver` hands its passes a sparse copy).
  result = parseFromFile(input, 4000)

proc runImi*(input, suffix, xnifDir: string; changed: var bool): TokenBuf =
  ## Parse the `.c.nif` at `input` and run inter-module inlining on it.
  ## `changed` reports whether the inliner altered anything.
  result = parseModule(input)
  changed = inlineModule(result, suffix, shoggothStage, xnifDir)

proc runImiOn*(buf: var TokenBuf; suffix, xnifDir: string): bool =
  ## Inter-module inlining on a module already parsed with `parseModule` (and
  ## possibly rewritten since); true when the inliner altered anything.
  inlineModule(buf, suffix, shoggothStage, xnifDir)
