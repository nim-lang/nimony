# The `orc` memory management strategy: the reference counting of `arc` plus a
# cycle collector. Selected by `--mm:orc` via `include "$MM"` in `system.nim`.
#
# The collector IS Nim's `lib/system/orc.nim` (and its `cellseqs_v2.nim`),
# copied into `system/upstream/` by `hastur` at the commit
# `system/upstream.commit` pins; it says `when defined(nimony)` where
# the two compilers differ. This file supplies what Nim's `arc.nim` would.
#
# Nim's collector is synchronous trial deletion after
# Bacon & Rajan, "Concurrent Cycle Collection in Reference Counted Systems"
# (https://www.cs.purdue.edu/homes/hosking/690M/Bacon01Concurrent.pdf, Fig. 2),
# with Lins' observation that only a decrement that does NOT free the cell can
# leave garbage behind.
#
# Like `arc`, the counter updates are not atomic: a `ref` must not be shared
# between threads, and every thread collects its own cycles.
#
# `arcInc`/`arcDec`/`arcIsUnique` stay the three primitives every strategy
# supplies; refs to types that cannot form a cycle use them exactly like `arc`.
# The compiler protocol is described in `system/nimrtshim`.

{.feature: "lenientnils".}

include "nimrtshim"

func arcInc*(memLoc: var int) {.inline.} =
  ## Increments the reference count.
  {.cast(noSideEffect).}:
    memLoc = memLoc +% rcIncrement

func arcDec*(memLoc: var int): bool {.inline.} =
  ## Decrements the reference count. Returns true when it was the last
  ## reference (the count is zero based, as in `arc`).
  {.cast(noSideEffect).}:
    if (memLoc and not rcMask) == 0:
      result = true
    else:
      memLoc = memLoc -% rcIncrement
      result = false

func arcIsUnique*(memLoc: var int): bool {.inline.} =
  ## Returns true if there are no extra references.
  {.cast(noSideEffect).}:
    result = (memLoc and not rcMask) == 0

include "upstream/orc"

func nimDecRefCyclic*(p: pointer; op: CellOp): bool {.inline.} =
  ## Called by the `=destroy` of a `ref T` the compiler hooks for the collector
  ## (`op` is nil when `T` cannot form a cycle). True when this was the last
  ## reference: the caller then runs `op(p, nil)`.
  {.cast(noSideEffect).}:
    result = nimDecRefIsLastCyclicStatic(p, op)
