# A memory management strategy that does NOT live under `lib/std/system/`: the
# test directory selects it with `--mm:tests/nimony/mmcustom/rt/counting`, so
# `include "$MM"` in system.nim resolves to this file. It is `arc` plus a
# counter, which is what the test uses to prove this file -- and not a strategy
# that ships with the stdlib -- is the one that got included.
#
# A strategy module supplies exactly these three primitives; everything built on
# top of them (`GC_ref`, `GC_unref`) lives in `system/refops` and is shared.

const customRuntimeMarker* = 42
  ## Only this file defines it, and `system.nim` includes the file, so a test
  ## that names it does not compile unless the custom runtime was selected.

var customArcIncs*, customArcDecs*: int

func arcInc*(memLoc: var int) {.inline.} =
  ## Increments the reference count.
  {.cast(noSideEffect).}:
    inc memLoc
    inc customArcIncs

func arcDec*(memLoc: var int): bool {.inline.} =
  ## Decrements the reference count. Returns true when it reaches zero.
  ## Mirrors `atomicSubFetch`: the comparison is against the value AFTER the
  ## decrement, and a fresh object starts at 0, so "reached zero" is `< 0`.
  {.cast(noSideEffect).}:
    dec memLoc
    inc customArcDecs
    result = memLoc < 0

func arcIsUnique*(memLoc: var int): bool {.inline.} =
  ## Returns true if the reference count is 0 (no extra references).
  {.cast(noSideEffect).}:
    result = memLoc == 0
