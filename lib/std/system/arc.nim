# The `arc` memory management strategy: reference counting with plain, unsynchronized
# counter updates. Selected by `--mm:arc` via `include "$MM"` in `system.nim`.
#
# This is `atomicArc` minus the atomics, and that is the whole difference: the
# counters are cheaper, and a `ref` must not be shared between threads, because two
# threads updating one counter at once will lose an update and free the object early
# (or leak it). Use `--mm:atomicArc` (the default) for anything multi-threaded.
#
# A strategy module supplies exactly these three primitives; everything built on
# top of them (`GC_ref`, `GC_unref`) lives in `system/refops` and is shared.

func arcInc*(memLoc: var int) {.inline.} =
  ## Increments the reference count.
  {.cast(noSideEffect).}:
    inc memLoc

func arcDec*(memLoc: var int): bool {.inline.} =
  ## Decrements the reference count. Returns true when it reaches zero.
  ## Mirrors `atomicSubFetch`: the comparison is against the value AFTER the
  ## decrement, and a fresh object starts at 0, so "reached zero" is `< 0`.
  {.cast(noSideEffect).}:
    dec memLoc
    result = memLoc < 0

func arcIsUnique*(memLoc: var int): bool {.inline.} =
  ## Returns true if the reference count is 0 (no extra references).
  {.cast(noSideEffect).}:
    result = memLoc == 0
