# The `__atomic_*` builtin layer (`AtomMemModel`, `ATOMIC_*`, `atomic*Fetch`,
# `atomicLoadN`, ...) lives in `system/atomintrin`, included earlier so both
# this module and the allocator share one set of declarations.

func arcInc*(memLoc: var int) {.inline.} =
  ## Atomically increments the reference count.
  {.cast(noSideEffect).}:
    discard atomicAddFetch(memLoc.addr, 1, ATOMIC_SEQ_CST)

func arcDec*(memLoc: var int): bool {.inline.} =
  ## Atomically decrements the reference count. Returns true when it reaches zero.
  {.cast(noSideEffect).}:
    result = atomicSubFetch(memLoc.addr, 1, ATOMIC_SEQ_CST) < 0

func arcIsUnique*(memLoc: var int): bool {.inline.} =
  ## Atomically loads the reference count and returns true if it equals 0 (no extra references).
  {.cast(noSideEffect).}:
    result = atomicLoadN(memLoc.addr, ATOMIC_ACQUIRE) == 0
