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

proc GC_ref*[T](x: ref T) {.nodestroy, inline.} =
  ## Manually increments the reference count of `x`. Pairs with `GC_unref`.
  ## For raw `alloc`'d memory that holds `ref` fields ARC cannot trace, this
  ## keeps the referenced object alive past the lifetime of the `ref` variable.
  ## `nodestroy` is essential: it suppresses the destructor the compiler would
  ## otherwise inject on the `=dup` result, so the extra reference deliberately
  ## leaks (no-op when `x` is nil — the `=dup` hook guards it).
  discard `=dup`(x)

proc GC_unref*[T](x: ref T) {.inline.} =
  ## Manually decrements the reference count of `x`, freeing it at zero.
  ## Inverse of `GC_ref`. `x` is a borrowed (non-`sink`) parameter, so the
  ## only effect is the `=destroy` hook's `arcDec` (nil-safe).
  `=destroy`(x)
