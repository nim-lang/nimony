# The `atomicArc` memory management strategy: reference counting whose counter
# updates are atomic, so a `ref` may be shared between threads. Selected by
# `--mm:atomicArc`, the default, via `include "$MM"` in `system.nim`.
#
# A strategy module supplies exactly these three primitives; everything built on
# top of them (`GC_ref`, `GC_unref`) lives in `system/refops` and is shared.
#
# The `__atomic_*` builtin layer (`AtomMemModel`, `ATOMIC_*`, `atomic*Fetch`,
# `atomicLoadN`, ...) lives in `system/atomintrin`, included earlier so both
# this module and the allocator share one set of declarations.

func arcInc*(memLoc: var int) {.inline.} =
  ## Atomically increments the reference count.
  {.cast(noSideEffect).}:
    discard atomicAddFetch(memLoc.addr, 1, ATOMIC_SEQ_CST)

func arcDec*(memLoc: var int): bool {.inline.} =
  ## Atomically decrements the reference count. Returns true when it reaches zero.
  ##
  ## Contract: a caller must free the cell when this returns true, or must
  ## already know the count is non-zero. The fast path below answers "you are
  ## the last one" WITHOUT writing the decrement back -- there is no point
  ## storing into a cell that is about to be freed -- so a caller that
  ## discards a `true` leaves the count untouched rather than at -1.
  {.cast(noSideEffect).}:
    when defined(nimNoAtomicArcFastPath):
      result = atomicSubFetch(memLoc.addr, 1, ATOMIC_SEQ_CST) < 0
    else:
      # Uniquely-referenced fast path: skip the read-modify-write entirely.
      #
      # The count is biased -- 0 means "one reference" -- so a zero count says
      # the caller holds the only one. A counted reference can only be derived
      # from the location being destroyed (which happens-before this
      # destructor, unless the program races on that location) or from another
      # counted reference, whose contribution is already in the count and so
      # forces the RMW below. Observing zero therefore proves that no other
      # thread holds a reference to this cell and that none can appear: there
      # is nothing to adjudicate, and the caller is about to free the cell, so
      # the decrement need not be written back at all.
      #
      # This is sound only because atomicArc has no collector. A collector
      # mutates the count from a participant holding no counted reference,
      # which is exactly the case this argument rules out; a strategy that
      # gains one must not inherit this fast path.
      #
      # The load must be ACQUIRE: the count may have reached zero because
      # another thread's decrement got there first, and its writes to the
      # object have to be visible before we destroy it.
      #
      # The slow path stays self-testing -- it answers from the value its own
      # RMW returned, never from a separate load. Deciding "who frees" from a
      # load and discarding the RMW result is the nim-lang/threading#45 bug,
      # where every participant could drop the role at once.
      if atomicLoadN(memLoc.addr, ATOMIC_ACQUIRE) == 0:
        result = true
      else:
        result = atomicSubFetch(memLoc.addr, 1, ATOMIC_SEQ_CST) < 0

func arcIsUnique*(memLoc: var int): bool {.inline.} =
  ## Atomically loads the reference count and returns true if it equals 0 (no extra references).
  {.cast(noSideEffect).}:
    result = atomicLoadN(memLoc.addr, ATOMIC_ACQUIRE) == 0
