# The `yrc` memory management strategy: thread-safe ORC, a cycle collector
# that runs concurrently with the mutators and with other collecting threads.
# Selected by `--mm:yrc` via `include "$MM"` in `system.nim`.
#
# The collector IS Nim's `lib/system/yrc.nim` (and its `cellseqs_v2.nim`),
# copied into `system/upstream/` by `hastur` at the commit
# `system/upstream.commit` pins; its header has the full story and
# the Lean/TLA+ proofs of the invariants live next to it in the Nim
# repository. It says `when defined(nimony)` where the two compilers differ.
# This file supplies what Nim's `arc.nim`, `seqs_v2.nim`, `threadids.nim` and
# `std/locks` would:
#
# * The compiler protocol is Nimony's (see `system/nimrtshim`). A decrement
#   of a class that cannot form a cycle (`op == nil`) is prompt, as with `arc`.
# * No OS locks or condition variables: `system` cannot import them. The
#   critical sections are a handful of instructions, so `Lock` is a spinlock
#   and waiting on a `Cond` spins.
# * The seq fence is `nimSeqFenceEnter`/`nimSeqFenceExit`, which `seq` takes
#   around structural changes of buffers the collector traces.
# * The ref-field write barrier is the ordinary lifted `=copy`/`=sink` (direct
#   atomic increment of the new value, deferred decrement of the old one).
#   Nim's `nimAsgnYrc` additionally exchanges the slot atomically, which only
#   matters for two threads storing into the same field without
#   synchronization.

{.feature: "lenientnils".}

include "nimrtshim"

# ---------------- atomics under Nim's names ----------------

template atomicFetchAdd(p: ptr int; val: int; mem: AtomMemModel): int =
  atomicAddFetch(p, val, mem) -% val
template atomicFetchSub(p: ptr int; val: int; mem: AtomMemModel): int =
  atomicSubFetch(p, val, mem) +% val
proc atomicInc(memLoc: var int; x: int = 1): int {.inline.} =
  atomicAddFetch(addr memLoc, x, ATOMIC_SEQ_CST)
proc atomicDec(memLoc: var int; x: int = 1): int {.inline.} =
  atomicSubFetch(addr memLoc, x, ATOMIC_SEQ_CST)

# The native back end has no atomics on proc values: a descriptor's bits go
# through `pointer`.
proc atomicLoadN(p: ptr PNimTypeV2; mem: AtomMemModel): PNimTypeV2 {.inline.} =
  result = nil
  cast[ptr pointer](addr result)[] = atomicLoadN(cast[ptr pointer](p), mem)
proc atomicStoreN(p: ptr PNimTypeV2; val: PNimTypeV2; mem: AtomMemModel) {.inline.} =
  var v = val
  atomicStoreN(cast[ptr pointer](p), cast[ptr pointer](addr v)[], mem)

proc increment(c: Cell) {.inline.} =
  discard atomicAddFetch(addr c.rc, rcIncrement, ATOMIC_ACQ_REL)

# ---------------- the three primitives every strategy supplies ----------------
# Only refs that CANNOT form a cycle use them for decrements.

func arcInc*(memLoc: var int) {.inline.} =
  ## Atomically increments the reference count.
  {.cast(noSideEffect).}:
    discard atomicAddFetch(addr memLoc, rcIncrement, ATOMIC_ACQ_REL)

func arcDec*(memLoc: var int): bool {.inline.} =
  ## Atomically decrements the reference count. Returns true when it was the
  ## last reference.
  {.cast(noSideEffect).}:
    result = atomicSubFetch(addr memLoc, rcIncrement, ATOMIC_ACQ_REL) < 0

func arcIsUnique*(memLoc: var int): bool {.inline.} =
  ## Returns true if there are no extra references.
  {.cast(noSideEffect).}:
    result = (atomicLoadN(addr memLoc, ATOMIC_ACQUIRE) and not rcMask) == 0

# ---------------- threadids ----------------

var
  gThreadIdCounter: int
  myThreadId {.threadvar.}: int # id + 1; 0 = not yet assigned

proc getThreadId(): int {.inline.} =
  ## Dense ids in creation order: all Nim's collector needs is a stable
  ## per-thread number to hash to a stripe.
  if myThreadId == 0:
    myThreadId = atomicAddFetch(addr gThreadIdCounter, 1, ATOMIC_RELAXED)
  result = myThreadId - 1

# ---------------- std/locks, spinning ----------------

type
  Lock = object
    v: int
  Cond = object
    seq: int ## bumped by every broadcast

proc initLock(L: var Lock) {.inline.} = L.v = 0
proc initCond(c: var Cond) {.inline.} = c.seq = 0

proc acquire(L: var Lock) {.inline.} =
  while atomicExchangeN(addr L.v, 1, ATOMIC_ACQUIRE) != 0:
    while atomicLoadN(addr L.v, ATOMIC_RELAXED) != 0:
      discard

proc release(L: var Lock) {.inline.} =
  atomicStoreN(addr L.v, 0, ATOMIC_RELEASE)

template withLock(L: Lock; body: untyped) =
  acquire L
  try:
    body
  finally:
    release L

proc wait(c: var Cond; L: var Lock) =
  ## Called with `L` held, like `pthread_cond_wait`: a `broadcast` (made under
  ## `L`) after we read `seq` is never missed.
  let s = atomicLoadN(addr c.seq, ATOMIC_ACQUIRE)
  release L
  while atomicLoadN(addr c.seq, ATOMIC_ACQUIRE) == s:
    discard
  acquire L

proc broadcast(c: var Cond) {.inline.} =
  discard atomicAddFetch(addr c.seq, 1, ATOMIC_RELEASE)

# ---------------- the seq fence (Nim's `seqs_v2`) ----------------
# Seq structure mutations and collections exclude each other; seq ops run
# concurrently with seq ops and collections with collections.

const
  NumLockStripes = 64

type
  YrcLockState = enum
    HasNoLock
    HasMutatorLock
    HasCollectorLock
    Collecting

  AlignedCounter = object
    ## one counter per cache line: no false sharing between stripes
    c: int
    pad: array[7, int]

var
  gSeqActive: array[NumLockStripes, AlignedCounter] # open seq fences per stripe
  gGcActive: int                                    # running collections
  lockState {.threadvar.}: YrcLockState
  fenceDepth {.threadvar.}: int

proc getYrcStripe(): int {.inline, ensures: (0 <= result and result < NumLockStripes).} =
  getThreadId() and (NumLockStripes - 1)

proc nimSeqFenceEnter*() =
  ## Taken by `seq` around changes of a buffer the collector traces.
  if lockState == HasNoLock:
    let s = getYrcStripe()
    while true:
      # SEQ_CST inc-then-check pairs with the collector's SEQ_CST
      # inc-then-drain (Dekker-style store/load ordering)
      discard atomicAddFetch(addr gSeqActive[s].c, 1, ATOMIC_SEQ_CST)
      if atomicLoadN(addr gGcActive, ATOMIC_SEQ_CST) == 0: break
      discard atomicSubFetch(addr gSeqActive[s].c, 1, ATOMIC_SEQ_CST)
      while atomicLoadN(addr gGcActive, ATOMIC_ACQUIRE) != 0:
        discard
    lockState = HasMutatorLock
  if lockState == HasMutatorLock:
    inc fenceDepth

proc nimSeqFenceExit*() =
  if lockState == HasMutatorLock:
    dec fenceDepth
    if fenceDepth == 0:
      lockState = HasNoLock
      discard atomicSubFetch(addr gSeqActive[getYrcStripe()].c, 1, ATOMIC_SEQ_CST)

proc yrcGcFenceEnter() =
  ## A collection announces itself and waits for open seq fences to drain.
  discard atomicAddFetch(addr gGcActive, 1, ATOMIC_SEQ_CST)
  for s in 0 ..< NumLockStripes:
    while atomicLoadN(addr gSeqActive[s].c, ATOMIC_SEQ_CST) > 0:
      discard

proc yrcGcFenceExit() =
  discard atomicSubFetch(addr gGcActive, 1, ATOMIC_SEQ_CST)

include "upstream/yrc"

# ---------------- the compiler's and the threads' entry points ----------------

func nimDecRefCyclic*(p: pointer; op: CellOp): bool {.inline.} =
  ## The decrement of a cell whose type can form a cycle: deferred, the cell
  ## is only ever freed by a collection, so this returns false. `op == nil` is
  ## a class that cannot form a cycle: freed promptly, as with `arc`.
  {.cast(noSideEffect).}:
    if op == nil:
      result = nimDecRefIsLastDyn(p)
    else:
      result = nimDecRefIsLastCyclicStatic(p, op)

proc nimThreadTeardown*() =
  ## Called when a thread exits (`std/rawthreads`).
  nimYrcThreadTeardown()
