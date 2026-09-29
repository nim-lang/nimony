# The `yrc` memory management strategy: thread-safe ORC, a cycle collector
# that runs concurrently with the mutators and with other collecting threads.
# Selected by `--mm:yrc` via `include "$MM"` in `system.nim`.
#
# A port of Nim's `lib/system/yrc.nim`; its header has the full story and the
# Lean/TLA+ proofs of the invariants live next to it in the Nim repository.
# In short:
#
# * Refs may be shared between threads. Counter updates are atomic.
# * A decrement of a cell whose type can form a cycle is DEFERRED: it goes into
#   a lock-free per-thread-stripe queue (`enqueueDec`). Draining a queue
#   applies the decrements and makes the cells candidate roots of the draining
#   thread. Such a cell is only ever freed by a collection -- plain garbage and
#   cyclic garbage alike -- so its destructor runs at collection time.
# * Refs that cannot form a cycle are freed promptly, exactly as with `arc`.
# * A collection captures the graph reachable from its candidates with one
#   Tarjan SCC traversal, decides deadness on the captured condensation
#   without touching the heap, validates against mutations that happened
#   during capture (deferred decs still queued, changed rc words) and only
#   then frees. Captures claim cells by CAS-ing a tag into the `rootIdx` word,
#   so several threads collect disjoint partitions at once. Commit stamps
#   proven-live cells with an epoch so long-lived structures are traced once
#   per epoch instead of once per collection (generational).
# * `seq` takes `nimSeqFenceEnter`/`nimSeqFenceExit` around structural
#   changes of buffers the collector traces; a collection waits for open
#   fences and a fence waits for running collections.
#
# Differences to Nim's version, all consequences of living inside `system`:
#
# * The compiler protocol is Nimony's (see `system/orc`): the type descriptor
#   is the cell operation `proc (cell, env: pointer)` (`CellOp`), and
#   `nimDecRefCyclic` is the decrement of a cell that can be part of a cycle.
# * No OS locks or condition variables: `system` cannot import them. The
#   critical sections are a handful of instructions, so they are spinlocks,
#   and waiting for a slot/grace period spins instead of parking.
# * The ref-field write barrier is the ordinary lifted `=copy`/`=sink` (direct
#   atomic increment of the new value, deferred decrement of the old one).
#   Nim additionally exchanges the slot atomically, which only matters for two
#   threads storing into the same field without synchronization.

{.feature: "lenientnils".}

const
  rcIncrement = 0b10000
  rcShift = 4
  inRootsFlag = 0b1000
  rcMask = 0b1111

  NumStripes = 64
  QueueSize = 256

  MaxPar = 256
    ## Capacity of the collection slot table: a ceiling on concurrent
    ## collections. `gParSlots` is how many of them are in play.
  YrcEpochLen = 64
    ## collections per epoch; bounds how long a stale "proven live" stamp
    ## defers rescans (see Nim's yrc.nim for why not less)
  YrcPromoteAge = 3
    ## captures a cell must survive before its stamp prunes; die-young data
    ## must never be deferred
  epochBase = 0x40000000'i64      # stamp namespace: tags stay below this
  epochMask = 0x3FFFFFFF'i64

  defaultThreshold = 128

# ---------------- the three primitives every strategy supplies ----------------
# Only refs that CANNOT form a cycle use them for decrements; the counter
# lives above the flag bits.

func arcInc*(memLoc: var int) {.inline.} =
  ## Atomically increments the reference count.
  {.cast(noSideEffect).}:
    discard atomicAddFetch(addr memLoc, rcIncrement, ATOMIC_ACQ_REL)

func arcDec*(memLoc: var int): bool {.inline.} =
  ## Atomically decrements the reference count. Returns true when it reached
  ## zero (a fresh cell starts at 0, so "zero" is below it).
  {.cast(noSideEffect).}:
    result = atomicSubFetch(addr memLoc, rcIncrement, ATOMIC_ACQ_REL) < 0

func arcIsUnique*(memLoc: var int): bool {.inline.} =
  ## Returns true if there is exactly one reference.
  {.cast(noSideEffect).}:
    result = (atomicLoadN(addr memLoc, ATOMIC_ACQUIRE) shr rcShift) == 0

# ---------------- cells and side structures ----------------

type
  YrcHeader = object
    ## Must match the cell layout `hexer/lengcgen.trRefBody` emits for a
    ## runtime that `.enableTrace`s: `(rc, rootIdx, payload)`.
    rc: int
      ## `(references - 1) shl rcShift`, or'ed with `inRootsFlag`
    rootIdx: int64
      ## the capture claim word: `(tag shl 32) or denseIndex` while a
      ## collection owns the cell, `(epochStamp shl 32) or age` once a
      ## collection proved it live, 0 for a fresh cell
  Cell = ptr YrcHeader

  CellOp* = proc (cell: pointer; env: pointer) {.nimcall.}
    ## The compiler's cell operation: traces the payload of `cell` into
    ## `env` (a `ptr GcEnv`), or with `env == nil` destroys the payload and
    ## frees the cell.

  CellEntry = object
    cell: Cell
    op: CellOp

  CellSeq = object
    len, cap: int
    d: ptr UncheckedArray[CellEntry]

  TraceEntry = object
    ## (slot, value) snapshot taken at trace time. The value is read exactly
    ## once: mutators may exchange the slot concurrently.
    slot: ptr pointer
    val: pointer
    op: CellOp

  TraceSeq = object
    len, cap: int
    d: ptr UncheckedArray[TraceEntry]

  RawSeq[T] = object
    ## growable array of plain scalars
    len, cap: int
    d: ptr UncheckedArray[T]

proc yrcOutOfMem() {.noinline.} =
  # The collector's own bookkeeping could not grow. A half-captured graph
  # cannot be committed, and `=destroy` hooks have nobody to report to.
  cAbort()

proc growBuf(d: pointer; cap, elemSize: int): pointer =
  result = realloc(d, cap * elemSize)
  if result == nil: yrcOutOfMem()

proc init(s: var CellSeq; cap = 1024) =
  s.len = 0
  s.cap = cap
  s.d = cast[ptr UncheckedArray[CellEntry]](growBuf(nil, cap, sizeof(CellEntry)))

proc deinit(s: var CellSeq) =
  if s.d != nil:
    dealloc(s.d)
    s.d = nil
  s.len = 0
  s.cap = 0

proc add(s: var CellSeq; c: Cell; op: CellOp) {.inline.} =
  if s.len >= s.cap:
    s.cap = max(s.cap div 2 + s.cap, 16)
    s.d = cast[ptr UncheckedArray[CellEntry]](growBuf(s.d, s.cap, sizeof(CellEntry)))
  s.d[s.len] = CellEntry(cell: c, op: op)
  inc s.len

proc init(s: var TraceSeq; cap = 1024) =
  s.len = 0
  s.cap = cap
  s.d = cast[ptr UncheckedArray[TraceEntry]](growBuf(nil, cap, sizeof(TraceEntry)))

proc deinit(s: var TraceSeq) =
  if s.d != nil:
    dealloc(s.d)
    s.d = nil
  s.len = 0
  s.cap = 0

proc add(s: var TraceSeq; e: TraceEntry) {.inline.} =
  if s.len >= s.cap:
    s.cap = max(s.cap div 2 + s.cap, 16)
    s.d = cast[ptr UncheckedArray[TraceEntry]](growBuf(s.d, s.cap, sizeof(TraceEntry)))
  s.d[s.len] = e
  inc s.len

proc pop(s: var TraceSeq): TraceEntry {.inline.} =
  dec s.len
  result = s.d[s.len]

proc resize[T](s: var RawSeq[T]; minCap: int) =
  s.cap = max(minCap, max(s.cap div 2 + s.cap, 16))
  s.d = cast[ptr UncheckedArray[T]](growBuf(s.d, s.cap, sizeof(T)))

proc add[T](s: var RawSeq[T]; v: T) {.inline.} =
  if s.len >= s.cap: resize(s, s.len + 1)
  s.d[s.len] = v
  inc s.len

proc addUnchecked[T](s: var RawSeq[T]; v: T) {.inline.} =
  ## `add` without the capacity branch; the caller guarantees the room.
  s.d[s.len] = v
  inc s.len

proc pop[T](s: var RawSeq[T]): T {.inline.} =
  dec s.len
  result = s.d[s.len]

proc init[T](s: var RawSeq[T]; cap = 256) =
  s.len = 0
  s.cap = cap
  s.d = cast[ptr UncheckedArray[T]](growBuf(nil, cap, sizeof(T)))

proc deinit[T](s: var RawSeq[T]) =
  if s.d != nil:
    dealloc(s.d)
    s.d = nil
  s.len = 0
  s.cap = 0

# ---------------- spinlocks and waiting ----------------

type
  SpinLock = object
    v: int

proc acquire(L: var SpinLock) {.inline.} =
  while atomicExchangeN(addr L.v, 1, ATOMIC_ACQUIRE) != 0:
    while atomicLoadN(addr L.v, ATOMIC_RELAXED) != 0:
      discard

proc release(L: var SpinLock) {.inline.} =
  atomicStoreN(addr L.v, 0, ATOMIC_RELEASE)

# ---------------- rc word ----------------

proc loadRc(c: Cell): int {.inline.} = atomicLoadN(addr c.rc, ATOMIC_ACQUIRE)

proc trialDec(c: Cell) {.inline.} =
  discard atomicSubFetch(addr c.rc, rcIncrement, ATOMIC_ACQ_REL)

proc rcClearFlag(c: Cell; flag: int) =
  var expected = atomicLoadN(addr c.rc, ATOMIC_RELAXED)
  while not atomicCompareExchangeN(addr c.rc, addr expected, expected and not flag,
                                   true, ATOMIC_ACQ_REL, ATOMIC_RELAXED):
    discard

proc rcTestSetFlag(c: Cell; flag: int): bool =
  ## Atomically set `flag`; true iff THIS call set it (it was clear).
  ## Candidate registration must win this race so that a cell sits in at most
  ## one candidate buffer.
  result = false
  var expected = atomicLoadN(addr c.rc, ATOMIC_RELAXED)
  while (expected and flag) == 0:
    if atomicCompareExchangeN(addr c.rc, addr expected, expected or flag,
                              true, ATOMIC_ACQ_REL, ATOMIC_RELAXED):
      result = true
      break

proc loadClaim(c: Cell): int64 {.inline.} = atomicLoadN(addr c.rootIdx, ATOMIC_ACQUIRE)

# Every packed word squeezes two 32-bit fields into one int64 as
# `(hi shl 32) or lo`; every value stored in `hi` is below 2^31.
proc loWord(w: int64): int64 {.inline.} = w and 0xFFFFFFFF'i64
proc hiWord(w: int64): int64 {.inline.} = w shr 32
proc epochStamp(e: int): int64 {.inline.} =
  (epochBase or (int64(e) and epochMask)) shl 32
proc stampAge(w: int64): int {.inline.} = int(loWord(w))
proc isEpochStamp(w: int64): bool {.inline.} = hiWord(w) >= epochBase

# ---------------- capture side structure ----------------

type
  TarjanFrame = object
    u: int32     # dense index of the cell this frame belongs to
    ebase: int32 # edges.len when this cell's frame was pushed
    base: int    # traceStack.len before this cell's trace ran

  CaptureRec = object
    ## per captured cell, position == Tarjan discovery index
    cell: Cell
    op: CellOp
    rcWord: int               # rc word as captured, without the flag bits
    lowlink: int32
    selfRefs: int32           # self edges, folded out of the edge array

  SccRec = object
    ## per SCC of the condensation; a sentinel record sits at [nScc]:
    ## memStart/crossOff are prefix offsets, an SCC's slice is [s]..<[s+1].
    sumRefs: int              # sum of member reference counts
    internal: int             # number of intra-SCC edges
    deadIn: int               # number of edges from dead SCCs
    memStart: int32           # offset into sccMembers
    crossOff: int32           # offset into crossTgt (cross edges by source)
    flags: uint8

  CaptureBufs = object
    ## per collector thread and persistent across collections, so frequent
    ## small collections don't pay per-collection allocations
    recs: RawSeq[CaptureRec]
    sccIdx: RawSeq[int32]     # per captured cell: its SCC, -1 while on the Tarjan stack
    tstack: RawSeq[int32]
    frames: RawSeq[TarjanFrame]
    edges: RawSeq[int32]      # pending out-edge targets of SCCs still being built
    sccs: RawSeq[SccRec]
    sccMembers: RawSeq[int32]
    crossTgt: RawSeq[int32]
    crossPend: CellSeq        # edge targets owned by other active collections
    prunedSrc: RawSeq[int32]  # dense indices of cells with pruned out-edges
    prunedTgt: CellSeq        # epoch-stamped targets we did not descend into
    ages: RawSeq[int32]       # per captured cell: captures survived so far
    slots: RawSeq[ptr pointer] # field addresses seen in capture

  GcEnv = object
    traceStack: TraceSeq
    toFree: CellSeq
    nScc: int
    nDeadScc: int
    nAborted: int
    freed, touched: int
    keepThreshold: bool

  CollCtx = object
    ## This thread's collector context, one threadvar for all of it.
    tag: int64
    slot: int
    epochStamp: int64   ## this collection's epoch, as a stamp word
    amSolo: bool
    genSuspects: CellSeq
      ## stamped cells that received a young→old commit dec this epoch without
      ## an RC death blow; flushed into the roots when the epoch advances.
      ## Holds `inRootsFlag` on every entry, which keeps them alive while listed.
    seenEpoch: int
    genFlushReady: bool

  LockState = enum
    HasNoLock, HasFence, Collecting

  DecEntry = object
    cell: Cell
    op: CellOp
    ready: int        ## published last (release), after `cell` and `op`;
                      ## an int, not the op itself: the native back end has
                      ## no atomics on proc values

  Stripe = object
    consumerLock: SpinLock
      ## consumers (drains) exclude each other; producers are lock-free
    toDecLen: int     # reservation counter; may exceed QueueSize under overflow
    toDec: array[QueueSize, DecEntry]

  AlignedCounter = object
    ## one counter per cache line: no false sharing between stripes
    c: int
    pad: array[7, int]

const
  flagDead = 1'u8
  flagForcedLive = 2'u8
  flagDirty = 4'u8   # a member's reference set changed during capture
  flagPruned = 8'u8  # an out-edge was pruned via an epoch stamp

var
  gCap {.threadvar.}: CaptureBufs
  gCtx {.threadvar.}: CollCtx
  lockState {.threadvar.}: LockState
  fenceDepth {.threadvar.}: int
  myStripe {.threadvar.}: int        # stripe index + 1; 0 = not yet assigned
  gLocalRoots {.threadvar.}: CellSeq
    ## this thread's candidate roots; only the owning thread touches it
  gPendingCells {.threadvar.}: CellSeq
    ## cells this thread committed dead but has not freed yet, because a
    ## capture in flight at commit time may still hold a stale snapshot
  gPendingWatch {.threadvar.}: RawSeq[int64]
    ## the captures that batch must outlive, packed as (slot shl 32) or tag
  gPendingSlot {.threadvar.}: int
  gPendingActive {.threadvar.}: bool
  gSpareRoots {.threadvar.}: CellSeq
  gTraceBuf {.threadvar.}: TraceSeq
  gFreeBuf {.threadvar.}: CellSeq

  gMergeLock: SpinLock                   # protects the tag slots + orphaned roots
  gActiveTags: array[MaxPar, int64]      # 0 = free slot
  gSlotPhase: array[MaxPar, int]         # 0 idle, 1 capturing, 2 committing
  gParSlots: int = 1
    ## how many slots are in play; only ever grows (under gMergeLock)
  gSoloCapture: int                      # a solo collection is in its capture phase
  gTagCounter: int64
  gEpoch: int                            # advanced every YrcEpochLen collections
  gCollectionCounter: int
  gStripeCounter: int

  roots: CellSeq
    ## ORPHANED candidates only: spilled by exiting threads, adopted by the
    ## next collection on any thread. Guarded by gMergeLock.
  stripes: array[NumStripes, Stripe]
  rootsThreshold: int = defaultThreshold   # shared adaptive heuristic; races are benign

  gSeqActive: array[NumStripes, AlignedCounter]  # open seq fences per stripe
  gGcActive: int                                 # running collections

# The static tables below are indexed by stripe/slot numbers that are in
# range by construction (masked, or below `gParSlots <= MaxPar`); these
# accessors say so instead of re-proving it at every use.
proc elemAt(base: pointer; i, size: int): pointer {.inline.} =
  cast[pointer](cast[uint](base) + uint(i) * uint(size))
proc stripe(i: int): ptr Stripe {.inline.} =
  cast[ptr Stripe](elemAt(addr stripes, i, sizeof(Stripe)))
proc decLen(i: int): ptr int {.inline.} =
  let st = stripe(i)
  result = addr st.toDecLen
proc decSlot(i, j: int): ptr DecEntry {.inline.} =
  let st = stripe(i)
  result = cast[ptr DecEntry](elemAt(addr st.toDec, j, sizeof(DecEntry)))
proc decReady(i, j: int): ptr int {.inline.} =
  let e = decSlot(i, j)
  result = addr e.ready
proc tagSlot(i: int): ptr int64 {.inline.} =
  cast[ptr int64](elemAt(addr gActiveTags, i, sizeof(int64)))
proc phaseSlot(i: int): ptr int {.inline.} =
  cast[ptr int](elemAt(addr gSlotPhase, i, sizeof(int)))
proc seqActiveSlot(i: int): ptr int {.inline.} =
  cast[ptr int](elemAt(addr gSeqActive, i, sizeof(AlignedCounter)))

proc getStripeIdx(): int {.inline.} =
  if myStripe == 0:
    myStripe = (atomicAddFetch(addr gStripeCounter, 1, ATOMIC_RELAXED) and
                (NumStripes - 1)) + 1
  result = myStripe - 1

proc slotsInPlay(): int {.inline.} =
  ## Must be loaded AFTER whatever claim word the caller is validating: a slot
  ## is put in play before the collection owning it can tag any cell.
  atomicLoadN(addr gParSlots, ATOMIC_ACQUIRE)

proc anySlotFree(): bool =
  result = false
  for sl in 0 ..< slotsInPlay():
    if atomicLoadN(tagSlot(sl), ATOMIC_ACQUIRE) == 0:
      return true

proc isStamped(c: Cell; ctx: ptr CollCtx): bool {.inline.} =
  ## claimed by THIS collection
  hiWord(atomicLoadN(addr c.rootIdx, ATOMIC_RELAXED)) == ctx.tag

proc denseIdx(c: Cell): int32 {.inline.} =
  int32(loWord(atomicLoadN(addr c.rootIdx, ATOMIC_RELAXED)))

proc isActiveTag(t: int64): bool =
  result = false
  if t != 0:
    for s in 0 ..< slotsInPlay():
      if atomicLoadN(tagSlot(s), ATOMIC_ACQUIRE) == t:
        return true

# ---------------- the seq fence ----------------
# Seq structure mutations and collections exclude each other; seq ops run
# concurrently with seq ops and collections with collections.

proc nimSeqFenceEnter*() =
  ## Taken by `seq` around changes of a buffer the collector traces.
  if lockState == HasNoLock:
    let s = getStripeIdx()
    while true:
      # SEQ_CST inc-then-check pairs with the collector's SEQ_CST
      # inc-then-drain (Dekker-style store/load ordering)
      discard atomicAddFetch(seqActiveSlot(s), 1, ATOMIC_SEQ_CST)
      if atomicLoadN(addr gGcActive, ATOMIC_SEQ_CST) == 0: break
      discard atomicSubFetch(seqActiveSlot(s), 1, ATOMIC_SEQ_CST)
      while atomicLoadN(addr gGcActive, ATOMIC_ACQUIRE) != 0:
        discard
    lockState = HasFence
  if lockState == HasFence:
    inc fenceDepth

proc nimSeqFenceExit*() =
  if lockState == HasFence:
    dec fenceDepth
    if fenceDepth == 0:
      lockState = HasNoLock
      discard atomicSubFetch(seqActiveSlot(getStripeIdx()), 1, ATOMIC_SEQ_CST)

proc gcFenceEnter() =
  ## A collection announces itself and waits for open seq fences to drain.
  discard atomicAddFetch(addr gGcActive, 1, ATOMIC_SEQ_CST)
  for s in 0 ..< NumStripes:
    while atomicLoadN(seqActiveSlot(s), ATOMIC_SEQ_CST) > 0:
      discard

proc gcFenceExit() =
  discard atomicSubFetch(addr gGcActive, 1, ATOMIC_SEQ_CST)

# ---------------- candidate registration ----------------

proc registerLocal(c: Cell; op: CellOp) {.inline.} =
  ## Register a candidate in THIS thread's buffer. Winning `inRootsFlag`
  ## makes this the one buffer the cell sits in; buffered cells are forced
  ## live by every collection, so no buffer entry can dangle.
  if rcTestSetFlag(c, inRootsFlag):
    if gLocalRoots.d == nil: init(gLocalRoots)
    add(gLocalRoots, c, op)

proc rememberGenSuspect(c: Cell; op: CellOp; ctx: ptr CollCtx) {.inline.} =
  ## A stamped cell that lost a young→old edge without an RC death blow: a
  ## candidate buffer that is only scanned when the epoch advances.
  if rcTestSetFlag(c, inRootsFlag):
    if ctx.genSuspects.d == nil: init(ctx.genSuspects)
    add(ctx.genSuspects, c, op)

proc spillGenSuspects(ctx: ptr CollCtx) =
  ## Move suspects into the root set; they stay flagged, only the buffer changes.
  if ctx.genSuspects.len == 0: return
  if gLocalRoots.d == nil: init(gLocalRoots)
  for i in 0 ..< ctx.genSuspects.len:
    add(gLocalRoots, ctx.genSuspects.d[i].cell, ctx.genSuspects.d[i].op)
  ctx.genSuspects.len = 0

proc flushGenSuspects(ctx: ptr CollCtx) =
  ## Promote deferred young→old dec targets into the root set after an epoch
  ## advance (major collection). No-op while the epoch is stable.
  let e = atomicLoadN(addr gEpoch, ATOMIC_RELAXED)
  if ctx.genFlushReady and e == ctx.seenEpoch: return
  ctx.genFlushReady = true
  ctx.seenEpoch = e
  spillGenSuspects(ctx)

proc drainStripe(i: int) =
  ## Apply the pending decrements of one stripe queue; the cells become THIS
  ## thread's candidates. Process published entries, wait out the publication
  ## window of in-flight reservers, then close the batch with a CAS (a plain
  ## reset could orphan a slot a producer is publishing into). Reservations
  ## past QueueSize never wrote anything, so an overflowed counter is reset.
  let st = stripe(i)
  acquire st.consumerLock
  var consumed = 0
  while true:
    let reserved = atomicLoadN(decLen(i), ATOMIC_ACQUIRE)
    let n = min(reserved, QueueSize)
    for j in consumed ..< n:
      while atomicLoadN(decReady(i, j), ATOMIC_ACQUIRE) == 0: discard
      let e = decSlot(i, j)
      trialDec(e.cell)
      registerLocal(e.cell, e.op)
      atomicStoreN(decReady(i, j), 0, ATOMIC_RELAXED)
    consumed = n
    if reserved >= QueueSize:
      discard atomicExchangeN(decLen(i), 0, ATOMIC_ACQ_REL)
      break
    else:
      var cur = reserved
      if atomicCompareExchangeN(decLen(i), addr cur, 0, false,
                                ATOMIC_ACQ_REL, ATOMIC_RELAXED):
        break
      # new reservations arrived during processing: consume them too
  release st.consumerLock

proc drainAllStripes() =
  for i in 0 ..< NumStripes:
    drainStripe(i)

proc adoptOrphans() =
  ## Adopt candidates spilled by exited threads.
  if roots.len > 0:               # racy peek; exact under the lock
    acquire gMergeLock
    if gLocalRoots.d == nil: init(gLocalRoots)
    for i in 0 ..< roots.len:
      add(gLocalRoots, roots.d[i].cell, roots.d[i].op)
    roots.len = 0
    release gMergeLock

proc trace(s: Cell; op: CellOp; j: var GcEnv) {.inline.} =
  op(s, addr j)

proc free(s: Cell; op: CellOp) =
  # a cell that sits in a candidate buffer stays: the buffer points at it
  if (loadRc(s) and inRootsFlag) == 0:
    op(s, nil)

proc nimTraceRef*(q: pointer; op: CellOp; env: pointer) {.inline, enableTrace.} =
  ## Called by a compiler-generated `=trace`: `q` is the address of a `ref`
  ## field, `op` the cell operation of its target type.
  let p = cast[ptr pointer](q)
  # read the slot exactly once: mutators may exchange it concurrently
  let v = p[]
  if v != nil:
    let j = cast[ptr GcEnv](env)
    j.traceStack.add TraceEntry(slot: p, val: v, op: op)

# ---------------- grace period for freed batches ----------------

proc graceSatisfied(): bool =
  ## Has every capture recorded in `gPendingWatch` finished?
  result = true
  for i in 0 ..< gPendingWatch.len:
    let w = gPendingWatch.d[i]
    let s = int(hiWord(w))
    let tg = loWord(w)
    if atomicLoadN(tagSlot(s), ATOMIC_ACQUIRE) == tg and
       atomicLoadN(phaseSlot(s), ATOMIC_ACQUIRE) == 1:
      return false

proc buildPendingWatch(): bool =
  ## Record the collections in their CAPTURE phase right now: only they can
  ## hold a stale (slot, value) snapshot of the cells we are about to free.
  if gPendingWatch.d == nil: init gPendingWatch
  gPendingWatch.len = 0
  for s in 0 ..< slotsInPlay():
    if s != gCtx.slot:
      let tg = atomicLoadN(tagSlot(s), ATOMIC_ACQUIRE)
      if tg != 0 and atomicLoadN(phaseSlot(s), ATOMIC_ACQUIRE) == 1:
        gPendingWatch.add((int64(s) shl 32) or tg)
  result = gPendingWatch.len > 0

proc releasePending() =
  ## Free this thread's parked batch and give its slot up. Called at the start
  ## of every collection, by which time the watched captures have long ended.
  if not gPendingActive: return
  while not graceSatisfied(): discard
  gPendingActive = false
  gPendingWatch.len = 0
  atomicStoreN(tagSlot(gPendingSlot), 0'i64, ATOMIC_SEQ_CST)
  # destructors run here; `Collecting` keeps them from starting a nested
  # collection that would write into the batch we are walking
  let prev = lockState
  lockState = Collecting
  for i in 0 ..< gPendingCells.len:
    free(gPendingCells.d[i].cell, gPendingCells.d[i].op)
  gPendingCells.len = 0
  lockState = prev

# ---------------- phase 1: capture ----------------

proc prepareCapture() =
  if gCap.recs.d == nil:
    init gCap.recs
    init gCap.sccIdx
    init gCap.tstack
    init gCap.frames
    init gCap.edges
    init gCap.sccs
    init gCap.sccMembers
    init gCap.crossTgt
    init gCap.crossPend
    init gCap.prunedSrc
    init gCap.prunedTgt
    init gCap.ages
    init gCap.slots
  else:
    gCap.recs.len = 0
    gCap.sccIdx.len = 0
    gCap.tstack.len = 0
    gCap.frames.len = 0
    gCap.edges.len = 0
    gCap.sccs.len = 0
    gCap.sccMembers.len = 0
    gCap.crossTgt.len = 0
    gCap.crossPend.len = 0
    gCap.prunedSrc.len = 0
    gCap.prunedTgt.len = 0
    gCap.ages.len = 0
    gCap.slots.len = 0

proc growCaptureArrays(cap: ptr CaptureBufs) {.noinline.} =
  ## `recs`, `sccIdx`, `ages` and `tstack` are appended to together, only by
  ## `claimCell`: one capacity check on `recs` covers all four.
  resize(cap.recs, cap.recs.len + 1)
  let n = cap.recs.cap
  if cap.sccIdx.cap < n: resize(cap.sccIdx, n)
  if cap.ages.cap < n: resize(cap.ages, n)
  if cap.tstack.cap < n: resize(cap.tstack, n)

proc recordClaim(c: Cell; op: CellOp; cap: ptr CaptureBufs; idx: int; old: int64) {.inline.} =
  # rc is captured without the flag bits: the collector itself toggles
  # inRootsFlag between capture and commit, which must not look like a
  # mutation to the commit-time rc validation
  if idx >= cap.recs.cap: growCaptureArrays(cap)
  cap.recs.addUnchecked CaptureRec(cell: c, op: op,
                                   rcWord: loadRc(c) and not rcMask,
                                   lowlink: int32(idx), selfRefs: 0'i32)
  cap.sccIdx.addUnchecked -1'i32
  cap.ages.addUnchecked int32(if isEpochStamp(old): min(stampAge(old), 1000) else: 0)
  cap.tstack.addUnchecked int32(idx)

proc claimCell(c: Cell; op: CellOp; cap: ptr CaptureBufs;
               ctx: ptr CollCtx; pruneLive: bool; old0: int64): int32 =
  ## Dense index if this collection owns `c` (claiming it if it was
  ## unclaimed), -1 if another ACTIVE collection owns it, or -2 if `pruneLive`
  ## and the cell was proven live in the current epoch (an opaque live
  ## external: don't descend). `old0` is `c`'s claim word as already read.
  let idx = cap.recs.len
  if ctx.amSolo:
    # no other collection is (or can start) capturing: plain stores
    let old = old0
    if hiWord(old) == ctx.tag:
      return int32(loWord(old))
    if pruneLive and hiWord(old) == hiWord(ctx.epochStamp) and
        stampAge(old) >= YrcPromoteAge:
      return -2'i32
    c.rootIdx = (ctx.tag shl 32) or int64(idx)
    recordClaim(c, op, cap, idx, old)
    return int32(idx)
  var old = old0
  while true:
    if hiWord(old) == ctx.tag:
      return int32(loWord(old))
    if pruneLive and hiWord(old) == hiWord(ctx.epochStamp) and
        stampAge(old) >= YrcPromoteAge:
      return -2'i32
    # an epoch stamp is never a tag (tags stay below `epochBase`)
    if not isEpochStamp(old) and isActiveTag(hiWord(old)):
      return -1'i32
    if atomicCompareExchangeN(addr c.rootIdx, addr old,
                              (ctx.tag shl 32) or int64(idx), false,
                              ATOMIC_ACQ_REL, ATOMIC_RELAXED):
      recordClaim(c, op, cap, idx, old)
      return int32(idx)
    # a failed CAS leaves the fresh claim word in `old`; loop with it

proc capture(s: Cell; op: CellOp; j: var GcEnv; cap: ptr CaptureBufs) =
  ## Iterative Tarjan SCC over everything reachable from `s`. A frame's
  ## pending out-edges are the traceStack entries above frame.base. An SCC's
  ## out-edges are classified into internal/cross the moment it is emitted,
  ## so `edges` stays proportional to the DFS frontier.
  let ctx = addr gCtx
  let rootWord = loadClaim(s)
  if hiWord(rootWord) == ctx.tag: return
  # roots never prune: a dec-witnessed suspicion overrides any epoch stamp
  let root = claimCell(s, op, cap, ctx, pruneLive = false, old0 = rootWord)
  if root < 0:
    return   # another active collection owns this candidate; it handles it
  trace(s, op, j)
  # the innermost frame lives in `u`/`base`/`ebase`; `cap.frames` holds its ancestors
  var u = root
  var base = 0
  var ebase = 0'i32
  while true:
    if j.traceStack.len > base:
      let entry = j.traceStack.pop()
      let t = cast[Cell](entry.val)
      # field address for the allDead nil pass
      cap.slots.add entry.slot
      let cw = loadClaim(t)
      if hiWord(cw) == ctx.tag:
        let v = int32(loWord(cw))
        if v == u:
          # a self edge is internal by construction and already in `rcWord`
          inc cap.recs.d[u].selfRefs
        else:
          cap.edges.add v
          if cap.sccIdx.d[v] < 0'i32 and v < cap.recs.d[u].lowlink:
            cap.recs.d[u].lowlink = v
      else:
        let childBase = j.traceStack.len
        let v = claimCell(t, entry.op, cap, ctx, pruneLive = true, old0 = cw)
        if v == -1'i32:
          # cross-collection edge: the owner sees our reference in the rc
          # word and classifies the target live; re-registered before commit
          cap.crossPend.add(t, entry.op)
        elif v == -2'i32:
          # target proven live this epoch: opaque live external, no descent.
          # Taint u's SCC: its own "live" verdict may lean on the stamp.
          if cap.prunedSrc.len == 0 or cap.prunedSrc.d[cap.prunedSrc.len - 1] != u:
            cap.prunedSrc.add u
          # a stamped target that looks RC-dead stays examinable; winning
          # `inRootsFlag` keeps it alive until our commit hands it over
          if (loadRc(t) and not rcMask) == 0 and rcTestSetFlag(t, inRootsFlag):
            cap.prunedTgt.add(t, entry.op)
        else:
          cap.edges.add v
          trace(t, entry.op, j)
          cap.frames.add TarjanFrame(u: u, ebase: ebase, base: base)
          u = v
          ebase = int32(cap.edges.len)
          base = childBase
    else:
      let lowU = cap.recs.d[u].lowlink
      if lowU == u:
        # u is the root of an SCC: pop the members off the Tarjan stack
        let sid = int32(j.nScc)
        let memStart = int32(cap.sccMembers.len)
        var sum = 0
        while true:
          let m = cap.tstack.pop()
          cap.sccIdx.d[m] = sid
          cap.sccMembers.add m
          # `- selfRefs`: a self edge counts in both `sumRefs` and `internal`
          sum = sum + (cap.recs.d[m].rcWord shr rcShift) + 1 - int(cap.recs.d[m].selfRefs)
          if m == u: break
        # classify this SCC's out-edges now that every target's SCC is final
        let crossOff = int32(cap.crossTgt.len)
        var internal = 0
        for i in int(ebase) ..< cap.edges.len:
          let sv = cap.sccIdx.d[cap.edges.d[i]]
          if sv == sid: inc internal
          else: cap.crossTgt.add sv
        cap.edges.len = int(ebase)
        cap.sccs.add SccRec(sumRefs: sum, internal: internal,
                            memStart: memStart, crossOff: crossOff)
        inc j.nScc
      if cap.frames.len == 0: break
      let f = cap.frames.pop()
      if lowU < cap.recs.d[f.u].lowlink:
        cap.recs.d[f.u].lowlink = lowU
      u = f.u
      ebase = f.ebase
      base = f.base

# ---------------- phase 2: deadness, side arrays only ----------------

proc computeDeadness(j: var GcEnv; cap: ptr CaptureBufs) =
  let nScc = j.nScc
  # the sentinel record ([nScc]) closing the last SCC's slices
  cap.sccs.add SccRec(memStart: int32(cap.sccMembers.len),
                      crossOff: int32(cap.crossTgt.len))
  # pruned out-edges taint the source SCC: a "live" verdict may lean on a
  # stamp that went stale within the epoch
  for i in 0 ..< cap.prunedSrc.len:
    let s = cap.sccIdx.d[cap.prunedSrc.d[i]]
    cap.sccs.d[s].flags = cap.sccs.d[s].flags or flagPruned
  # cells that stay registered as candidates count as externally
  # referenced: the buffer itself points at them
  for m in 0 ..< cap.recs.len:
    if (loadRc(cap.recs.d[m].cell) and inRootsFlag) != 0:
      let s = cap.sccIdx.d[m]
      cap.sccs.d[s].flags = cap.sccs.d[s].flags or flagForcedLive
  # Tarjan emits sinks first, so every cross edge goes from a higher SCC id
  # to a lower one: one reverse scan settles everything
  var s = nScc - 1
  while s >= 0:
    let ext = cap.sccs.d[s].sumRefs - cap.sccs.d[s].internal - cap.sccs.d[s].deadIn
    if (cap.sccs.d[s].flags and flagForcedLive) == 0'u8 and ext == 0:
      cap.sccs.d[s].flags = cap.sccs.d[s].flags or flagDead
      inc j.nDeadScc
      for k in int(cap.sccs.d[s].crossOff) ..< int(cap.sccs.d[s+1].crossOff):
        inc cap.sccs.d[cap.crossTgt.d[k]].deadIn
    else:
      # a live SCC keeps everything it points to alive
      for k in int(cap.sccs.d[s].crossOff) ..< int(cap.sccs.d[s+1].crossOff):
        let t = cap.crossTgt.d[k]
        cap.sccs.d[t].flags = cap.sccs.d[t].flags or flagForcedLive
    dec s

# ---------------- phase 3: validate & commit ----------------

proc markDirtyFromQueues(j: var GcEnv; cap: ptr CaptureBufs) =
  ## Any cell with a dec enqueued since the last drain had its reference set
  ## changed during capture. Peek (don't drain!) the queues and taint the
  ## affected SCCs; the entries stay queued for the next merge.
  let ctx = addr gCtx
  for i in 0 ..< NumStripes:
    let decLen = min(atomicLoadN(decLen(i), ATOMIC_ACQUIRE), QueueSize)
    for k in 0 ..< decLen:
      if atomicLoadN(decReady(i, k), ATOMIC_ACQUIRE) != 0:
        let c = decSlot(i, k).cell
        if isStamped(c, ctx):
          let s = cap.sccIdx.d[denseIdx(c)]
          cap.sccs.d[s].flags = cap.sccs.d[s].flags or flagDirty

proc demoteTouchedDead(j: var GcEnv; cap: ptr CaptureBufs) =
  ## One descending demotion pass: a dead SCC a mutator touched survives, and
  ## so must its dead cross targets (their deadIn assumed this SCC dies too).
  var s = j.nScc - 1
  while s >= 0:
    if (cap.sccs.d[s].flags and flagDead) != 0'u8:
      var ok = (cap.sccs.d[s].flags and flagDirty) == 0'u8
      if ok:
        for mi in int(cap.sccs.d[s].memStart) ..< int(cap.sccs.d[s+1].memStart):
          let m = cap.sccMembers.d[mi]
          if (loadRc(cap.recs.d[m].cell) and not rcMask) != cap.recs.d[m].rcWord:
            ok = false
            break
      if not ok:
        cap.sccs.d[s].flags = cap.sccs.d[s].flags and not flagDead
        inc j.nAborted
        for k in int(cap.sccs.d[s].crossOff) ..< int(cap.sccs.d[s+1].crossOff):
          let t = cap.crossTgt.d[k]
          if (cap.sccs.d[t].flags and flagDead) != 0'u8:
            cap.sccs.d[t].flags = cap.sccs.d[t].flags or flagDirty
        let m = cap.sccMembers.d[cap.sccs.d[s].memStart]
        registerLocal(cap.recs.d[m].cell, cap.recs.d[m].op)
    elif cap.prunedSrc.len > 0:
      # a prune happened in THIS collection, so every "live" verdict is
      # suspect: keep one member of every survivor examinable -- as a
      # suspect, settled by the next epoch advance, not as a root
      let m = cap.sccMembers.d[cap.sccs.d[s].memStart]
      rememberGenSuspect(cap.recs.d[m].cell, cap.recs.d[m].op, addr gCtx)
    dec s

proc validateDead(j: var GcEnv; cap: ptr CaptureBufs) =
  ## Demote every dead SCC a mutator touched during capture: dirty via the
  ## queues, or a direct incRef visible as a changed rc word.
  markDirtyFromQueues(j, cap)
  demoteTouchedDead(j, cap)

proc isDeadCell(w: int64; ctx: ptr CollCtx; cap: ptr CaptureBufs): bool {.inline.} =
  ## `w` is the target's claim word, read once by the caller
  hiWord(w) == ctx.tag and
    (cap.sccs.d[cap.sccIdx.d[int32(loWord(w))]].flags and flagDead) != 0'u8

proc commitDead(j: var GcEnv; cap: ptr CaptureBufs) =
  validateDead(j, cap)
  let ctx = addr gCtx
  # cross-collection edge targets are owner-live this round; registering them
  # keeps them examinable later
  for i in 0 ..< cap.crossPend.len:
    registerLocal(cap.crossPend.d[i].cell, cap.crossPend.d[i].op)
  # pruned targets: capture already won their `inRootsFlag`, they only
  # change buffers here
  if cap.prunedTgt.len > 0:
    if gLocalRoots.d == nil: init(gLocalRoots)
    for i in 0 ..< cap.prunedTgt.len:
      add(gLocalRoots, cap.prunedTgt.d[i].cell, cap.prunedTgt.d[i].op)
  # Everything captured dies and no slot can point outside the dead set:
  # nil from the capture-time slot log and free, no re-trace.
  let allDead = j.nDeadScc == j.nScc and j.nAborted == 0 and
                cap.crossPend.len == 0 and cap.prunedSrc.len == 0
  if allDead:
    # A capture still in flight may hold stale snapshots of our dead cells:
    # park the batch (freed by `releasePending`, our tag stays active).
    let deferred = cap.recs.len > 0 and buildPendingWatch()
    if deferred and gPendingCells.d == nil: init gPendingCells
    for i in 0 ..< cap.slots.len:
      cap.slots.d[i][] = nil
    for m in 0 ..< cap.recs.len:
      if deferred:
        gPendingCells.add(cap.recs.d[m].cell, cap.recs.d[m].op)
      else:
        free(cap.recs.d[m].cell, cap.recs.d[m].op)
    j.freed = cap.recs.len
    if deferred:
      gPendingSlot = ctx.slot
      gPendingActive = true
  else:
    if gFreeBuf.d == nil: init gFreeBuf
    gFreeBuf.len = 0
    j.toFree = gFreeBuf
    for s in 0 ..< j.nScc:
      if (cap.sccs.d[s].flags and flagDead) != 0'u8:
        for mi in int(cap.sccs.d[s].memStart) ..< int(cap.sccs.d[s+1].memStart):
          let m = cap.sccMembers.d[mi]
          let cell = cap.recs.d[m].cell
          let op = cap.recs.d[m].op
          j.toFree.add(cell, op)
          # nil every slot so the destructor cannot dec these edges again;
          # references to survivors are decremented for real, references into
          # the dead group die with it (already accounted by deadIn)
          trace(cell, op, j)
          while j.traceStack.len > 0:
            let entry = j.traceStack.pop()
            let t = cast[Cell](entry.val)
            entry.slot[] = nil
            let tw = atomicLoadN(addr t.rootIdx, ATOMIC_RELAXED)
            if not isDeadCell(tw, ctx, cap):
              trialDec(t)
              # a stamped target was not analyzed by THIS collection:
              # re-root it only on a true RC death blow, remember it as a
              # suspect otherwise (generational young→old)
              if isEpochStamp(tw):
                if (loadRc(t) shr rcShift) < 0:
                  registerLocal(t, entry.op)
                else:
                  rememberGenSuspect(t, entry.op, ctx)
    # epoch-stamp what this collection PROVED live with the SCC's survival
    # age (the age of its youngest member: promotion is all-or-nothing per
    # SCC). Demoted and pruned SCCs stay unproven.
    for s in 0 ..< j.nScc:
      if (cap.sccs.d[s].flags and (flagDead or flagDirty or flagPruned)) == 0'u8:
        let memStart = int(cap.sccs.d[s].memStart)
        let memEnd = int(cap.sccs.d[s+1].memStart)
        var age = high(int32)
        for mi in memStart ..< memEnd:
          let a = cap.ages.d[cap.sccMembers.d[mi]]
          if a < age: age = a
        let stamp = ctx.epochStamp or int64(age + 1'i32)
        for mi in memStart ..< memEnd:
          atomicStoreN(addr cap.recs.d[cap.sccMembers.d[mi]].cell.rootIdx,
                       stamp, ATOMIC_RELAXED)
    j.freed = j.toFree.len
    if j.toFree.len > 0 and buildPendingWatch():
      # park the whole batch by swapping buffers
      let spare = gPendingCells
      gPendingCells = j.toFree
      j.toFree = spare
      j.toFree.len = 0
      gPendingSlot = ctx.slot
      gPendingActive = true
    else:
      for i in 0 ..< j.toFree.len:
        free(j.toFree.d[i].cell, j.toFree.d[i].op)
      j.toFree.len = 0
    gFreeBuf = j.toFree

proc startCollection(minRoots, keepBelow: int; slice: var CellSeq;
                     wait: bool; drainAll = false): bool =
  ## Drain pending decrements (own stripe; all stripes for a full collect)
  ## and try to become a collector over THIS THREAD's candidates: claim a
  ## tag slot and steal the thread-local buffer as this collection's slice.
  result = false
  releasePending()   # last collection's batch: its watch list is long clear
  if drainAll: drainAllStripes()
  else: drainStripe(getStripeIdx())
  adoptOrphans()
  flushGenSuspects(addr gCtx)  # epoch advanced ⇒ suspects become roots
  while gLocalRoots.len >= minRoots and gLocalRoots.len > keepBelow:
    acquire gMergeLock
    var slot = -1
    let inPlay = gParSlots
    for sl in 0 ..< inPlay:
      if atomicLoadN(tagSlot(sl), ATOMIC_RELAXED) == 0'i64:
        slot = sl
        break
    if slot < 0 and inPlay < MaxPar:
      # every slot in play is busy and the table has room: widen the pool
      slot = inPlay
      atomicStoreN(addr gParSlots, inPlay + 1, ATOMIC_SEQ_CST)
    if slot < 0:
      release gMergeLock
      if not wait: break
      while not anySlotFree(): discard
      drainStripe(getStripeIdx())   # the world moved while we waited
      adoptOrphans()
    else:
      gTagCounter = (gTagCounter + 1) and (epochBase - 1)  # tags below the stamp namespace
      if gTagCounter == 0: gTagCounter = 1
      gCtx.tag = gTagCounter
      gCtx.slot = slot
      gCtx.epochStamp = epochStamp(atomicLoadN(addr gEpoch, ATOMIC_RELAXED))
      var othersActive = false
      for sl in 0 ..< gParSlots:
        if sl != slot and atomicLoadN(tagSlot(sl), ATOMIC_RELAXED) != 0'i64:
          othersActive = true
      gCtx.amSolo = not othersActive
      if gCtx.amSolo:
        atomicStoreN(addr gSoloCapture, 1, ATOMIC_RELEASE)
      atomicStoreN(phaseSlot(slot), 1, ATOMIC_RELEASE)
      atomicStoreN(tagSlot(slot), gCtx.tag, ATOMIC_SEQ_CST)
      release gMergeLock
      # our buffer, our slice: no lock needed
      if keepBelow == 0:
        slice = gLocalRoots     # steal the whole buffer
        if gSpareRoots.d != nil:
          gLocalRoots = gSpareRoots
          gLocalRoots.len = 0
          gSpareRoots = default(CellSeq)
        else:
          init(gLocalRoots)
      else:
        init(slice, max(gLocalRoots.len - keepBelow, 8))
        for i in keepBelow ..< gLocalRoots.len:
          slice.add(gLocalRoots.d[i].cell, gLocalRoots.d[i].op)
        gLocalRoots.len = keepBelow
      result = true
      break

proc finishCollection() =
  if atomicAddFetch(addr gCollectionCounter, 1, ATOMIC_RELAXED) mod YrcEpochLen == 0:
    discard atomicAddFetch(addr gEpoch, 1, ATOMIC_RELAXED)
  if gPendingActive:
    # a batch is parked under this collection's tag: clear only the phase,
    # the tag keeps foreign captures from claiming cells of the batch
    atomicStoreN(phaseSlot(gCtx.slot), 0, ATOMIC_RELEASE)
  else:
    atomicStoreN(tagSlot(gCtx.slot), 0'i64, ATOMIC_SEQ_CST)
    atomicStoreN(phaseSlot(gCtx.slot), 0, ATOMIC_RELEASE)
  gCtx.tag = 0
  gCtx.amSolo = false

proc collectCyclesImpl(j: var GcEnv; slice: var CellSeq) =
  # All destruction is deferred to collection time: plain garbage forms
  # singleton SCCs with external count 0 and is freed like the cycles.
  if gTraceBuf.d == nil: init gTraceBuf
  gTraceBuf.len = 0
  j.traceStack = gTraceBuf
  prepareCapture()
  let cap = addr gCap
  j.nScc = 0
  var i = slice.len - 1
  while i >= 0:
    capture(slice.d[i].cell, slice.d[i].op, j, cap)
    dec i
  j.touched = cap.recs.len
  atomicStoreN(phaseSlot(gCtx.slot), 2, ATOMIC_RELEASE)  # capture done
  if gCtx.amSolo:
    atomicStoreN(addr gSoloCapture, 0, ATOMIC_RELEASE)
  # Unregister the processed candidates before computing deadness: only
  # cells that STAY registered count as externally referenced.
  for k in 0 ..< slice.len:
    rcClearFlag(slice.d[k].cell, inRootsFlag)
  computeDeadness(j, cap)
  commitDead(j, cap)
  j.keepThreshold = j.freed == j.touched and j.touched > 0
  gTraceBuf = j.traceStack   # hand the (possibly grown) buffer back

proc runCollection(j: var GcEnv; slice: var CellSeq) =
  ## One collection over the stolen slice, concurrently with mutators and
  ## with the other collecting threads over disjoint partitions.
  gcFenceEnter()      # freeze seq structure mutations, not ref writes
  if not gCtx.amSolo:
    # a solo collection claims with plain stores; nobody else may claim
    # cells until its capture phase is over
    while atomicLoadN(addr gSoloCapture, ATOMIC_ACQUIRE) != 0: discard
  let prev = lockState
  lockState = Collecting
  collectCyclesImpl(j, slice)
  lockState = prev
  gcFenceExit()
  finishCollection()
  if gSpareRoots.d == nil and slice.d != nil:
    gSpareRoots = slice          # recycle it as the next steal's replacement
  else:
    deinit slice

proc collectCycles() =
  if lockState != HasNoLock:
    # Collecting: a destructor-driven dec filled the stripe; re-entering
    # would corrupt collector state. HasFence: becoming a collector would
    # wait on our own open fence. Just make room in the queue.
    drainStripe(getStripeIdx())
    return
  var slice = default(CellSeq)
  if startCollection(rootsThreshold, 0, slice, wait = true):
    let nRoots = slice.len
    var j = GcEnv()
    runCollection(j, slice)
    if j.keepThreshold:
      discard
    elif j.freed * 2 >= j.touched:
      rootsThreshold = max(rootsThreshold div 3 * 2, 16)
    elif rootsThreshold < high(int) div 4:
      if rootsThreshold <= 0: rootsThreshold = defaultThreshold
      rootsThreshold = rootsThreshold div 2 + rootsThreshold
      # an expensive run (large graph) raises the threshold more
      if j.touched > nRoots * 4:
        rootsThreshold = rootsThreshold div 2 + rootsThreshold
      rootsThreshold = min(rootsThreshold, defaultThreshold * 16)
      rootsThreshold = min(rootsThreshold, nRoots * 2)

proc releaseCollectorScratch() =
  ## Drop the per-thread collector scratch; after an exhaustive collect and
  ## on thread exit, so one large capture cannot pin memory forever.
  deinit(gCtx.genSuspects)
  deinit(gSpareRoots)
  deinit(gTraceBuf)
  deinit(gFreeBuf)
  deinit(gPendingCells)
  deinit(gPendingWatch)
  deinit(gCap.recs)
  deinit(gCap.sccIdx)
  deinit(gCap.tstack)
  deinit(gCap.frames)
  deinit(gCap.edges)
  deinit(gCap.sccs)
  deinit(gCap.sccMembers)
  deinit(gCap.crossTgt)
  deinit(gCap.crossPend)
  deinit(gCap.prunedSrc)
  deinit(gCap.prunedTgt)
  deinit(gCap.ages)
  deinit(gCap.slots)

proc GC_runOrc*() =
  ## Forces an exhaustive cycle collection. Candidates of other RUNNING
  ## threads are theirs to collect.
  if lockState != HasNoLock: return
  # age out every liveness stamp so nothing is pruned; loop until quiet, a
  # commit may remember further suspects
  discard atomicAddFetch(addr gEpoch, 1, ATOMIC_RELAXED)
  var slice = default(CellSeq)
  while true:
    spillGenSuspects(addr gCtx)
    if not startCollection(1, 0, slice, wait = true, drainAll = true):
      break
    var j = GcEnv()
    runCollection(j, slice)
  releasePending()   # a full collect leaves no batch parked
  spillGenSuspects(addr gCtx)
  if gLocalRoots.len == 0: deinit(gLocalRoots)
  releaseCollectorScratch()

proc GC_fullCollect*() =
  ## Forces a full garbage collection pass. With `--mm:yrc` an alias for `GC_runOrc`.
  GC_runOrc()

proc GC_enableOrc*() =
  ## Enables the cycle collector subsystem of `--mm:yrc`.
  rootsThreshold = 0

proc GC_disableOrc*() =
  ## Disables the cycle collector subsystem of `--mm:yrc`.
  rootsThreshold = high(int)

proc GC_prepareOrc*(): int =
  drainAllStripes()
  adoptOrphans()
  result = gLocalRoots.len

proc GC_partialCollect*(limit: int) =
  if lockState != HasNoLock: return
  var slice = default(CellSeq)
  if startCollection(limit + 1, limit, slice, wait = true):
    var j = GcEnv()
    runCollection(j, slice)

proc nimThreadTeardown*() =
  ## Called when a thread exits (`std/rawthreads`): drain our stripe so
  ## nothing of ours is stranded in a queue no other thread hashes to, then
  ## spill our candidates to the orphan buffer for the next collection.
  releasePending()   # nobody else can free this thread's parked batch
  drainStripe(getStripeIdx())
  spillGenSuspects(addr gCtx)
  if gLocalRoots.len > 0:
    acquire gMergeLock
    if roots.d == nil: init(roots)
    for i in 0 ..< gLocalRoots.len:
      add(roots, gLocalRoots.d[i].cell, gLocalRoots.d[i].op)
    release gMergeLock
  deinit(gLocalRoots)
  releaseCollectorScratch()

# ---------------- the compiler's entry points ----------------

proc enqueueDec(cell: Cell; op: CellOp) =
  ## Lock-free producer: reserve a slot with a fetch-add, store the cell, then
  ## publish by setting `ready` (release). A reservation past
  ## QueueSize never writes: the reserver drains the stripe to make room and
  ## collects when the candidate set is at the threshold.
  let idx = getStripeIdx()
  while true:
    let slot = atomicAddFetch(decLen(idx), 1, ATOMIC_ACQ_REL) - 1
    if slot < QueueSize:
      let e = decSlot(idx, slot)
      e.cell = cell
      e.op = op
      atomicStoreN(decReady(idx, slot), 1, ATOMIC_RELEASE)
      break
    drainStripe(idx)
    if gLocalRoots.len >= rootsThreshold:
      collectCycles()

func nimDecRefCyclic*(p: pointer; op: CellOp): bool {.inline.} =
  ## The decrement of a cell whose type can form a cycle: deferred, the cell
  ## is only ever freed by a collection, so this never returns true.
  ## `op == nil` is a class that cannot form a cycle: freed promptly, as with
  ## `arc` (nothing traces it and it is never queued).
  {.cast(noSideEffect).}:
    let cell = cast[Cell](p)
    if op == nil:
      result = atomicSubFetch(addr cell.rc, rcIncrement, ATOMIC_ACQ_REL) < 0
    else:
      enqueueDec(cell, op)
      result = false
