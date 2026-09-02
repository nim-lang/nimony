# Relay-based backend dispatch.
# Groups BackendRelays and global ring state so backends can access
# the completion queue, slot arena, and pool.

import ./types
import ./slots
import std/[threadpool, ticketlocks, stripes, syncio]

const CqSize* = 4096
const MaxOps* = 8192

type
  BackendRelays* = object
    poll*: proc (timeoutMs: int): bool {.nimcall.}
    close*: proc () {.nimcall.}
    forgetFd*: proc (fd: cint) {.nimcall.}
      ## Drop any backend-side per-fd registration/bookkeeping before a fd is
      ## closed (e.g. epoll's ADD/MOD tracking). `nil` for backends where the
      ## OS already tears this down on close (kqueue) — callers
      ## must nil-check before calling.

proc ioLanes*(): int {.inline.} =
  ## Number of independent I/O lanes: one per pool worker, plus one trailing
  ## lane shared by every non-worker submitter.
  workerCount + 1

proc ioLane*(): int {.inline.} =
  ## The lane this thread owns. Every piece of per-thread ring state (the
  ## deferred op queue, the slot arena, the backend's poller instance) is
  ## indexed by this and by nothing else.
  ##
  ## It is deliberately *not* `threadIdx`: that is a threadvar defaulting to 0,
  ## so the main thread — which submits and polls through `waitCompletions` —
  ## reported the same index as worker 0 and the two drove one unlocked arena
  ## and one single-producer io_uring submission ring in parallel.
  ##
  ## Caveat: all non-worker threads share the trailing lane, so the ring still
  ## supports only *one* foreign submitter at a time (the usual "main thread
  ## drives the ring" shape). Ops submitted on that lane are drained by that
  ## thread's own `poll`, i.e. by its `waitCompletions`/`pollCompletions` loop.
  if isPoolWorker(): threadIdx else: workerCount

var gOpQueues*: seq[FifoStripe[OpContext]]
proc initOpQueues*() =
  gOpQueues = newSeq[FifoStripe[OpContext]](ioLanes())
  for q in gOpQueues:
    q.init(MaxOps)

var gSlots*: seq[SlotArena]
proc initSlots*() =
  gSlots = newSeq[SlotArena](ioLanes())
  for s in gSlots:
    s.init(MaxOps)

# ------------------------------------------------------------- deadlines ---
#
# Every op carries a deadline, so a per-lane min-heap of them answers two
# questions the poll loop needs: how long it may wait, and which ops have run
# out of time. Both in O(1) and O(log n); scanning the arena would be O(MaxOps)
# on every single poll.
#
# Entries are never removed when an op completes normally — finding and
# deleting one would cost more than leaving it. Instead an entry names the
# slot *generation* it was armed for, and a stale entry is dropped when it
# reaches the top. The heap is therefore bounded by ops-ever-armed between
# two expiries rather than by ops-in-flight, which is why `arm` skips `never`:
# an op with no deadline must not leave a permanent entry behind.

type
  TimerEntry* = object
    at*: Deadline
    slot*: int32
    gen*: uint32

  TimerHeap* = object
    a*: seq[TimerEntry]

proc len*(h: TimerHeap): int {.inline.} = h.a.len

proc swapEntries(h: var TimerHeap; i, j: int) {.inline.} =
  ## Both elements are copied out before either is written back: assigning one
  ## element of a seq straight from another is a mutable/immutable alias of
  ## the same object, which the compiler refuses — rightly, since it is the
  ## shape that hides real aliasing bugs.
  let a = h.a[i]
  let b = h.a[j]
  h.a[i] = b
  h.a[j] = a

proc push*(h: var TimerHeap; e: TimerEntry) =
  h.a.add e
  var i = h.a.len - 1
  while i > 0:
    let parent = (i - 1) div 2
    if h.a[parent].at <= h.a[i].at: break
    h.swapEntries(parent, i)
    i = parent

proc popMin*(h: var TimerHeap): TimerEntry =
  result = h.a[0]
  let last = h.a.len - 1
  let tail = h.a[last]
  h.a[0] = tail
  h.a.shrink last
  var i = 0
  while true:
    let l = 2 * i + 1
    let r = l + 1
    var small = i
    if l < h.a.len and h.a[l].at < h.a[small].at: small = l
    if r < h.a.len and h.a[r].at < h.a[small].at: small = r
    if small == i: break
    h.swapEntries(small, i)
    i = small

var gTimers*: seq[TimerHeap]
proc initTimers*() =
  gTimers = newSeq[TimerHeap](ioLanes())

proc armDeadline*(lane: int; slotIdx: int) =
  ## Record the deadline of the op just allocated into `slotIdx`. A `never`
  ## deadline arms nothing, so the heap holds only ops that can actually
  ## expire.
  let d = gSlots[lane].slots[slotIdx].op.deadline
  if d == never: return
  gTimers[lane].push TimerEntry(at: d, slot: int32(slotIdx),
                                gen: gSlots[lane].slots[slotIdx].gen)

proc nextDeadline*(lane: int): Deadline =
  ## The earliest deadline this lane is waiting on, skipping entries whose op
  ## has already completed. `never` when there is nothing to wait for.
  while gTimers[lane].len > 0:
    let e = gTimers[lane].a[0]
    let s = addr gSlots[lane].slots[e.slot.int]
    if s.inUse and s.gen == e.gen: return e.at
    discard gTimers[lane].popMin()
  result = never

proc waitMillis*(lane: int; requested: int): int =
  ## How long the backend may actually block: what the caller asked for, or
  ## the time to the earliest deadline, whichever is sooner. This is what
  ## turns a fixed poll interval into "sleep exactly until something is due".
  let d = nextDeadline(lane)
  if d == never: return requested
  let ms = millisUntil(d, monoNow())
  result = if requested < 0 or ms < requested: ms else: requested

var
  gNextSeq*: SeqNum
  gCqLock*: TicketLock
  gCq*: seq[IoCompletion]
  gCqHead*: int
  gCqTail*: int
  gCqCount*: int

const
  IoTimedOut* = -110
    ## Completion result for an op whose deadline passed. `ETIMEDOUT` on
    ## Linux, and negative like every other failure the ring reports.

proc complete*(slotIdx: int; res: int) =
  let lane = ioLane()
  let slot = addr gSlots[lane].slots[slotIdx]
  if slot.op.res != 0:
    cast[ptr int](slot.op.res)[] = res
  let cont  = slot.op.cont
  let fd    = slot.op.fd
  let seqnum = slot.op.seqnum
  let kind  = slot.op.kind
  gSlots[lane].freeSlot(slotIdx)
  if cont.fn != nil:
    submit(cont, int(fd))
  else:
    gCqLock.acquire()
    if gCqCount < CqSize:
      gCq[gCqTail] = IoCompletion(id: seqnum, op: kind, fd: fd, result: res)
      gCqTail = (gCqTail + 1) and (CqSize - 1)
      inc gCqCount
    gCqLock.release()

var gCancelInFlight*: proc (slotIdx: int; gen: uint32) {.nimcall.}
  ## Set by a backend where the OS keeps working on an op after this process
  ## has stopped waiting for it. The readiness backends leave it `nil`: an
  ## epoll/kqueue registration owns nothing, so dropping it is the whole of
  ## cancelling. io_uring is different — the kernel holds the op's buffer until
  ## it acknowledges a cancel, so an op completed here on a blown deadline must
  ## still be taken away from the kernel, or it writes into a buffer whose
  ## owner has moved on.

proc expireDeadlines*(lane: int) =
  ## Complete every op in this lane whose deadline has passed. Called by each
  ## backend after it waits, so a deadline fires whether or not any I/O did.
  ##
  ## This is what makes "nothing parks forever" structural: an op cannot be
  ## submitted without a deadline, and every deadline is either met or arrives
  ## here. A caller parked on a peer that has gone quiet is resumed with
  ## `IoTimedOut` rather than being left for the process's lifetime.
  if gTimers[lane].len == 0: return
  let now = monoNow()
  while gTimers[lane].len > 0:
    let e = gTimers[lane].a[0]
    let s = addr gSlots[lane].slots[e.slot.int]
    if not s.inUse or s.gen != e.gen:
      discard gTimers[lane].popMin()      # its op finished in time
      continue
    if now < e.at: break                  # the earliest is still in the future
    discard gTimers[lane].popMin()
    # A timer op reaching its deadline is a success — that is the whole point
    # of it. Anything else has run out of time.
    let res = if s.op.kind == opTimeout: 0 else: IoTimedOut
    # Before the slot is freed and reused: an op that never touched the OS
    # (a pure timer) has nothing to take back, anything else may still be in
    # the kernel's hands.
    if gCancelInFlight != nil and s.op.kind != opTimeout:
      gCancelInFlight(e.slot.int, s.gen)
    complete(e.slot.int, res)
