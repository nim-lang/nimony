# Relay-based backend dispatch.
# Groups BackendRelays and global ring state so backends can access
# the completion queue, slot arena, and pool.

# The ring's globals are guarded by `gCqLock` and per-lane stripes, which the
# shared-global check cannot see yet (no `.guard` support).
{.feature: "assumeSync".}

import ./types
import ./slots
import std/[threadpool, ticketlocks, stripes, syncio]

const CqSize* = 4096
const
  InitialOpQueue* = 64
    ## Starting capacity of a lane's deferred-op queue; it doubles when full,
  MaxOpQueue* = 8192
    ## up to this. Beyond it a submitter helps drain the queue instead.

type
  BackendRelays* = object
    ## `unchecked`, not not-nil: `backendRelays` is a global that `setupRing`
    ## fills through `initPlatformBackend`, and it cannot be filled at its
    ## declaration -- a backend's relays open kernel objects sized by
    ## `ioLanes()`, so `initPool` has to have run first. The fields really are
    ## nil for that window, and the ring's own ordering (`gReactorState`, which
    ## is published with a release store once the whole ring is up) is what
    ## keeps anyone from reading them in it.
    poll*: unchecked proc (timeoutMs: int): bool {.nimcall.}
    waits*: bool
      ## True when `poll(timeoutMs)` really does sleep for up to `timeoutMs`
      ## in the kernel. The pool's idle worker naps for a millisecond after a
      ## fruitless poll, which is the right thing to do behind a backend that
      ## only peeks and the wrong thing behind one that already waited — it
      ## doubles the idle latency and spends a second syscall doing it. A
      ## backend that cannot wait (or, for io_uring, is on a kernel without
      ## `FEAT_EXT_ARG`) leaves this `false` and keeps the nap.
    close*: unchecked proc () {.nimcall.}
    forgetFd*: nil proc (fd: cint) {.nimcall.}
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
    q.init(InitialOpQueue)

var gSlots*: seq[SlotArena]
proc initSlots*() =
  gSlots = newSeq[SlotArena](ioLanes())
  for s in gSlots:
    s.init()

# ------------------------------------------------------------- deadlines ---
#
# Every op carries a deadline, so a per-lane min-heap of them answers two
# questions the poll loop needs: how long it may wait, and which ops have run
# out of time. Both in O(1) and O(log n); scanning the slots would be
# O(ops in flight) on every single poll.
#
# An op's entry leaves the heap when the op completes (`disarmDeadline`), so
# the heap holds exactly the in-flight ops that can expire. An op with a
# `never` deadline has no entry at all.

type
  TimerEntry* = object
    at*: Deadline
    slot* {.cursor.}: Slot

  TimerHeap* = object
    a*: seq[TimerEntry]

proc len*(h: TimerHeap): int {.inline.} = h.a.len

proc put(h: var TimerHeap; i: int; e: TimerEntry) {.inline.} =
  h.a[i] = e
  e.slot.timerPos = i

proc siftUp(h: var TimerHeap; i: int) =
  var i = i
  let e = h.a[i]
  while i > 0:
    let parent = (i - 1) div 2
    if h.a[parent].at <= e.at: break
    let up = h.a[parent]
    h.put(i, up)
    i = parent
  h.put(i, e)

proc siftDown(h: var TimerHeap; i: int) =
  var i = i
  let e = h.a[i]
  while true:
    let l = 2 * i + 1
    if l >= h.a.len: break
    var small = l
    if l + 1 < h.a.len and h.a[l + 1].at < h.a[l].at: small = l + 1
    if e.at <= h.a[small].at: break
    let down = h.a[small]
    h.put(i, down)
    i = small
  h.put(i, e)

proc push*(h: var TimerHeap; e: TimerEntry) =
  h.a.add e
  h.siftUp(h.a.len - 1)

proc remove*(h: var TimerHeap; i: int) =
  ## Remove the entry at `i`.
  h.a[i].slot.timerPos = -1
  let last = h.a.len - 1
  if i != last:
    let moved = h.a[last]
    h.put(i, moved)
  h.a.shrink last
  if i < h.a.len:
    h.siftDown(i)
    h.siftUp(i)

var gTimers*: seq[TimerHeap]
proc initTimers*() =
  gTimers = newSeq[TimerHeap](ioLanes())

proc armDeadline*(lane: int; s: Slot) =
  ## Record the deadline of the op just allocated into `s`. A `never`
  ## deadline arms nothing, so the heap holds only ops that can actually
  ## expire.
  let d = s.op.deadline
  if d == never: return
  gTimers[lane].push TimerEntry(at: d, slot: s)

proc disarmDeadline(lane: int; s: Slot) {.inline.} =
  if s.timerPos >= 0:
    gTimers[lane].remove(s.timerPos)

proc nextDeadline*(lane: int): Deadline =
  ## The earliest deadline this lane is waiting on; `never` when there is
  ## nothing to wait for.
  result = if gTimers[lane].len > 0: gTimers[lane].a[0].at else: never

proc waitMillis*(lane: int; requested: int): int =
  ## How long the backend may actually block: what the caller asked for, or
  ## the time to the earliest deadline, whichever is sooner. This is what
  ## turns a fixed poll interval into "sleep exactly until something is due".
  let d = nextDeadline(lane)
  if d == never: return requested
  let ms = millisUntil(d, monoNow())
  result = if requested < 0 or ms < requested: ms else: requested

proc waitNanos*(lane: int; requestedMs: int): int64 =
  ## `waitMillis` in nanoseconds, for a backend whose wait is a `timespec`.
  ## `-1` is "no bound at all", which only happens when the caller asked for
  ## none AND the lane is waiting on nothing that can expire.
  let d = nextDeadline(lane)
  let byDeadline = if d == never: -1'i64 else: nanosUntil(d, monoNow())
  if requestedMs < 0: return byDeadline
  let byRequest = int64(requestedMs) * 1_000_000'i64
  result = if byDeadline < 0 or byRequest < byDeadline: byRequest else: byDeadline

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
  IoNoMemory* = -12
    ## Completion result for an op that could not get a slot: `ENOMEM`.

proc deliver(cont: Continuation; resPtr: int; seqnum: SeqNum; kind: IoOp;
             fd: cint; res: int) =
  ## Hand `res` to whoever waits for the op: its result pointer, then its
  ## continuation or, without one, the completion queue.
  if resPtr != 0:
    cast[ptr int](resPtr)[] = res
  if cont.fn != nil:
    submit(cont, int(fd))
  else:
    gCqLock.acquire()
    if gCqCount < CqSize:
      gCq[gCqTail] = IoCompletion(id: seqnum, op: kind, fd: fd, result: res)
      gCqTail = (gCqTail + 1) and (CqSize - 1)
      inc gCqCount
    gCqLock.release()

proc complete*(slot: Slot; res: int) =
  ## Deliver `res` for the op in `slot` and release the slot: the caller must
  ## not touch it afterwards.
  let lane = ioLane()
  disarmDeadline(lane, slot)
  case slot.op.kind
  of opAccept:
    # The peer out-parameter lives in the address payload of the op, so it is
    # only readable once the kind says it is there.
    let peerOut = slot.op.accept.peer
    if peerOut != nil and res >= 0:
      # Before `freeSlot`, which releases the op and the address the kernel
      # wrote with it. Only on success — a failed accept filled
      # nothing, and copying the storage anyway would hand the caller the last
      # peer to use this slot.
      peerOut[] = slot.op.accept.sockAddr
  of opRecvFrom:
    # Same shape: the datagram's source address, written by the kernel, handed
    # off only on success. See the `opAccept` arm.
    let peerOut = slot.op.recvfrom.peer
    if peerOut != nil and res >= 0:
      peerOut[] = slot.op.recvfrom.sockAddr
  else:
    discard
  let cont  = slot.op.cont
  let resPtr = slot.op.res
  let fd    = slot.op.fd
  let seqnum = slot.op.seqnum
  let kind  = slot.op.kind
  gSlots[lane].freeSlot(slot)
  deliver(cont, resPtr, seqnum, kind, fd, res)

proc newSlot*(lane: int; op: OpContext): nil Slot =
  ## Allocate the slot for a dequeued `op`. When there is no memory for one,
  ## the op completes right here with `IoNoMemory` and the result is `nil`:
  ## the caller skips it.
  result = gSlots[lane].allocSlot(op)
  if result == nil:
    deliver(op.cont, op.res, op.seqnum, op.kind, op.fd, IoNoMemory)

var gCancelInFlight*: nil proc (s: Slot): bool {.nimcall.}
  ## Set by a backend where the kernel keeps working on an op after this
  ## process has stopped waiting for it (io_uring, IOCP). Asks the kernel for
  ## the op back and answers whether the kernel owns it; if so the op is
  ## completed only when the kernel reports it (`completeFromKernel`), because
  ## until then it may still write into the slot and the caller's buffer. The
  ## readiness backends leave it `nil`: a registration owns nothing, so
  ## dropping it is the whole of cancelling.

proc cancelOp*(lane: int; s: Slot; res: int) =
  ## Stop waiting for the op in `s`: complete it with `res` now, or, when the
  ## kernel still owns it, as soon as the kernel gives it back.
  disarmDeadline(lane, s)
  if s.cancelRes != 0: return          # already being taken back
  if gCancelInFlight != nil and s.op.kind != opTimeout and
      gCancelInFlight(s):
    s.cancelRes = res
  else:
    complete(s, res)

proc completeFromKernel*(s: Slot; res: int) =
  ## Complete an op the kernel has just reported. If the ring had already
  ## given up on it, a failure is reported as the reason it gave up; a success
  ## is still a success, since the transfer happened and must not be lost.
  let c = s.cancelRes
  complete(s, if c != 0 and res < 0: c else: res)

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
  while gTimers[lane].len > 0 and gTimers[lane].a[0].at <= now:
    let s = gTimers[lane].a[0].slot
    # A timer op reaching its deadline is a success — that is the whole point
    # of it. Anything else has run out of time. `cancelOp` removes the entry.
    cancelOp(lane, s, if s.op.kind == opTimeout: 0 else: IoTimedOut)
