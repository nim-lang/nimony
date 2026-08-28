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

var
  gNextSeq*: SeqNum
  gCqLock*: TicketLock
  gCq*: seq[IoCompletion]
  gCqHead*: int
  gCqTail*: int
  gCqCount*: int

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
