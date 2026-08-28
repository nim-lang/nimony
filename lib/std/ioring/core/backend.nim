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

var gOpQueues*: seq[FifoStripe[OpContext]]
proc initOpQueues*() =
  gOpQueues = newSeq[FifoStripe[OpContext]](workerCount)
  for q in gOpQueues:
    q.init(MaxOps)

var gSlots*: seq[SlotArena]
proc initSlots*() =
  gSlots = newSeq[SlotArena](workerCount)
  for s in gSlots:
    s.init(MaxOps)

var
  gNextSeq*: SeqNum
  gCqLock*: TicketLock
  gCq*: seq[IoCompletion]
  gCqHead*: int
  gCqTail*: int
  gCqCount*: int
  gClosed*: bool

proc complete*(slotIdx: int; res: int) =
  let slot = addr gSlots[threadIdx].slots[slotIdx]
  if slot.op.res != 0:
    cast[ptr int](slot.op.res)[] = res
  let cont  = slot.op.cont
  let fd    = slot.op.fd
  let seqnum = slot.op.seqnum
  let kind  = slot.op.kind
  gSlots[threadIdx].freeSlot(slotIdx)
  if cont.fn != nil:
    submit(cont, int(fd))
  else:
    gCqLock.acquire()
    if gCqCount < CqSize:
      gCq[gCqTail] = IoCompletion(id: seqnum, op: kind, fd: fd, result: res)
      gCqTail = (gCqTail + 1) and (CqSize - 1)
      inc gCqCount
    gCqLock.release()
