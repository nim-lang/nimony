# Relay-based backend dispatch.
# Groups BackendRelays and global ring state so backends can access
# the completion queue, slot arena, and pool.

import ./types
import ./slots
import std/[threadpool, ticketlocks]

const CqSize* = 4096

type
  BackendRelays* = object
    submit*: proc (slotIdx: int; op: ptr OpContext) {.nimcall.}
    poll*: proc (timeoutMs: int): bool {.nimcall.}
    close*: proc () {.nimcall.}
    forgetFd*: proc (fd: cint) {.nimcall.}
      ## Drop any backend-side per-fd registration/bookkeeping before a fd is
      ## closed (e.g. epoll's ADD/MOD tracking). `nil` for backends where the
      ## OS already tears this down on close (kqueue) — callers
      ## must nil-check before calling.
      
var
  gSlots*: SlotArena
  gNextSeq*: SeqNum
  gCqLock*: TicketLock
  gCq*: seq[IoCompletion]
  gCqHead*: int
  gCqTail*: int
  gCqCount*: int
  gClosed*: bool

proc complete*(slotIdx: int; res: int) =
  let slot = addr gSlots.slots[slotIdx]
  if slot.res != 0:
    cast[ptr int](slot.res)[] = res
  let cont = slot.cont
  slot.cont = Continuation(fn: nil, env: nil)
  gSlots.freeSlot(slotIdx)
  if cont.fn != nil:
    submit(cont, int(slot.fd))
  else:
    gCqLock.acquire()
    if gCqCount < CqSize:
      gCq[gCqTail] = IoCompletion(id: slot.seqnum, op: slot.kind, fd: slot.fd, result: res)
      gCqTail = (gCqTail + 1) and (CqSize - 1)
      inc gCqCount
    gCqLock.release()
