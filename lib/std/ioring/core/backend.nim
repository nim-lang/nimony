# Backend type with backreferences to IoRing and SlotArena.
# Groups Backend, IoRing, and IoPool in one module so backends
# can use the proper Ring type for their backreference.

import ./types
import ./slots
import std/[threadpool, ticketlocks]

const CqSize* = 4096

type
  Backend* = ref object of RootObj
    ring*: Ring

  Ring* = ref object of RootObj
    slots*: SlotArena
    nextSeq*: SeqNum
    backend*: Backend
    pool*: Pool
    cqLock*: TicketLock
    cq*: seq[IoCompletion]
    cqHead*, cqTail*, cqCount*: int
    closed*: bool # accessed atomically; set once by shutdown()

method complete*(b: Backend; slotIdx: int; res: int) {.base.} =
  discard

method submit*(b: Backend; slotIdx: int; op: ptr OpContext) {.base.} =
  discard

method poll*(b: Backend; timeoutMs: int): bool {.base.} =
  false

method close*(b: Backend) {.base.} =
  discard

method forgetFd*(b: Backend; fd: cint) {.base.} =
  ## Drop any backend-side per-fd registration/bookkeeping before a fd is
  ## closed (e.g. epoll's ADD/MOD tracking). `nil` for backends where the
  ## OS already tears this down on close (kqueue) — callers
  ## must nil-check before calling.
  discard

method complete*(ring: Ring; slotIdx: int; res: int) =
  let slot = addr ring.slots.slots[slotIdx]
  if slot.res != 0:
    cast[ptr int](slot.res)[] = res
  let cont = slot.cont
  slot.cont = Continuation(fn: nil, env: nil)
  ring.slots.freeSlot(slotIdx)
  if cont.fn != nil:
    ring.pool.submit(cont, int(slot.fd))
  else:
    ring.cqLock.acquire()
    if ring.cqCount < CqSize:
      ring.cq[ring.cqTail] = IoCompletion(id: slot.seqnum, op: slot.kind, fd: slot.fd, result: res)
      ring.cqTail = (ring.cqTail + 1) and (CqSize - 1)
      inc ring.cqCount
    ring.cqLock.release()