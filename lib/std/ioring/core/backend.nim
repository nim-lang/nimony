# Backend type with backreferences to IoRing and SlotArena.
# Groups Backend, IoRing, and IoPool in one module so backends
# can use the proper Ring type for their backreference.

import ./types
import ./slots
import std/[threadpool, ticketlocks]

const CqSize* = 4096

type
  Backend* = ref object of RootObj
    arena*: SlotArena
    ring*: Ring
    completeFn*: proc(ring: Ring; slotIdx: int; res: int) {.nimcall.}
    submitFn*: proc(b: Backend; slotIdx: int; op: ptr OpContext) {.nimcall.}
    pollFn*: proc(b: Backend; timeoutMs: int): bool {.nimcall.}
    closeFn*: proc(b: Backend) {.nimcall.}

  Ring* = ref object of RootObj
    slots*: SlotArena
    nextSeq*: SeqNum
    backend*: Backend
    pool*: Pool
    cqLock*: TicketLock
    cq*: seq[IoCompletion]
    cqHead*, cqTail*, cqCount*: int


