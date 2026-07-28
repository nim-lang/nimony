# Linux io_uring backend.
# Uses the existing Queue from lib/std/posix/io_uring.nim.
# Extends Backend directly (not PollBackend) since io_uring uses
# its own submission/completion queue model.

import std/[assertions, posix/posix]
import ../../posix/io_uring
import ../core/types
import ../core/slots
import ../core/backend
from ./epoll import initEpollBackend

type IoUringBackend* = ref object of Backend
  sqEntries: int

var localQueue {.threadvar.}: Queue

proc tryInitLocalQueue(b: IoUringBackend) =
  if localQueue.params == nil:
    try:
      localQueue = newQueue(b.sqEntries)
    except:
      return

proc iouringSubmit(b: Backend; slotIdx: int; op: ptr OpContext) {.nimcall.} =
  let self = IoUringBackend(b)
  self.tryInitLocalQueue()
  var sqe: nil ptr Sqe
  try:
    sqe = localQueue.getSqe()
    if sqe == nil:
      discard localQueue.submit()
      sqe = localQueue.getSqe()
  except:
    return
  if sqe == nil:
    return
  sqe.userData = cast[pointer](uint(slotIdx))
  case op.kind
  of opRead:
    if op.buf != nil:
      discard sqe.read(op.fd, cast[pointer](op.buf), op.len)
  of opWrite:
    if op.buf != nil:
      discard sqe.write(op.fd, cast[pointer](op.buf), op.len)
  of opAccept:
    discard sqe.accept(SocketHandle(op.fd), cast[ptr SockAddr](addr op.acceptAddr), addr op.acceptLen, 0)

proc iouringPoll(b: Backend; timeoutMs: int): bool {.nimcall.} =
  let self = IoUringBackend(b)
  self.tryInitLocalQueue()
  try:
    discard localQueue.submit()
  except:
    discard
  let waitNr = if timeoutMs > 0: 1'u else: 0'u
  const batchSize = 64
  var cqes = newSeq[Cqe](batchSize)
  var n: int = 0
  try:
    n = localQueue.copyCqes(cqes, waitNr)
  except:
    discard
  if n <= 0:
    return false
  let a = b.arena
  for i in 0..<n:
    let slotIdx = int(cqes[i].userData)
    if slotIdx >= 0 and slotIdx < MaxOps and a.slots[slotIdx].inUse:
      if b.completeFn != nil:
        b.completeFn(b.ring, slotIdx, int(cqes[i].res))
  return true

proc iouringClose(b: Backend) {.nimcall.} = discard

proc initIoUringBackend*(arena: SlotArena; ring: Ring; sqEntries = 256): Backend =
  try:
    localQueue = newQueue(sqEntries)
    result = IoUringBackend(sqEntries: sqEntries)
    result.arena = arena
    result.ring = ring
    result.submitFn = iouringSubmit
    result.pollFn = iouringPoll
    result.closeFn = iouringClose
  except:
    # fallback to epoll
    result = initEpollBackend(arena, ring)
