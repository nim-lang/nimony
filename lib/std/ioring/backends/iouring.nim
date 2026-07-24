# Linux io_uring backend.
# Uses the existing Queue from lib/std/posix/io_uring.nim.
# Extends Backend directly (not PollBackend) since io_uring uses
# its own submission/completion queue model.

import std/[assertions, posix/posix]
import ../../posix/io_uring
import ../core/types
import ../core/slots
from ./epoll import initEpollBackend
import std/syncio

type IoUringBackend* = ref object of Backend
  arena: int
  sqEntries: int

var localQueue {.threadvar.}: Queue

proc initIoUringBackend*(arena: int; sqEntries = 256): Backend =
  try:
    localQueue = newQueue(sqEntries)
    result = IoUringBackend(arena: arena, sqEntries: sqEntries)
  except:
    # fallback to epoll
    result = initEpollBackend(arena)

proc tryInitLocalQueue(b: IoUringBackend) =
  if localQueue.params == nil:
    try:
      localQueue = newQueue(b.sqEntries)
    except:
      return

method submit*(b: IoUringBackend; slotIdx: int; op: ptr OpContext) =
  b.tryInitLocalQueue()
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

method poll*(b: IoUringBackend; timeoutMs: int): bool =
  b.tryInitLocalQueue()
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
  let a = cast[ptr SlotArena](b.arena)
  for i in 0..<n:
    let slotIdx = int(cqes[i].userData)
    if slotIdx >= 0 and slotIdx < MaxOps and a.slots[slotIdx].inUse:
      if b.completeFn != nil:
        b.completeFn(slotIdx, int(cqes[i].res), b.completeEnv)
  return true

method close*(b: IoUringBackend) =
  discard