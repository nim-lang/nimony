# Linux io_uring backend.
# Uses the existing Queue from lib/std/posix/io_uring.nim.
# Extends Backend directly (not PollBackend) since io_uring uses
# its own submission/completion queue model.

import std/[assertions, posix/posix]
import std/syncio
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
    quit "fatal: bug: submit cannot fail"
  let waitNr = if timeoutMs > 0: 1'u else: 0'u
  const batchSize = 64
  var cqes = newSeq[Cqe](batchSize)
  var n: int = 0
  try:
    n = localQueue.copyCqes(cqes, waitNr)
  except:
    quit "fatal: bug: copyCqes cannot fail"
  if n <= 0:
    return false
  let a = b.ring.slots
  for i in 0..<n:
    let slotIdx = int(cqes[i].userData)
    if slotIdx >= 0 and slotIdx < MaxOps and a.slots[slotIdx].inUse:
      b.ring.complete(slotIdx, int(cqes[i].res))
  return true

method forgetFd*(b: IoUringBackend; fd: cint) =
  ## Ask the kernel to cancel every in-flight op on `fd` before the arena
  ## frees the corresponding slots (see `ioring.closeFd`). Without this, a
  ## completion for an op the arena already freed/reused could land on the
  ## wrong (later) slot index once the fd number and the slot index are both
  ## recycled — best-effort: if there is no room for the cancel SQE right
  ## now we still proceed with the close, we just may leak that one slot
  ## until the kernel completion arrives naturally.
  b.tryInitLocalQueue()
  try:
    var sqe = localQueue.getSqe()
    if sqe == nil:
      discard localQueue.submit()
      sqe = localQueue.getSqe()
    if sqe != nil:
      discard sqe.cancelFd(FileHandle(fd))
      discard localQueue.submit()
  except:
    discard

method close*(b: IoUringBackend) =
  # Previously a no-op: the mapped SQ/CQ rings and the io_uring fd were never
  # released, leaking both per shutdown. Overwriting the threadvar with a
  # fresh (zero) `Queue` runs `=destroy` on the old value, which unmaps the
  # rings and closes the fd (see `posix/io_uring.=destroy`).
  if localQueue.params != nil:
    try:
      localQueue = newQueue(b.sqEntries)
    except:
      discard

proc initIoUringBackend*(ring: Ring; sqEntries = 256): Backend =
  try:
    localQueue = newQueue(sqEntries)
    result = IoUringBackend(sqEntries: sqEntries)
    result.ring = ring
  except:
    # fallback to epoll
    result = initEpollBackend(ring)
