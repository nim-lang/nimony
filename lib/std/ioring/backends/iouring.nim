# Linux io_uring backend.
# Uses the existing Queue from lib/std/posix/io_uring.nim.
# Does not use PollBackend since io_uring uses its own submission/completion
# queue model.
#
# Submissions are deferred to a shared queue so that the calling thread
# (e.g. main) never owns an SQE — every SQE is filled and flushed on the
# thread that calls poll(), which is always a worker thread (or whomever
# calls waitCompletions()). That avoids the "submitted on main, never
# polled" hang.

import std/[assertions, atomics, posix/posix, ticketlocks]
import std/syncio
import ../../posix/io_uring
import ../core/types
import ../core/slots
import ../core/backend
from ./epoll import initEpollBackendRelays

const
  DeferredSize = 4096 ## Must be a power of 2.
  DrainBatch   = 128  ## Max deferred entries drained per poll() call.

type
  DeferredEntry = object
    slotIdx: int
    fd: cint

  DeferredQueue = object
    lock: TicketLock
    head, tail, count: int
    data: array[DeferredSize, DeferredEntry]

var
  sqEntries: int
  deferred: DeferredQueue
  localQueue {.threadvar.}: Queue

proc tryInitLocalQueue(): bool =
  if localQueue.params == nil:
    try:
      localQueue = newQueue(sqEntries)
    except ErrorCode as e:
      stderr.writeLine("ioring: failed to init io_uring queue: " & $e)
      return false
  return true

proc fillSqe(sqe: ptr Sqe; op: ptr OpContext) {.inline.} =
  case op.kind
  of opRead:
    if op.buf != nil:
      discard sqe.read(op.fd, cast[pointer](op.buf), op.len)
  of opWrite:
    if op.buf != nil:
      discard sqe.write(op.fd, cast[pointer](op.buf), op.len)
  of opAccept:
    discard sqe.accept(SocketHandle(op.fd), cast[ptr SockAddr](addr op.acceptAddr), addr op.acceptLen, 0)

proc iouringSubmit(slotIdx: int; op: ptr OpContext) {.nimcall.} =
  # Enqueue onto the shared deferred queue so that poll() — which runs on
  # worker threads (or whatever thread calls waitCompletions) — fills the
  # SQE into its own thread-local io_uring instance and flushes it.
  #
  # Overflow spins rather than calling poll(0) inline: poll(0) would fill
  # the SQE on the calling (e.g. main) thread — whose ring is never
  # polled for completions afterwards. Spin-waiting instead lets a worker
  # drain the queue and own every SQE ↔ CQE pair in its own ring.
  while true:
    acquire(deferred.lock)
    if deferred.count < DeferredSize:
      deferred.data[deferred.tail] = DeferredEntry(slotIdx: slotIdx, fd: op.fd)
      deferred.tail = (deferred.tail + 1) and (DeferredSize - 1)
      inc deferred.count
      release(deferred.lock)
      return
    release(deferred.lock)
    cpuRelax()

proc iouringPoll(timeoutMs: int): bool {.nimcall.} =
  # Drain the shared deferred queue: for every pending slot, fill a fresh
  # SQE in THIS thread's io_uring instance. Only worker threads (and
  # callers of waitCompletions) poll, so all SQEs are always submitted
  # from within the poll loop that also reads their CQEs.
  #
  # Drain is bounded (DrainBatch) so a flood of submissions cannot keep a
  # worker inside poll() forever — the outer worker loop also runs task
  # draining, and remaining deferred entries are picked up next iteration.
  discard tryInitLocalQueue()
  var drained = 0
  while drained < DrainBatch:
    acquire(deferred.lock)
    if deferred.count == 0:
      release(deferred.lock)
      break
    let entry = deferred.data[deferred.head]
    deferred.head = (deferred.head + 1) and (DeferredSize - 1)
    dec deferred.count
    release(deferred.lock)
    inc drained
    let op = gSlots.addrSlot(entry.slotIdx)
    if op.inUse and op.fd == entry.fd:
      var sqe: nil ptr Sqe
      try:
        sqe = localQueue.getSqe()
      except ErrorCode as e:
        stderr.writeLine("ioring: failed to get sqe: " & $e)
        break
      if sqe == nil:
        break
      sqe.userData = cast[pointer](uint(entry.slotIdx))
      fillSqe(sqe, op)
  try:
    discard localQueue.submit()
  except ErrorCode as e:
    quit "fatal: bug: submit cannot fail: " & $e
  let waitNr = if timeoutMs > 0: 1'u else: 0'u
  const batchSize = 64
  var cqes = newSeq[Cqe](batchSize)
  var n: int = 0
  try:
    n = localQueue.copyCqes(cqes, waitNr)
  except ErrorCode as e:
    quit "fatal: bug: copyCqes cannot fail: " & $e
  if n <= 0:
    return false
  let a = gSlots
  for i in 0..<n:
    let slotIdx = int(cqes[i].userData)
    if slotIdx >= 0 and slotIdx < MaxOps and a.slots[slotIdx].inUse:
      complete(slotIdx, int(cqes[i].res))
  return true

proc iouringForgetFd(fd: cint) {.nimcall.} =
  # Remove pending deferred entries for `fd` from the shared queue so they
  # are never submitted against a closed (or worse, recycled) fd number.
  # We cannot issue a kernel cancel because the io_uring instance is
  # thread-local to whatever thread calls poll() — a cancel SQE submitted
  # here would target a different ring and never see the actual ops.
  # Already-submitted ops complete with whatever error the kernel returns
  # after close(2) (typically -EBADF); poll() silently skips their CQEs
  # because cancelAllForFd already freed those slots.
  acquire(deferred.lock)
  var readIdx = deferred.head
  var writeIdx = deferred.head
  var remaining = deferred.count
  while remaining > 0:
    let entry = deferred.data[readIdx]
    if entry.fd != fd:
      if writeIdx != readIdx:
        deferred.data[writeIdx] = entry
      writeIdx = (writeIdx + 1) and (DeferredSize - 1)
    else:
      dec deferred.count
    readIdx = (readIdx + 1) and (DeferredSize - 1)
    dec remaining
  deferred.tail = writeIdx
  release(deferred.lock)

proc iouringClose() {.nimcall.} =
  if localQueue.params != nil:
    teardown(localQueue)

proc initIoUringBackendRelays*(sqE = 256): BackendRelays =
  sqEntries = sqE
  if not tryInitLocalQueue():
    return initEpollBackendRelays()
  result = BackendRelays(
    submit: iouringSubmit,
    poll: iouringPoll,
    close: iouringClose,
    forgetFd: iouringForgetFd,
  )
