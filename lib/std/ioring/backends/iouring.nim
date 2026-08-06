# Linux io_uring backend.
# Uses the existing Queue from lib/std/posix/io_uring.nim.
# Extends Backend directly (not PollBackend) since io_uring uses
# its own submission/completion queue model.
#
# Submissions are deferred to a shared per-backend queue so that the
# calling thread (e.g. main) never owns an SQE — every SQE is filled
# and flushed on the thread that calls poll(), which is always a worker
# thread (or whomever calls waitCompletions()). That avoids the
# "submitted on main, never polled" hang.

import std/[assertions, atomics, posix/posix, ticketlocks]
import std/syncio
import ../../posix/io_uring
import ../core/types
import ../core/slots
import ../core/backend
from ./epoll import initEpollBackend

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

  IoUringBackend* = ref object of Backend
    sqEntries: int
    deferred*: DeferredQueue

var localQueue {.threadvar.}: Queue

proc tryInitLocalQueue(b: IoUringBackend): bool =
  if localQueue.params == nil:
    try:
      localQueue = newQueue(b.sqEntries)
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

method submit*(b: IoUringBackend; slotIdx: int; op: ptr OpContext) =
  # Enqueue onto the shared deferred queue so that poll() — which runs on
  # worker threads (or whatever thread calls waitCompletions) — fills the
  # SQE into its own thread-local io_uring instance and flushes it.
  #
  # Overflow spins rather than calling poll(0) inline: poll(0) would fill
  # the SQE on the calling (e.g. main) thread — whose ring is never
  # polled for completions afterwards. Spin-waiting instead lets a worker
  # drain the queue and own every SQE ↔ CQE pair in its own ring.
  while true:
    acquire(b.deferred.lock)
    if b.deferred.count < DeferredSize:
      b.deferred.data[b.deferred.tail] = DeferredEntry(slotIdx: slotIdx, fd: op.fd)
      b.deferred.tail = (b.deferred.tail + 1) and (DeferredSize - 1)
      inc b.deferred.count
      release(b.deferred.lock)
      return
    release(b.deferred.lock)
    cpuRelax()

method poll*(b: IoUringBackend; timeoutMs: int): bool =
  discard b.tryInitLocalQueue()
  # Drain the shared deferred queue: for every pending slot, fill a fresh
  # SQE in THIS thread's io_uring instance. Only worker threads (and
  # callers of waitCompletions) poll, so all SQEs are always submitted
  # from within the poll loop that also reads their CQEs.
  #
  # Drain is bounded (DrainBatch) so a flood of submissions cannot keep a
  # worker inside poll() forever — the outer worker loop also runs task
  # draining, and remaining deferred entries are picked up next iteration.
  var drained = 0
  while drained < DrainBatch:
    acquire(b.deferred.lock)
    if b.deferred.count == 0:
      release(b.deferred.lock)
      break
    let entry = b.deferred.data[b.deferred.head]
    b.deferred.head = (b.deferred.head + 1) and (DeferredSize - 1)
    dec b.deferred.count
    release(b.deferred.lock)
    inc drained
    let op = b.ring.slots.addrSlot(entry.slotIdx)
    if not op.inUse or op.fd != entry.fd:
      continue
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
  let a = b.ring.slots
  for i in 0..<n:
    let slotIdx = int(cqes[i].userData)
    if slotIdx >= 0 and slotIdx < MaxOps and a.slots[slotIdx].inUse:
      b.ring.complete(slotIdx, int(cqes[i].res))
  return true

method forgetFd*(b: IoUringBackend; fd: cint) =
  # Remove pending deferred entries for `fd` from the shared queue so they
  # are never submitted against a closed (or worse, recycled) fd number.
  # We cannot issue a kernel cancel because the io_uring instance is
  # thread-local to whatever thread calls poll() — a cancel SQE submitted
  # here would target a different ring and never see the actual ops.
  # Already-submitted ops complete with whatever error the kernel returns
  # after close(2) (typically -EBADF); poll() silently skips their CQEs
  # because cancelAllForFd already freed those slots.
  acquire(b.deferred.lock)
  var readIdx = b.deferred.head
  var writeIdx = b.deferred.head
  var remaining = b.deferred.count
  while remaining > 0:
    let entry = b.deferred.data[readIdx]
    if entry.fd != fd:
      if writeIdx != readIdx:
        b.deferred.data[writeIdx] = entry
      writeIdx = (writeIdx + 1) and (DeferredSize - 1)
    else:
      dec b.deferred.count
    readIdx = (readIdx + 1) and (DeferredSize - 1)
    dec remaining
  b.deferred.tail = writeIdx
  release(b.deferred.lock)

method close*(b: IoUringBackend) =
  if localQueue.params != nil:
    teardown(localQueue)

proc initIoUringBackend*(ring: Ring; sqEntries = 256): Backend =
  var backend = IoUringBackend(sqEntries: sqEntries)
  result = backend
  result.ring = ring
  if not tryInitLocalQueue(backend):
    # fallback to epoll
    result = initEpollBackend(ring)
