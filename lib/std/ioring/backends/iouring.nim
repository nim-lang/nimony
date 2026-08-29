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

import std/[assertions, atomics, posix/posix, ticketlocks, threadpool]
import std/syncio
import ../../posix/io_uring
import ../core/types
import ../core/slots
import ../core/backend
from ./epoll import initEpollBackendRelays

const
  DrainBatch = 128  ## Max deferred entries drained per poll() call.
  # Standard poll(2) mask bits used by io_uring's OP_POLL_ADD (distinct from
  # the internal EvRead/EvWrite flags, which never cross into the kernel).
  POLLIN = uint32(0x0001)
  POLLOUT = uint32(0x0004)

var
  sqEntries: int
  localQueues: seq[Queue]

proc tryInitLocalQueues(): bool =
  localQueues = @[]
  try:
    for i in 0..<ioLanes():
      localQueues.add newQueue(sqEntries)
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
  of opPollAdd:
    # Single-shot readiness probe on the direction(s) the caller asked for;
    # completes with the fired poll mask, then the slot is freed so the caller
    # re-arms with a new submitPollAdd (matching the epoll/kqueue oneshot
    # behaviour). Watching both regardless would spin a read-waiter on a
    # writable socket — see submitPollAdd's docstring.
    var pollFlags = 0'u32
    if (op.pollMask and EvRead) != 0: pollFlags = pollFlags or POLLIN
    if (op.pollMask and EvWrite) != 0: pollFlags = pollFlags or POLLOUT
    discard sqe.poll_add(op.fd, pollFlags)
  of opNop:
    discard sqe.nop()

proc iouringPoll(timeoutMs: int): bool {.nimcall.} =
  # Drain the shared deferred queue: for every pending slot, fill a fresh
  # SQE in THIS thread's io_uring instance. Only worker threads (and
  # callers of waitCompletions) poll, so all SQEs are always submitted
  # from within the poll loop that also reads their CQEs.
  #
  # Drain is bounded (DrainBatch) so a flood of submissions cannot keep a
  # worker inside poll() forever — the outer worker loop also runs task
  # draining, and remaining deferred entries are picked up next iteration.
  let lane = ioLane()
  var buf {.noinit.}: array[DrainBatch, OpContext]
  var n = gOpQueues[lane].tryBulkDequeue(DrainBatch, buf)
  if n > 0:
    for i in 0..<n:
      var sqe: nil ptr Sqe
      try:
        sqe = localQueues[lane].getSqe()
      except ErrorCode as e:
        stderr.writeLine("ioring: failed to get sqe: " & $e)
        # Ops buf[i..<n] were dequeued but never got an SQE/slot; put them
        # back so the next poll picks them up instead of losing them forever.
        for k in i..<n:
          discard gOpQueues[lane].tryEnqueue(buf[k])
        break
      if sqe == nil:
        for k in i..<n:
          discard gOpQueues[lane].tryEnqueue(buf[k])
        break
      let idx = gSlots[lane].allocSlot(buf[i])
      sqe.userData = cast[pointer](uint(idx))
      # Fill from the ARENA copy, never from `buf`: an accept SQE stores
      # `addr op.acceptAddr`/`addr op.acceptLen` and the kernel writes through
      # those at completion time, long after this stack frame is gone.
      fillSqe(sqe, addr gSlots[lane].slots[idx].op)
    try:
      discard localQueues[lane].submit()
    except ErrorCode as e:
      quit "fatal: bug: submit cannot fail: " & $e
  if localQueues[lane].cqReady > 0:
    var cqes {.noinit.}: array[DrainBatch, Cqe]
    try:
      n = localQueues[lane].copyCqes(cqes)
    except ErrorCode as e:
      quit "fatal: bug: copyCqes cannot fail: " & $e
    if n > 0:
      for i in 0..<n:
        let idx = int(cqes[i].userData)
        # For OP_POLL_ADD the kernel reports the fired mask in poll(2) form;
        # translate it to the same internal EvRead/EvWrite flags the epoll/
        # kqueue backends use, so the completion's `result` is consistent no
        # matter which backend is in use.
        var res = int(cqes[i].res)
        if gSlots[lane].slots[idx].op.kind == opPollAdd:
          var ev = 0
          if (uint32(res) and POLLIN) != 0:
            ev = ev or EvRead
          if (uint32(res) and POLLOUT) != 0:
            ev = ev or EvWrite
          res = ev
        complete(idx, res)
      return true
  return false

proc iouringForgetFd(fd: cint) {.nimcall.} =
  discard

proc iouringClose() {.nimcall.} =
  for q in localQueues:
    if q.params != nil:
      teardown(q)

proc initIoUringBackendRelays*(sqE = 256): BackendRelays =
  sqEntries = sqE
  if not tryInitLocalQueues():
    return initEpollBackendRelays()
  result = BackendRelays(
    poll: iouringPoll,
    close: iouringClose,
    forgetFd: iouringForgetFd,
  )
