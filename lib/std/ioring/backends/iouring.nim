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
import std/syncio   # quit
import ../../posix/io_uring
import ../core/types
import ../core/slots
import ../core/backend
from ./epoll import initEpollBackendRelays

const
  DrainBatch = 128  ## Max deferred entries drained per poll() call.

var
  sqEntries: int
  localQueues: seq[Queue]

proc tryInitLocalQueues(): bool =
  localQueues = @[]
  try:
    for i in 0..<ioLanes():
      localQueues.add newQueue(sqEntries)
  except ErrorCode:
    return false   # the caller falls back to the epoll backend
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
    discard sqe.accept(SocketHandle(op.fd), cast[ptr SockAddr](addr op.sockAddr), addr op.sockAddrLen, 0)
  of opPollAdd:
    # Single-shot readiness probe on the direction(s) the caller asked for;
    # completes with the fired poll mask, then the slot is freed so the caller
    # re-arms with a new submitPollAdd (matching the epoll/kqueue oneshot
    # behaviour). Watching both regardless would spin a read-waiter on a
    # writable socket — see submitPollAdd's docstring.
    # The kernel speaks poll(2) events, the ring speaks `IoEvents`; the two
    # never share a representation, so translate in both directions here and
    # in the completion loop below.
    var pollEvents: PollEvents = {}
    if evRead in op.pollMask: pollEvents.incl POLL_IN
    if evWrite in op.pollMask: pollEvents.incl POLL_OUT
    discard sqe.poll_add(op.fd, pollEvents)
  of opConnect:
    discard sqe.connect(SocketHandle(op.fd),
                        cast[ptr SockAddr](addr op.sockAddr), op.sockAddrLen)
  of opNop, opTimeout:
    # A timer needs no SQE. The lane's deadline heap already knows when it is
    # due and bounds the `submit(waitNr)` below, so letting the kernel hold a
    # second copy of the same deadline would only be a second thing to cancel.
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
      if buf[i].kind == opTimeout:
        # No SQE, but it still needs a slot so the heap can complete it.
        let idx = gSlots[lane].allocSlot(buf[i])
        armDeadline(lane, idx)
        continue
      var sqe: nil ptr Sqe
      try:
        sqe = localQueues[lane].getSqe()
      except ErrorCode:
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
      armDeadline(lane, idx)
      sqe.userData = cast[pointer](uint(idx))
      # Fill from the ARENA copy, never from `buf`: an accept SQE stores
      # `addr op.sockAddr`/`addr op.sockAddrLen` and the kernel writes through
      # those at completion time, long after this stack frame is gone.
      fillSqe(sqe, addr gSlots[lane].slots[idx].op)
    try:
      discard localQueues[lane].submit()
    except ErrorCode as e:
      quit "fatal: bug: submit cannot fail: " & $e
  # Bound the wait by the earliest deadline in this lane, the same way the
  # readiness backends bound theirs. io_uring can attach an absolute
  # `link_timeout` to an individual SQE, which is the better answer for
  # per-op deadlines and is the natural next step here; one lane-wide bound
  # keeps the two backends behaving identically in the meantime.
  discard waitMillis(lane, timeoutMs)
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
        # translate it to the same internal `IoEvents` the epoll/kqueue
        # backends report, so the completion's `readyEvents` are consistent no
        # matter which backend is in use.
        var res = int(cqes[i].res)
        if gSlots[lane].slots[idx].op.kind == opPollAdd:
          var fired = toPollEvents(uint32(res))
          var ev: IoEvents = {}
          if POLL_IN in fired: ev.incl evRead
          if POLL_OUT in fired: ev.incl evWrite
          res = toEventMask(ev)
        complete(idx, res)
      expireDeadlines(lane)
      return true
  expireDeadlines(lane)
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
