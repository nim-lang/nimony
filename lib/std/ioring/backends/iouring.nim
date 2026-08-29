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
  inFlight: seq[int]
    ## Ops submitted on this lane and not yet completed. Owned by the lane's own
    ## thread — `iouringPoll` is the only writer and is the only place either
    ## end of an op's life is observed.
    ##
    ## It exists to answer "can a completion possibly arrive?", which the ring
    ## itself cannot: `sqReady` counts SQEs the KERNEL has yet to consume, and
    ## drops to zero the moment it does, while the op is still outstanding.
    ## Reading it instead would skip the wait exactly when there is work to wait
    ## for. Drift is one-directional by construction — an op that is cancelled
    ## without a CQE leaves the count high, which costs a syscall and never a
    ## missed completion.

proc tryInitLocalQueues(): bool =
  localQueues = @[]
  inFlight = newSeq[int](ioLanes())
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
    discard sqe.accept(SocketHandle(op.fd), cast[ptr SockAddr](addr op.acceptAddr), addr op.acceptLen, 0)
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
      sqe.userData = cast[pointer](uint(idx))
      # Fill from the ARENA copy, never from `buf`: an accept SQE stores
      # `addr op.acceptAddr`/`addr op.acceptLen` and the kernel writes through
      # those at completion time, long after this stack frame is gone.
      fillSqe(sqe, addr gSlots[lane].slots[idx].op)
      inFlight[lane] = inFlight[lane] + 1
  # ONE enter per poll: flush whatever was just filled and, when the caller gave
  # us a time budget, wait for a completion inside the same syscall.
  #
  # Waiting here is the whole point. Peeking at the CQ and returning empty
  # leaves the caller to spend its budget in `nanosleep` (see
  # `threadpool.workerLoop`) — a sleep no completion can interrupt, so every
  # completion picks up latency bounded by the poll interval rather than being
  # delivered when the kernel has it. epoll never had that problem because its
  # wait IS `epoll_wait(timeoutMs)`.
  #
  # Not conditional on having just filled SQEs: with nothing new to submit there
  # is still a budget to spend waiting on work already in flight. But with
  # NOTHING in flight either, no completion can arrive, so the enter is a
  # syscall that can only ever time out — skip it and let the caller idle in its
  # own sleep, which is the pre-io_uring behaviour for a lane with no work.
  var waited = false
  if inFlight[lane] > 0:
    waited = timeoutMs > 0 and localQueues[lane].canTimedWait
    let waitNr = if waited: 1'u else: 0'u
    discard localQueues[lane].submitAndWait(waitNr,
                                            timeoutMs.int64 * 1_000_000'i64)
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
      inFlight[lane] = inFlight[lane] - n
      return true
  # Nothing completed — but if the wait above happened, the caller's timeout has
  # ALREADY been spent, in the kernel, where a completion could have cut it
  # short. Saying `false` here would send it into `nanosleep` for the same
  # interval a second time: twice the idle latency, and the second half
  # uninterruptible, which is the exact cost this backend just stopped paying.
  # Hence the relay's contract is "the budget is spent", not "an event fired".
  return waited

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
