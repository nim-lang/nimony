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
  CancelUserData = 0xffff_ffff_ffff_ffff'u64
    ## `user_data` for the cancellation SQEs this backend submits on its own
    ## behalf. Its low word is not a slot index any arena can hold, so the
    ## completion loop drops the CQE on the bounds check below and no slot is
    ## touched by it.

proc tagFor(idx: int; gen: uint32): uint64 {.inline.} =
  ## A CQE's `user_data`: the slot index, and the generation of the op that was
  ## in it when the SQE was filled.
  uint64(uint32(idx)) or (uint64(gen) shl 32)

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
      sqe.userData = cast[pointer](tagFor(idx, gSlots[lane].slots[idx].gen))
      # Fill from the ARENA copy, never from `buf`: an accept SQE stores
      # `addr op.sockAddr`/`addr op.sockAddrLen` and the kernel writes through
      # those at completion time, long after this stack frame is gone.
      fillSqe(sqe, addr gSlots[lane].slots[idx].op)
  # Sleep in the kernel until something is due — an I/O completion, or the
  # earliest deadline this lane is waiting on, whichever comes first. That is
  # ONE timeout for the whole lane, taken off the top of the deadline heap,
  # rather than one timeout per op: a `link_timeout` on every SQE would buy the
  # same wakeup for a second submission and a second completion each, and a
  # server holding an idle deadline per connection would spend most of its ring
  # on timers. The readiness backends bound their wait exactly this way
  # (`epoll_wait(waitMs)`), so all three answer "how long may I sleep" from the
  # same heap.
  #
  # The wait is still bounded from above by what the caller asked for, because
  # nothing wakes a worker when a TASK is enqueued — `threadpool.submit` only
  # enqueues — so a lane may not sleep past its next look at the run queue.
  # Give the pool a wakeup and this bound can go, and then the ring sleeps
  # until there is genuinely something to do.
  #
  # Before this the ring never entered the kernel to wait at all: the bound was
  # computed and discarded, and an idle worker `nanosleep`t a millisecond with
  # its eyes shut, so a completion arriving 10us in was noticed 990us late.
  let blocking = timeoutMs != 0 and localQueues[lane].cqReady == 0 and
                 localQueues[lane].hasExtArg
  if blocking:
    let ns = waitNanos(lane, timeoutMs)
    var ts = Timespec(tv_sec: Time(ns div 1_000_000_000'i64),
                      tv_nsec: clong(ns mod 1_000_000_000'i64))
    var tsp: nil ptr Timespec = nil
    if ns >= 0: tsp = addr ts
    try:
      discard localQueues[lane].submitAndWait(1, tsp)
    except ErrorCode as e:
      quit "fatal: bug: submit and wait cannot fail: " & $e
  elif n > 0:
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
        # A slot is recycled the instant its op completes, and an op can
        # complete *here* while the kernel is still working on it: a blown
        # deadline expires it from the lane's heap, and `closeFd` cancels it.
        # The CQE that turns up afterwards then names a slot that holds an
        # unrelated op. Applying it completed that op with the wrong result and
        # — worse — freed a slot that was already free, so its index went onto
        # the freelist twice and two later ops shared one slot. One of them
        # could never complete, which is how the ring came to hang instead of
        # merely misreporting. The generation in `user_data` says which op the
        # kernel is talking about; anything else is already accounted for.
        let idx = int(cqes[i].userData and 0xffff_ffff'u64)
        let gen = uint32(cqes[i].userData shr 32)
        if idx >= gSlots[lane].slots.len: continue
        if not gSlots[lane].slots[idx].inUse or
           gSlots[lane].slots[idx].gen != gen: continue
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

proc iouringCancelInFlight(slotIdx: int; gen: uint32) {.nimcall.} =
  ## The lane's deadline heap is about to complete this op locally, but the
  ## kernel does not know that and still owns the op's buffer. Match the op by
  ## the `user_data` its SQE carries and ask for it back. Failing to get an SQE
  ## is not fatal: the CQE that eventually arrives is stale and dropped, and
  ## the only cost is that the kernel held the buffer longer than we wanted.
  let lane = ioLane()
  if lane >= localQueues.len: return
  var sqe: nil ptr Sqe
  try:
    sqe = localQueues[lane].getSqe()
  except ErrorCode:
    return
  if sqe == nil: return
  discard sqe.cancel(tagFor(slotIdx, gen))
  sqe.userData = cast[pointer](CancelUserData)
  # Submitted here rather than left for the next `submit` in the poll loop:
  # this runs from `expireDeadlines`, which the loop reaches *after* its own
  # submit, and a deadline blowing is the slow path anyway.
  try:
    discard localQueues[lane].submit()
  except ErrorCode:
    discard

proc iouringForgetFd(fd: cint) {.nimcall.} =
  ## `closeFd` calls this before close(2). The readiness backends drop a
  ## registration here; io_uring has none to drop, but it does have ops the
  ## kernel is still working on — and `closeFd` is about to free their slots
  ## locally. Until the kernel acknowledges, it still owns each op's buffer and
  ## may write through it, so ask it to give them back now rather than at some
  ## unpredictable later point. `cancelFd` is the one-SQE form of exactly that
  ## question. Whatever CQEs still arrive for those ops are stale by then, and
  ## the generation check in `iouringPoll` drops them.
  let lane = ioLane()
  if lane >= localQueues.len: return
  if not gSlots[lane].hasPendingForFd(fd): return
  var sqe: nil ptr Sqe
  try:
    sqe = localQueues[lane].getSqe()
  except ErrorCode:
    return
  if sqe == nil: return
  discard sqe.cancelFd(FileHandle(fd))
  sqe.userData = cast[pointer](CancelUserData)
  try:
    discard localQueues[lane].submit()
  except ErrorCode:
    discard

proc iouringClose() {.nimcall.} =
  # By index, and `var`: a `Queue` owns an fd and three mappings, so tearing
  # down a copy would leave the original for the seq's own destructor to tear
  # down a second time — including a second `close(2)` on a number the OS may
  # have handed to somebody else by then.
  for i in 0..<localQueues.len:
    if localQueues[i].params != nil:
      teardown(localQueues[i])

proc initIoUringBackendRelays*(sqE = 256): BackendRelays =
  sqEntries = sqE
  if not tryInitLocalQueues():
    return initEpollBackendRelays()
  gCancelInFlight = iouringCancelInFlight
  result = BackendRelays(
    poll: iouringPoll,
    waits: localQueues[0].hasExtArg,
    close: iouringClose,
    forgetFd: iouringForgetFd,
  )
