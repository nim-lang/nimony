# Linux epoll backend.
# epoll_ctl ADD is only valid the *first* time a fd is registered; every
# subsequent (re-)arm on the same fd — including the EPOLLONESHOT re-arm
# after each event — must use MOD, or epoll_ctl fails with EEXIST and the
# fd is silently never re-armed again (a per-connection deadlock that is
# easy to miss under light testing). Whether a fd is already known to this
# epoll instance is derived from the slot arena (`hasPendingForFd`), so
# submit/re-arm pick the right verb; an ADD that loses that race falls back
# to MOD below.

import ../../posix/epoll
import ../../posix/posix

import ../core/types
import ../core/slots
import ../core/backend
import ./poll

const
  MaxIoEvents = 64
  DrainBatch = 128

var
  epollFds: seq[cint]

proc fdNotPollable(): bool {.inline.} =
  ## True when the epoll_ctl that just failed did so because the fd is no longer
  ## a live pollable descriptor we own: EPERM (a non-pollable type — a regular
  ## file, e.g. a socket fd closed by its transfer and its number reused by one
  ## of the process's file opens before this arm ran) or EBADF (already closed).
  ## Skipping such an fd is correct — it carries no real transfer, so not
  ## watching it can't stall one — and avoids error spam under multi-threaded
  ## handler resumption.
  ##
  ## Reads `errno`, not the return value: epoll_ctl reports *every* failure as
  ## -1 and puts the reason in errno, so comparing the result against EPERM/EBADF
  ## never matched and the fallback below ran (and printed) for every failure.
  ##
  ## `sysErrno`, not `posix.errno`: under `-d:nimNativeIo` the latter reads a
  ## variable that libc calls never touch, so this test silently took the
  ## wrong branch. See the note in `poll.nim`.
  let e = sysErrno()
  result = e == EPERM or e == EBADF

proc epollReArm(fd: cint; events: IoEvents, alreadyRegistered: bool): bool {.nimcall.} =
  let epollFd = epollFds[ioLane()]
  var ev {.noinit.}: EpollEvent
  ev.events = EPOLLONESHOT
  if evRead in events:
    ev.events = ev.events or EPOLLIN
  if evWrite in events:
    ev.events = ev.events or EPOLLOUT
  # Store the fd itself (not a slot index) as user data: a slot can be freed
  # and its index reused for a *different* fd between registration and the
  # event firing, which previously made `data.ptr` an unreliable — and
  # occasionally wrong — way to recover the fd on delivery.
  ev.data.`ptr` = cast[pointer](uint(fd))
  let op = if alreadyRegistered: EPOLL_CTL_MOD else: EPOLL_CTL_ADD
  var res = epoll_ctl(epollFd, op, fd, addr ev)
  if res != 0 and op == EPOLL_CTL_ADD and not fdNotPollable():
    # ADD on an already-present fd → EEXIST (the arena is one poll cycle behind
    # a concurrent submit at worst); fall back to MOD so the fd ends up armed
    # rather than staying a fired, disarmed oneshot.
    res = epoll_ctl(epollFd, EPOLL_CTL_MOD, fd, addr ev)
  # Still failing → this fd will not deliver readiness (both verbs failed, a MOD
  # on a registered fd failed, or it is not pollable at all: EPERM/EBADF).
  # `submitForPoll` fails the ops waiting on it.
  result = res == 0

proc epollPoll(timeoutMs: int): bool {.nimcall.} =
  let lane = ioLane()
  var buf {.noinit.}: array[DrainBatch, OpContext]
  var n = gOpQueues[lane].tryBulkDequeue(DrainBatch, buf)
  if n > 0:
    for i in 0..<n:
      let alreadyRegistered = gSlots[lane].hasPendingForFd(buf[i].fd)
      let idx = gSlots[lane].allocSlot(buf[i])
      armDeadline(lane, idx)
      if buf[i].kind == opTimeout:
        continue          # nothing to arm on: the deadline heap is the wait
      if buf[i].kind == opConnect:
        # Start the attempt here, on the polling thread, so the fd is already
        # connecting by the time we watch it. A connect that finished at once
        # has completed the slot and there is nothing left to arm.
        if not startConnect(buf[i].fd, idx):
          continue
      submitForPoll(buf[i].fd, alreadyRegistered)
  var ioEvents {.noinit.}: array[MaxIoEvents, EpollEvent]
  # Sleep no longer than the earliest deadline in this lane.
  let waitMs = waitMillis(lane, timeoutMs)
  n = int(epoll_wait(epollFds[lane], addr ioEvents[0], MaxIoEvents.cint, waitMs.cint))
  if n <= 0:
    expireDeadlines(lane)
    return false
  for i in 0..<n:
    let fd = cint(cast[uint](ioEvents[i].data.`ptr`))
    let events = ioEvents[i].events
    var firedEvents: IoEvents = {}
    if (events and EPOLLIN) != 0:
      firedEvents.incl evRead
    if (events and EPOLLOUT) != 0:
      firedEvents.incl evWrite
    processFd(fd, firedEvents)
  expireDeadlines(lane)
  return true

proc epollClose() {.nimcall.} =
  for i in 0..<epollFds.len:
    discard close(epollFds[i])

proc epollForgetFd(fd: cint) {.nimcall.} =
  ## Drop bookkeeping for a fd that is being closed, so a *future* fd with
  ## the same number (POSIX recycles them) is treated as a fresh ADD rather
  ## than incorrectly reusing stale MOD state.
  discard epoll_ctl(epollFds[ioLane()], EPOLL_CTL_DEL, fd, nil)

proc initEpollBackendRelays*(): BackendRelays =
  epollFds = @[]
  for i in 0..<ioLanes():
    epollFds.add(epoll_create1(0))
  reArmEvent = epollReArm
  result = BackendRelays(
    poll: epollPoll,
    close: epollClose,
    forgetFd: epollForgetFd,
  )
