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
import std/syncio

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
  let e = errno()
  result = e == EPERM or e == EBADF

proc epollReArm(fd: cint; mask: int, alreadyRegistered: bool) {.nimcall.} =
  let epollFd = epollFds[ioLane()]
  var ev {.noinit.}: EpollEvent
  ev.events = EPOLLONESHOT
  if (mask and EvRead) != 0:
    ev.events = ev.events or EPOLLIN
  if (mask and EvWrite) != 0:
    ev.events = ev.events or EPOLLOUT
  # Store the fd itself (not a slot index) as user data: a slot can be freed
  # and its index reused for a *different* fd between registration and the
  # event firing, which previously made `data.ptr` an unreliable — and
  # occasionally wrong — way to recover the fd on delivery.
  ev.data.`ptr` = cast[pointer](uint(fd))
  let op = if alreadyRegistered: EPOLL_CTL_MOD else: EPOLL_CTL_ADD
  var res = epoll_ctl(epollFd, op, fd, addr ev)
  if res != 0 and op == EPOLL_CTL_ADD:
    # Lost the race with a concurrent submit on the same fd that already
    # ADD'ed it (or the fd was previously registered and evicted from our
    # bookkeeping some other way) — fall back to MOD once.
    if not fdNotPollable():
      # Not a stale/non-pollable fd → a genuine ADD-vs-MOD race (whether the fd
      # is already registered is derived from the arena, one poll cycle behind
      # a concurrent submit at worst). ADD on an already-present
      # fd → EEXIST; fall back to MOD so the fd ends up armed with the current
      # mask instead of staying a fired (disarmed) oneshot — that stall loses the
      # connection. (A regular-file/closed fd is skipped above; MOD can't help it.)
      res = epoll_ctl(epollFd, EPOLL_CTL_MOD, fd, addr ev)
      if res != 0:
        # Report the ERRNO, not the return value: epoll_ctl reports every
        # failure as -1, so "failed: -1" says nothing about which failure it
        # was — and this branch fires only when both verbs failed on a fd we
        # believe is live, i.e. exactly when the reason matters.
        stderr.writeLine("ioring: epoll ADD+MOD both failed on fd " & $fd &
                         " (errno " & $errno() & ")")

proc epollPoll(timeoutMs: int): bool {.nimcall.} =
  let lane = ioLane()
  var buf {.noinit.}: array[DrainBatch, OpContext]
  var n = gOpQueues[lane].tryBulkDequeue(DrainBatch, buf)
  if n > 0:
    for i in 0..<n:
      let alreadyRegistered = gSlots[lane].hasPendingForFd(buf[i].fd)
      discard gSlots[lane].allocSlot(buf[i])
      submitForPoll(buf[i].fd, alreadyRegistered)
  var ioEvents {.noinit.}: array[MaxIoEvents, EpollEvent]
  n = int(epoll_wait(epollFds[lane], addr ioEvents[0], MaxIoEvents.cint, timeoutMs.cint))
  if n <= 0:
    return false
  for i in 0..<n:
    let fd = cint(cast[uint](ioEvents[i].data.`ptr`))
    let events = ioEvents[i].events
    var firedEvents = 0
    if (events and EPOLLIN) != 0:
      firedEvents = firedEvents or EvRead
    if (events and EPOLLOUT) != 0:
      firedEvents = firedEvents or EvWrite
    processFd(fd, firedEvents)
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
