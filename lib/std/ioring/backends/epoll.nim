# Linux epoll backend.
# epoll_ctl ADD is only valid the *first* time a fd is registered; every
# subsequent (re-)arm on the same fd — including the EPOLLONESHOT re-arm
# after each event — must use MOD, or epoll_ctl fails with EEXIST and the
# fd is silently never re-armed again (a per-connection deadlock that is
# easy to miss under light testing). `registeredFds` tracks which fds are
# already known to the epoll instance so submit/re-arm pick the right verb.

import ../../posix/epoll
import ../../posix/posix

import ../core/types
import ../core/slots
import ../core/backend
import ./poll
import std/[tables, ticketlocks]
import std/syncio

const MaxIoEvents = 64

var
  epollFd: cint
  registeredFds: Table[cint, bool]
  regLock: TicketLock

# Header-free per the std/posix convention: real symbols are bare-importc'd
# and constants are hand-written ABI transcriptions. EPERM/EBADF are 1/9 on
# every Linux ABI (asm-generic, shared by amd64/arm64/i386; musl agrees).
const
  EPERM = cint(1)
  EBADF = cint(9)

proc fdNotPollable(res: cint): bool {.inline.} =
  ## True when a failed epoll_ctl means the fd is no longer a live pollable
  ## descriptor we own: EPERM (a non-pollable type — a regular file, e.g. a socket
  ## fd closed by its transfer and its number reused by one of the process's file
  ## opens before this arm ran) or EBADF (already closed). Skipping such an fd is
  ## correct — it carries no real transfer, so not watching it can't stall one —
  ## and avoids error spam under multi-threaded handler resumption.
  result = res == EPERM or res == EBADF

proc epollReArm(fd: cint; mask: int) {.nimcall.} =
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
  var alreadyRegistered: bool
  withLock regLock:
    alreadyRegistered = registeredFds.getOrDefault(fd, false)
    registeredFds[fd] = true
  let op = if alreadyRegistered: EPOLL_CTL_MOD else: EPOLL_CTL_ADD
  var res = epoll_ctl(epollFd, op, fd, addr ev)
  if res != 0 and op == EPOLL_CTL_ADD:
    # Lost the race with a concurrent submit on the same fd that already
    # ADD'ed it (or the fd was previously registered and evicted from our
    # bookkeeping some other way) — fall back to MOD once.
    if not fdNotPollable(res):
      # Not a stale/non-pollable fd → a genuine ADD-vs-MOD race (the slot's
      # `registered` flag is advisory across workers). ADD on an already-present
      # fd → EEXIST; fall back to MOD so the fd ends up armed with the current
      # mask instead of staying a fired (disarmed) oneshot — that stall loses the
      # connection. (A regular-file/closed fd is skipped above; MOD can't help it.)
      res = epoll_ctl(epollFd, EPOLL_CTL_MOD, fd, addr ev)
      if res != 0:
        stderr.writeLine("ioring: epoll ADD+MOD both failed: " & $res)

proc epollPoll(timeoutMs: int): bool {.nimcall.} =
  var ioEvents {.noinit.}: array[MaxIoEvents, EpollEvent]
  let n = int(epoll_wait(epollFd, addr ioEvents[0], MaxIoEvents.cint, timeoutMs.cint))
  if n <= 0:
    return false
  for i in 0..<n:
    let fd = cint(cast[uint](ioEvents[i].data.`ptr`))
    processFd(fd, int(ioEvents[i].events))
  return true

proc epollClose() {.nimcall.} =
  discard close(epollFd)

proc epollForgetFd(fd: cint) {.nimcall.} =
  ## Drop bookkeeping for a fd that is being closed, so a *future* fd with
  ## the same number (POSIX recycles them) is treated as a fresh ADD rather
  ## than incorrectly reusing stale MOD state.
  withLock regLock:
    registeredFds.del(fd)
  discard epoll_ctl(epollFd, EPOLL_CTL_DEL, fd, nil)

proc initEpollBackendRelays*(): BackendRelays =
  epollFd = epoll_create1(0)
  registeredFds = initTable[cint, bool]()
  reArmEvent = epollReArm
  result = BackendRelays(
    submit: submitForPoll,
    poll: epollPoll,
    close: epollClose,
    forgetFd: epollForgetFd,
  )
