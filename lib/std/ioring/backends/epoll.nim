# Linux epoll backend — extends PollBackend.
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

const MaxIoEvents = 64

type EpollBackend* = ref object of PollBackend
  registeredFds: Table[cint, bool]
  regLock: TicketLock

# Header-free per the std/posix convention: real symbols are bare-importc'd
# and constants are hand-written ABI transcriptions. EPERM/EBADF are 1/9 on
# every Linux ABI (asm-generic, shared by amd64/arm64/i386; musl agrees).
const
  epollErrPerm = cint(1)   # EPERM
  epollErrBadFd = cint(9)  # EBADF

proc errnoLocation(): ptr cint {.importc: "__errno_location", sideEffect.}
  ## glibc's and musl's address-returning errno accessor (same pattern as
  ## std/posix). Read immediately after a failing epoll_ctl, no intervening
  ## call, to classify the failure.

proc reportResidualFailure(msg: string) =
  ## A residual epoll_ctl failure (both the primary op and its fallback
  ## failed on a live pollable fd) — report on stderr with the errno number.
  ## Rides the panic path's `writeErr` (raw fd 2, no stdio); a private
  ## bare-importc `write` here would collide with the <unistd.h> prototype
  ## that this module's usleep/close header imports pull into the TU.
  writeErr(msg & " (errno " & $errnoLocation()[] & ")\n")

proc fdNotPollable(): bool {.inline.} =
  ## True when a failed epoll_ctl means the fd is no longer a live pollable
  ## descriptor we own: EPERM (a non-pollable type — a regular file, e.g. a socket
  ## fd closed by its transfer and its number reused by one of the process's file
  ## opens before this arm ran) or EBADF (already closed). Skipping such an fd is
  ## correct — it carries no real transfer, so not watching it can't stall one —
  ## and avoids error spam under multi-threaded handler resumption.
  let e = errnoLocation()[]
  result = e == epollErrPerm or e == epollErrBadFd

method reArmEvent*(b: EpollBackend; fd: cint; mask: int) =
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
  withLock b.regLock:
    alreadyRegistered = b.registeredFds.getOrDefault(fd, false)
    b.registeredFds[fd] = true
  let op = if alreadyRegistered: EPOLL_CTL_MOD else: EPOLL_CTL_ADD
  if epoll_ctl(b.pollFd, op, fd, addr ev) != 0 and op == EPOLL_CTL_ADD:
    # Lost the race with a concurrent submit on the same fd that already
    # ADD'ed it (or the fd was previously registered and evicted from our
    # bookkeeping some other way) — fall back to MOD once.
    if not fdNotPollable():
      # Not a stale/non-pollable fd → a genuine ADD-vs-MOD race (the slot's
      # `registered` flag is advisory across workers). ADD on an already-present
      # fd → EEXIST; fall back to MOD so the fd ends up armed with the current
      # mask instead of staying a fired (disarmed) oneshot — that stall loses the
      # connection. (A regular-file/closed fd is skipped above; MOD can't help it.)
      if epoll_ctl(b.pollFd, EPOLL_CTL_MOD, fd, addr ev) != 0:
        reportResidualFailure("ioring: epoll ADD+MOD both failed")

method poll*(b: EpollBackend; timeoutMs: int): bool =
  var ioEvents {.noinit.}: array[MaxIoEvents, EpollEvent]
  let n = int(epoll_wait(b.pollFd, addr ioEvents[0], MaxIoEvents.cint, timeoutMs.cint))
  if n <= 0:
    return false
  for i in 0..<n:
    let fd = cint(cast[uint](ioEvents[i].data.`ptr`))
    b.processFd(fd, int(ioEvents[i].events))
  return true

method close*(b: EpollBackend) =
  discard close(b.pollFd)

method forgetFd*(b: EpollBackend; fd: cint) =
  ## Drop bookkeeping for a fd that is being closed, so a *future* fd with
  ## the same number (POSIX recycles them) is treated as a fresh ADD rather
  ## than incorrectly reusing stale MOD state.
  withLock b.regLock:
    b.registeredFds.del(fd)
  discard epoll_ctl(b.pollFd, EPOLL_CTL_DEL, fd, nil)

proc initEpollBackend*(ring: Ring): EpollBackend =
  new result
  result.pollFd = epoll_create1(0)
  result.ring = ring
  result.registeredFds = initTable[cint, bool]()
