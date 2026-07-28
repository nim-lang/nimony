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
import ../core/backends
import std/[tables, ticketlocks]

const MaxIoEvents = 64

type EpollBackend* = ref object of PollBackend
  registeredFds: Table[cint, bool]
  regLock: TicketLock

proc epollArm(b: EpollBackend; fd: cint; mask: int) =
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
    discard epoll_ctl(b.pollFd, EPOLL_CTL_MOD, fd, addr ev)

proc epollReArmEvent(b: Backend; fd: cint; mask: int) {.nimcall.} =
  EpollBackend(b).epollArm(fd, mask)

proc epollSubmit(b: Backend; slotIdx: int; op: ptr OpContext) {.nimcall.} =
  let self = EpollBackend(b)
  var mask = 0
  if op.kind == opRead or op.kind == opAccept:
    mask = mask or EvRead
  if op.kind == opWrite:
    mask = mask or EvWrite
  self.epollArm(op.fd, mask)

proc epollPoll(b: Backend; timeoutMs: int): bool {.nimcall.} =
  let self = EpollBackend(b)
  var ioEvents {.noinit.}: array[MaxIoEvents, EpollEvent]
  let n = int(epoll_wait(self.pollFd, addr ioEvents[0], MaxIoEvents.cint, timeoutMs.cint))
  if n <= 0:
    return false
  for i in 0..<n:
    let fd = cint(cast[uint](ioEvents[i].data.`ptr`))
    b.processFd(fd, int(ioEvents[i].events))
  return true

proc epollClose(b: Backend) {.nimcall.} =
  let self = EpollBackend(b)
  discard close(self.pollFd)

proc epollForgetFd*(b: EpollBackend; fd: cint) =
  ## Drop bookkeeping for a fd that is being closed, so a *future* fd with
  ## the same number (POSIX recycles them) is treated as a fresh ADD rather
  ## than incorrectly reusing stale MOD state.
  withLock b.regLock:
    b.registeredFds.del(fd)
  discard epoll_ctl(b.pollFd, EPOLL_CTL_DEL, fd, nil)

proc initEpollBackend*(arena: SlotArena; ring: Ring): EpollBackend =
  new result
  result.pollFd = epoll_create1(0)
  result.arena = arena
  result.ring = ring
  result.registeredFds = initTable[cint, bool]()
  result.submitFn = epollSubmit
  result.pollFn = epollPoll
  result.closeFn = epollClose
  result.reArmEventFn = epollReArmEvent
  result.forgetFdFn = proc(b: Backend; fd: cint) {.nimcall.} =
    EpollBackend(b).epollForgetFd(fd)
