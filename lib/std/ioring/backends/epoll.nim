# Linux epoll backend — extends PollBackend.
# registerEvent/reArmEvent use epoll_ctl ADD/MOD with EPOLLONESHOT.
# poll drives epoll_wait → processFd per event.

import ../../posix/epoll
import ../../posix/posix

import ../core/types
import ../core/slots
import ../core/backend
import ../core/backends

const MaxIoEvents = 64

type EpollBackend* = ref object of PollBackend

proc epollRegisterEvent(b: EpollBackend; fd: cint; slotIdx: int; mask: int) =
  var ev {.noinit.}: EpollEvent
  ev.events = EPOLLONESHOT
  if (mask and EvRead) != 0:
    ev.events = ev.events or EPOLLIN
  if (mask and EvWrite) != 0:
    ev.events = ev.events or EPOLLOUT
  ev.data.`ptr` = cast[pointer](uint(slotIdx))
  discard epoll_ctl(b.pollFd, EPOLL_CTL_ADD, fd, addr ev)

proc epollReArmEvent(b: EpollBackend; fd: cint; slotIdx: int; mask: int) =
  var ev {.noinit.}: EpollEvent
  ev.events = EPOLLONESHOT
  if (mask and EvRead) != 0:
    ev.events = ev.events or EPOLLIN
  if (mask and EvWrite) != 0:
    ev.events = ev.events or EPOLLOUT
  ev.data.`ptr` = cast[pointer](uint(slotIdx))
  discard epoll_ctl(b.pollFd, EPOLL_CTL_MOD, fd, addr ev)

proc epollSubmit(b: Backend; slotIdx: int; op: ptr OpContext) {.nimcall.} =
  let self = EpollBackend(b)
  var mask = 0
  if op.kind == opRead or op.kind == opAccept:
    mask = mask or EvRead
  if op.kind == opWrite:
    mask = mask or EvWrite
  self.epollRegisterEvent(op.fd, slotIdx, mask)

proc epollPoll(b: Backend; timeoutMs: int): bool {.nimcall.} =
  let self = EpollBackend(b)
  let a = b.arena
  var ioEvents {.noinit.}: array[MaxIoEvents, EpollEvent]
  let n = int(epoll_wait(self.pollFd, addr ioEvents[0], MaxIoEvents.cint, timeoutMs.cint))
  if n <= 0:
    return false
  for i in 0..<n:
    let slotIdx = int(cast[uint](ioEvents[i].data.`ptr`))
    if slotIdx < 0 or slotIdx >= MaxOps:
      continue
    let fd = a.slots[slotIdx].fd
    if not a.slots[slotIdx].inUse:
      continue
    b.processFd(fd, int(ioEvents[i].events))
  return true

proc epollClose(b: Backend) {.nimcall.} =
  let self = EpollBackend(b)
  discard close(self.pollFd)

proc initEpollBackend*(arena: SlotArena; ring: Ring): EpollBackend =
  new result
  result.pollFd = epoll_create1(0)
  result.arena = arena
  result.ring = ring
  result.submitFn = epollSubmit
  result.pollFn = epollPoll
  result.closeFn = epollClose
