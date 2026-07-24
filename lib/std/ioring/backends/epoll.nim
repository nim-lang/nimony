# Linux epoll backend — extends PollBackend.
# registerEvent/reArmEvent use epoll_ctl ADD/MOD with EPOLLONESHOT.
# poll drives epoll_wait → processFd per event.

import ../core/types
import ../core/slots
import ../core/backends

const MaxIoEvents = 64

type EpollBackend* = ref object of PollBackend

when defined(linux):
  {.feature: "lenientnils".}

  import std / assertions

  type
    EpollData {.importc: "epoll_data_t", header: "<sys/epoll.h>".} = object
      `ptr`*: pointer

    EpollEvent {.importc: "struct epoll_event", header: "<sys/epoll.h>".} = object
      events*: uint32
      data*: EpollData

  const
    EPOLLONESHOT = (1 shl 30).uint32
    EPOLLIN = 0x001'u32
    EPOLLOUT = 0x004'u32
    EPOLL_CTL_ADD = 1.cint
    EPOLL_CTL_DEL = 2.cint
    EPOLL_CTL_MOD = 3.cint

  proc epoll_create1(flags: cint): cint {.importc, header: "<sys/epoll.h>".}
  proc epoll_ctl(epfd, op, fd: cint; ev: ptr EpollEvent): cint {.importc, header: "<sys/epoll.h>".}
  proc epoll_wait(epfd: cint; events: ptr EpollEvent; maxevents, timeout: cint): cint {.importc, header: "<sys/epoll.h>".}
  proc close_fd(fd: cint): cint {.importc: "close", header: "<unistd.h>".}

  proc initEpollBackend*(arena: int): EpollBackend =
    new result
    result.pollFd = epoll_create1(0)
    result.arena = arena

  method registerEvent*(b: EpollBackend; fd: cint; slotIdx: int; mask: int) =
    var ev {.noinit.}: EpollEvent
    ev.events = EPOLLONESHOT
    if (mask and EvRead) != 0:
      ev.events = ev.events or EPOLLIN
    if (mask and EvWrite) != 0:
      ev.events = ev.events or EPOLLOUT
    ev.data.`ptr` = cast[pointer](uint(slotIdx))
    discard epoll_ctl(b.pollFd, EPOLL_CTL_ADD, fd, addr ev)

  method reArmEvent*(b: EpollBackend; fd: cint; slotIdx: int; mask: int) =
    var ev {.noinit.}: EpollEvent
    ev.events = EPOLLONESHOT
    if (mask and EvRead) != 0:
      ev.events = ev.events or EPOLLIN
    if (mask and EvWrite) != 0:
      ev.events = ev.events or EPOLLOUT
    ev.data.`ptr` = cast[pointer](uint(slotIdx))
    discard epoll_ctl(b.pollFd, EPOLL_CTL_MOD, fd, addr ev)

  method poll*(b: EpollBackend; timeoutMs: int): bool =
    let a = cast[ptr SlotArena](b.arena)
    var ioEvents {.noinit.}: array[MaxIoEvents, EpollEvent]
    let n = int(epoll_wait(b.pollFd, addr ioEvents[0], MaxIoEvents.cint, timeoutMs.cint))
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

  method close*(b: EpollBackend) =
    discard close_fd(b.pollFd)

else:
  proc initEpollBackend*(arena: int): EpollBackend =
    new result

  method registerEvent*(b: EpollBackend; fd: cint; slotIdx: int; mask: int) = discard
  method reArmEvent*(b: EpollBackend; fd: cint; slotIdx: int; mask: int) = discard
  method poll*(b: EpollBackend; timeoutMs: int): bool = false
  method close*(b: EpollBackend) = discard
