# Linux epoll backend — OOP version.
# Each fd is registered with EPOLLONESHOT (auto-disarm after first event).
# When poll fires, all pending slots for the fd are processed, then the
# fd is re-armed if any slots remain.

import ../core/types
import ../core/slots

const MaxIoEvents = 64

type EpollBackend* = ref object of Backend
  epfd: cint
  arena: int

when defined(linux):
  {.feature: "lenientnils".}

  import std / assertions
  from std/posix/posix import SockAddr, SockLen, FileHandle

  proc posixRead(fd: cint; buf: pointer; count: int): int {.importc: "read", header: "<unistd.h>".}
  proc posixWrite(fd: cint; buf: pointer; count: int): int {.importc: "write", header: "<unistd.h>".}

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
  proc accept_sock(s: cint; `addr`: ptr SockAddr; addrlen: ptr SockLen): cint {.
    importc: "accept", header: "<sys/socket.h>".}

  proc initEpollBackend*(arena: int): EpollBackend =
    new result
    result.epfd = epoll_create1(0)
    result.arena = arena

  method submit*(b: EpollBackend; slotIdx: int; op: ptr OpContext) =
    var mask = EPOLLONESHOT
    if op.kind == opRead or op.kind == opAccept:
      mask = mask or EPOLLIN
    if op.kind == opWrite:
      mask = mask or EPOLLOUT
    var ev {.noinit.}: EpollEvent
    ev.events = mask
    ev.data.`ptr` = cast[pointer](uint(slotIdx))
    discard epoll_ctl(b.epfd, EPOLL_CTL_ADD, op.fd, addr ev)

  method poll*(b: EpollBackend; timeoutMs: int): bool =
    let a = cast[ptr SlotArena](b.arena)
    var ioEvents {.noinit.}: array[MaxIoEvents, EpollEvent]
    let n = int(epoll_wait(b.epfd, addr ioEvents[0], MaxIoEvents.cint, timeoutMs.cint))
    if n <= 0:
      return false

    for i in 0..<n:
      let slotIdx = int(cast[uint](ioEvents[i].data.`ptr`))
      if slotIdx < 0 or slotIdx >= MaxOps:
        continue
      let fd = a.slots[slotIdx].fd
      if not a.slots[slotIdx].inUse:
        continue

      # collect all pending slot indices for this fd
      var pending = newSeq[int]()
      for j in 0..<MaxOps:
        if a.slots[j].inUse and a.slots[j].fd == fd:
          pending.add(j)
          if pending.len == MaxOps:
            break

      # process each pending slot
      for j in pending:
        let s = addr a.slots[j]
        case s.kind
        of opRead:
          let r = posixRead(fd, s.buf, s.len)
          if b.completeFn != nil:
            b.completeFn(j, if r >= 0: r else: -1, b.completeEnv)
        of opWrite:
          let r = posixWrite(fd, s.buf, s.len)
          if b.completeFn != nil:
            b.completeFn(j, if r >= 0: r else: -1, b.completeEnv)
        of opAccept:
          var addrLen = s.acceptLen
          let clientFd = accept_sock(fd, cast[ptr SockAddr](addr s.acceptAddr), addr addrLen)
          if b.completeFn != nil:
            b.completeFn(j, if clientFd >= 0: clientFd else: -1, b.completeEnv)

      # re-arm if any slots remain for this fd
      var remaining = false
      var newSlotIdx = -1
      var newMask = EPOLLONESHOT
      for j in 0..<MaxOps:
        if a.slots[j].inUse and a.slots[j].fd == fd:
          if not remaining:
            remaining = true
            newSlotIdx = j
            let sk = a.slots[j].kind
            if sk == opRead or sk == opAccept:
              newMask = newMask or EPOLLIN
            if sk == opWrite:
              newMask = newMask or EPOLLOUT
          else:
            break

      if remaining:
        var ev {.noinit.}: EpollEvent
        ev.events = newMask
        ev.data.`ptr` = cast[pointer](uint(newSlotIdx))
        discard epoll_ctl(b.epfd, EPOLL_CTL_MOD, fd, addr ev)

    return true

  method close*(b: EpollBackend) =
    discard close_fd(b.epfd)

else:
  proc initEpollBackend*(arena: int): EpollBackend =
    new result

  method submit*(b: EpollBackend; slotIdx: int; op: ptr OpContext) = discard
  method poll*(b: EpollBackend; timeoutMs: int): bool = false
  method close*(b: EpollBackend) = discard
