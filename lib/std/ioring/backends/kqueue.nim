# macOS/BSD kqueue backend — extends PollBackend.
# registerEvent/reArmEvent use kevent with EV_ONESHOT.
# poll drives kevent → processFd per event.

import ../core/types
import ../core/slots
import ../core/backends

type KqueueBackend* = ref object of PollBackend

when defined(macosx) or defined(freebsd) or defined(netbsd) or defined(openbsd) or defined(dragonfly):
  import std / assertions

  type
    Kevent {.importc: "struct kevent", header: "<sys/event.h>".} = object
      ident: uintptr
      filter: int16
      flags: uint16
      fflags: uint32
      data: int64
      udata: pointer

    Timespec {.importc: "struct timespec", header: "<sys/event.h>".} = object
      tv_sec: clong
      tv_nsec: clong

  proc kqueue(): cint {.importc, header: "<sys/event.h>".}

  proc kevent(kq: cint; changelist: ptr Kevent; nchanges: cint;
              eventlist: ptr Kevent; nevents: cint; timeout: ptr Timespec): cint {.
    importc, header: "<sys/event.h>".}

  proc close_fd(fd: cint): cint {.importc: "close", header: "<unistd.h>".}

  const
    EVFILT_READ = -1.cshort
    EVFILT_WRITE = -2.cshort
    EV_ADD = 0x0001.cushort
    EV_ONESHOT = 0x0020.cushort

  proc initKqueueBackend*(arena: int): KqueueBackend =
    new result
    result.pollFd = kqueue()
    result.arena = arena

  method registerEvent*(b: KqueueBackend; fd: cint; slotIdx: int; mask: int) =
    var ev {.noinit.}: Kevent
    if (mask and EvRead) != 0:
      ev.ident = uintptr(fd)
      ev.filter = EVFILT_READ
      ev.flags = EV_ADD or EV_ONESHOT
      ev.udata = cast[pointer](uint(slotIdx))
      discard kevent(b.pollFd, addr ev, 1, nil, 0, nil)
    if (mask and EvWrite) != 0:
      ev.ident = uintptr(fd)
      ev.filter = EVFILT_WRITE
      ev.flags = EV_ADD or EV_ONESHOT
      ev.udata = cast[pointer](uint(slotIdx))
      discard kevent(b.pollFd, addr ev, 1, nil, 0, nil)

  method reArmEvent*(b: KqueueBackend; fd: cint; slotIdx: int; mask: int) =
    b.registerEvent(fd, slotIdx, mask)

  method poll*(b: KqueueBackend; timeoutMs: int): bool =
    let a = cast[ptr SlotArena](b.arena)
    var events {.noinit.}: array[64, Kevent]
    var ts = Timespec(
      tv_sec: clong(timeoutMs div 1000),
      tv_nsec: clong((timeoutMs mod 1000) * 1_000_000))
    let n = int(kevent(b.pollFd, nil, 0, addr events[0], 64, addr ts))
    if n <= 0:
      return false
    for i in 0..<n:
      let fd = cint(events[i].ident)
      let fired = if events[i].filter == EVFILT_READ: EvRead else: EvWrite
      b.processFd(fd, fired)
    return true

  method close*(b: KqueueBackend) =
    discard close_fd(b.pollFd)

else:
  proc initKqueueBackend*(arena: int): KqueueBackend =
    new result

  method registerEvent*(b: KqueueBackend; fd: cint; slotIdx: int; mask: int) = discard
  method reArmEvent*(b: KqueueBackend; fd: cint; slotIdx: int; mask: int) = discard
  method poll*(b: KqueueBackend; timeoutMs: int): bool = false
  method close*(b: KqueueBackend) = discard
