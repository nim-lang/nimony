# macOS/BSD kqueue backend — extends PollBackend.
# registerEvent/reArmEvent use kevent with EV_ONESHOT.
# poll drives kevent → processFd per event.

import ../../posix/kqueue
import ../../posix/posix

import ../core/types
import ../core/slots
import ../core/backend
import ../core/backends

type KqueueBackend* = ref object of PollBackend

method reArmEvent*(b: KqueueBackend; fd: cint; mask: int) =
  # EV_ADD is idempotent in kqueue (add-or-modify), unlike epoll's ADD/MOD
  # split, so there is no separate "first time vs re-arm" bookkeeping needed
  # here. `ident` (the fd) is what `kqueuePoll` reads back on delivery, not
  # `udata`, so no slot index needs to travel through the kernel at all.
  var ev {.noinit.}: Kevent
  if (mask and EvRead) != 0:
    ev.ident = uint(fd)
    ev.filter = EVFILT_READ
    ev.flags = EV_ADD or EV_ONESHOT
    discard kevent(b.pollFd, addr ev, 1, nil, 0, nil)
  if (mask and EvWrite) != 0:
    ev.ident = uint(fd)
    ev.filter = EVFILT_WRITE
    ev.flags = EV_ADD or EV_ONESHOT
    discard kevent(b.pollFd, addr ev, 1, nil, 0, nil)

method poll*(b: KqueueBackend; timeoutMs: int): bool =
  let a = b.ring.slots
  var events {.noinit.}: array[64, Kevent]
  var ts = Timespec(
    tv_sec: Time(timeoutMs div 1000),
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
  discard close(b.pollFd)

proc initKqueueBackend*(ring: Ring): KqueueBackend =
  new result
  result.pollFd = kqueue()
  result.ring = ring
