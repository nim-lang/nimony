# macOS/BSD kqueue backend.
# registerEvent/reArmEvent use kevent with EV_ONESHOT.
# poll drives kevent → processFd per event.

import ../../posix/kqueue
import ../../posix/posix

import ../core/types
import ../core/slots
import ../core/backend
import ./poll

var kqFd: cint

proc kqueueReArm(fd: cint; mask: int) {.nimcall.} =
  # EV_ADD is idempotent in kqueue (add-or-modify), unlike epoll's ADD/MOD
  # split, so there is no separate "first time vs re-arm" bookkeeping needed
  # here. `ident` (the fd) is what `kqueuePoll` reads back on delivery, not
  # `udata`, so no slot index needs to travel through the kernel at all.
  var ev {.noinit.}: KEvent
  if (mask and EvRead) != 0:
    ev.ident = uint(fd)
    ev.filter = EVFILT_READ
    ev.flags = EV_ADD or EV_ONESHOT
    discard kevent(kqFd, addr ev, 1, nil, 0, nil)
  if (mask and EvWrite) != 0:
    ev.ident = uint(fd)
    ev.filter = EVFILT_WRITE
    ev.flags = EV_ADD or EV_ONESHOT
    discard kevent(kqFd, addr ev, 1, nil, 0, nil)

proc kqueuePoll(timeoutMs: int): bool {.nimcall.} =
  var events {.noinit.}: array[64, KEvent]
  var ts = Timespec(
    tv_sec: Time(timeoutMs div 1000),
    tv_nsec: clong((timeoutMs mod 1000) * 1_000_000))
  let n = int(kevent(kqFd, nil, 0, addr events[0], 64, addr ts))
  if n <= 0:
    return false
  for i in 0..<n:
    let fd = cint(events[i].ident)
    let fired = if events[i].filter == EVFILT_READ: EvRead else: EvWrite
    processFd(fd, fired)
  return true

proc kqueueClose() {.nimcall.} =
  discard close(kqFd)

proc kqueueForgetFd(fd: cint) {.nimcall.} =
  discard

proc initKqueueBackendRelays*(): BackendRelays =
  kqFd = kqueue()
  reArmEvent = kqueueReArm
  result = BackendRelays(
    submit: submitForPoll,
    poll: kqueuePoll,
    close: kqueueClose,
    forgetFd: kqueueForgetFd,
  )
