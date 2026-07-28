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

proc kqueueRegisterEvent(b: KqueueBackend; fd: cint; slotIdx: int; mask: int) =
  var ev {.noinit.}: Kevent
  if (mask and EvRead) != 0:
    ev.ident = uint(fd)
    ev.filter = EVFILT_READ
    ev.flags = EV_ADD or EV_ONESHOT
    ev.udata = cast[pointer](uint(slotIdx))
    discard kevent(b.pollFd, addr ev, 1, nil, 0, nil)
  if (mask and EvWrite) != 0:
    ev.ident = uint(fd)
    ev.filter = EVFILT_WRITE
    ev.flags = EV_ADD or EV_ONESHOT
    ev.udata = cast[pointer](uint(slotIdx))
    discard kevent(b.pollFd, addr ev, 1, nil, 0, nil)

proc kqueueReArmEvent(b: Backend; fd: cint; slotIdx: int; mask: int) {.nimcall.} =
  let self = KqueueBackend(b)
  self.kqueueRegisterEvent(fd, slotIdx, mask)

proc kqueueSubmit(b: Backend; slotIdx: int; op: ptr OpContext) {.nimcall.} =
  let self = KqueueBackend(b)
  var mask = 0
  if op.kind == opRead or op.kind == opAccept:
    mask = mask or EvRead
  if op.kind == opWrite:
    mask = mask or EvWrite
  self.kqueueRegisterEvent(op.fd, slotIdx, mask)

proc kqueuePoll(b: Backend; timeoutMs: int): bool {.nimcall.} =
  let self = KqueueBackend(b)
  let a = b.arena
  var events {.noinit.}: array[64, Kevent]
  var ts = Timespec(
    tv_sec: Time(timeoutMs div 1000),
    tv_nsec: clong((timeoutMs mod 1000) * 1_000_000))
  let n = int(kevent(self.pollFd, nil, 0, addr events[0], 64, addr ts))
  if n <= 0:
    return false
  for i in 0..<n:
    let fd = cint(events[i].ident)
    let fired = if events[i].filter == EVFILT_READ: EvRead else: EvWrite
    b.processFd(fd, fired)
  return true

proc kqueueClose(b: Backend) {.nimcall.} =
  let self = KqueueBackend(b)
  discard close(self.pollFd)

proc initKqueueBackend*(arena: SlotArena; ring: Ring): KqueueBackend =
  new result
  result.pollFd = kqueue()
  result.arena = arena
  result.ring = ring
  result.submitFn = kqueueSubmit
  result.pollFn = kqueuePoll
  result.closeFn = kqueueClose
  result.reArmEventFn = kqueueReArmEvent
