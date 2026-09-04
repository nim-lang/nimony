# macOS/BSD kqueue backend.
# registerEvent/reArmEvent use kevent with EV_ONESHOT.
# poll drives kevent → processFd per event.

import std/threadpool

import ../../posix/kqueue
import ../../posix/posix

import ../core/types
import ../core/slots
import ../core/backend
import ./poll

const 
  DrainBatch = 128

var kqFds: seq[cint]

proc kqueueReArm(fd: cint; events: IoEvents, alreadyRegistered: bool): bool {.nimcall.} =
  # EV_ADD is idempotent in kqueue (add-or-modify), unlike epoll's ADD/MOD
  # split, so there is no separate "first time vs re-arm" bookkeeping needed
  # here. `ident` (the fd) is what `kqueuePoll` reads back on delivery, not
  # `udata`, so no slot index needs to travel through the kernel at all.
  # Zero-initialised, NOT `{.noinit.}`: `fflags`, `data` and `udata` are read by
  # the kernel too. A stray NOTE_LOWAT in a garbage `fflags` turns the garbage in
  # `data` into a low-water mark, so the registration would fire only after N
  # bytes — or never.
  var ev = default(KEvent)
  let kq = kqFds[ioLane()]
  # Reported, not discarded: a failed registration means no readiness, and
  # `submitForPoll` fails the ops waiting on it.
  result = true
  if evRead in events:
    ev.ident = uint(fd)
    ev.filter = EVFILT_READ
    ev.flags = EV_ADD or EV_ONESHOT
    if kevent(kq, addr ev, 1, nil, 0, nil) < 0: result = false
  if evWrite in events:
    ev.ident = uint(fd)
    ev.filter = EVFILT_WRITE
    ev.flags = EV_ADD or EV_ONESHOT
    if kevent(kq, addr ev, 1, nil, 0, nil) < 0: result = false

proc kqueuePoll(timeoutMs: int): bool {.nimcall.} =
  let lane = ioLane()
  var buf {.noinit.}: array[DrainBatch, OpContext]
  var n = gOpQueues[lane].tryBulkDequeue(DrainBatch, buf)
  if n > 0:
    for i in 0..<n:
      let idx = gSlots[lane].allocSlot(buf[i])
      armDeadline(lane, idx)
      case buf[i].kind
      of opTimeout:
        discard            # nothing to arm on: the deadline heap is the wait
      of opNop:
        complete(idx, 0)   # nothing to wait for either
      of opConnect:
        # Start the attempt here, on the polling thread, so the fd is already
        # connecting by the time we watch it. A connect that finished at once
        # has completed the slot and there is nothing left to arm.
        if startConnect(buf[i].fd, idx):
          submitForPoll(buf[i].fd)
      else:
        submitForPoll(buf[i].fd)
  var kevents {.noinit.}: array[64, KEvent]
  # Sleep no longer than the earliest deadline in this lane, so a timer fires
  # on time instead of on the next poll that happens for another reason.
  let waitMs = waitMillis(lane, timeoutMs)
  var ts = Timespec(
    tv_sec: Time(waitMs div 1000),
    tv_nsec: clong((waitMs mod 1000) * 1_000_000))
  n = int(kevent(kqFds[lane], nil, 0, addr kevents[0], 64, addr ts))
  if n <= 0:
    expireDeadlines(lane)
    return false
  # A poll-add registers two kevents (EVFILT_READ and EVFILT_WRITE); when the
  # fd is ready in both directions both fire in the same batch. Deliver the
  # union per fd so a oneshot poll-add on a socket that is simultaneously
  # readable and writable reports {evRead, evWrite} like io_uring's
  # IORING_OP_POLL_ADD does, instead of completing on whichever event comes
  # first. Per-direction matching for read/write ops still happens inside
  # processFd.
  var firedFds {.noinit.}: array[64, cint]
  var firedEvents {.noinit.}: array[64, IoEvents]
  var m = 0
  for i in 0..<n:
    let fd = cint(kevents[i].ident)
    let dir = if kevents[i].filter == EVFILT_READ: evRead else: evWrite
    var k = 0
    while k < m and firedFds[k] != fd:
      inc k
    if k == m:
      firedFds[m] = fd
      firedEvents[m] = {dir}
      inc m
    else:
      firedEvents[k].incl dir
  for i in 0..<m:
    processFd(firedFds[i], firedEvents[i])
  expireDeadlines(lane)
  return true

proc kqueueClose() {.nimcall.} =
  for i in 0..<kqFds.len:
    discard close(kqFds[i])

proc kqueueForgetFd(fd: cint) {.nimcall.} =
  discard

proc initKqueueBackendRelays*(): BackendRelays =
  kqFds = @[]
  for i in 0..<ioLanes():
    kqFds.add kqueue()
  reArmEvent = kqueueReArm
  result = BackendRelays(
    poll: kqueuePoll,
    close: kqueueClose,
    forgetFd: kqueueForgetFd,
  )
