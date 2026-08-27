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

proc kqueueReArm(fd: cint; mask: int, alreadyRegistered: bool) {.nimcall.} =
  # EV_ADD is idempotent in kqueue (add-or-modify), unlike epoll's ADD/MOD
  # split, so there is no separate "first time vs re-arm" bookkeeping needed
  # here. `ident` (the fd) is what `kqueuePoll` reads back on delivery, not
  # `udata`, so no slot index needs to travel through the kernel at all.
  var ev {.noinit.}: KEvent
  if (mask and EvRead) != 0:
    ev.ident = uint(fd)
    ev.filter = EVFILT_READ
    ev.flags = EV_ADD or EV_ONESHOT
    discard kevent(kqFds[threadIdx], addr ev, 1, nil, 0, nil)
  if (mask and EvWrite) != 0:
    ev.ident = uint(fd)
    ev.filter = EVFILT_WRITE
    ev.flags = EV_ADD or EV_ONESHOT
    discard kevent(kqFds[threadIdx], addr ev, 1, nil, 0, nil)

proc kqueuePoll(timeoutMs: int): bool {.nimcall.} =
  var buf {.noinit.}: array[DrainBatch, OpContext]
  var n = gOpQueues[threadIdx].tryBulkDequeue(DrainBatch, buf)
  if n > 0:
    for i in 0..<n:
      let idx = gSlots[threadIdx].allocSlot(buf[i])
      submitForPoll(idx, buf[i].addr)
  var events {.noinit.}: array[64, KEvent]
  var ts = Timespec(
    tv_sec: Time(timeoutMs div 1000),
    tv_nsec: clong((timeoutMs mod 1000) * 1_000_000))
  n = int(kevent(kqFds[threadIdx], nil, 0, addr events[0], 64, addr ts))
  if n <= 0:
    return false
  # A poll-add registers two kevents (EVFILT_READ and EVFILT_WRITE); when the
  # fd is ready in both directions both fire in the same batch. Deliver the
  # union per fd so a oneshot poll-add on a socket that is simultaneously
  # readable and writable reports EvRead or EvWrite like io_uring's
  # IORING_OP_POLL_ADD does, instead of completing on whichever event comes
  # first. Per-direction matching for read/write ops still happens inside
  # processFd.
  var firedFds: array[64, cint]
  var firedMasks: array[64, int]
  var m = 0
  for i in 0..<n:
    let fd = cint(events[i].ident)
    var k = 0
    while k < m and firedFds[k] != fd:
      inc k
    if k == m:
      firedFds[m] = fd
      firedMasks[m] = if events[i].filter == EVFILT_READ: EvRead else: EvWrite
      inc m
    elif events[i].filter == EVFILT_READ:
      firedMasks[k] = firedMasks[k] or EvRead
    else:
      firedMasks[k] = firedMasks[k] or EvWrite
  for i in 0..<m:
    processFd(firedFds[i], firedMasks[i])
  return true

proc kqueueClose() {.nimcall.} =
  discard close(kqFds[threadIdx])

proc kqueueForgetFd(fd: cint) {.nimcall.} =
  discard

proc initKqueueBackendRelays*(): BackendRelays =
  kqFds = @[]
  for i in 0..<workerCount:
    kqFds.add kqueue()
  reArmEvent = kqueueReArm
  result = BackendRelays(
    poll: kqueuePoll,
    close: kqueueClose,
    forgetFd: kqueueForgetFd,
  )
