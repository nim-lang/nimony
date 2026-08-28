# Shared poll-based backend helpers (epoll / kqueue).
# Provides submitForPoll() and processFd() for both epoll and kqueue backends.
# The global reArmEvent proc is set by each backend's init to dispatch to its
# own platform-specific implementation.

import std/threadpool

import ../core/types
import ../core/slots
import ../core/backend

proc noopReArm(fd: cint; mask: int, alreadyRegistered: bool) {.nimcall.} = discard
var reArmEvent*: proc (fd: cint; mask: int, alreadyRegistered: bool) {.nimcall.} = noopReArm
## Registers (or re-arms, for EPOLLONESHOT-style backends) readiness
## interest for `fd`. Takes only `fd` and the direction mask — never a
## specific slot index: a fd can have several ops in flight (e.g. a
## pending read *and* a pending write) sharing one epoll/kqueue
## registration, so the registration is keyed by fd, not by any one op.
  
proc submitForPoll*(slotIdx: int; op: ptr OpContext; alreadyRegistered: bool = false) {.nimcall.} =
  var mask = 0
  if op.kind == opRead or op.kind == opAccept:
    mask = mask or EvRead
  if op.kind == opWrite:
    mask = mask or EvWrite
  if op.kind == opPollAdd:
    # Pure readiness probe: interested in either direction so the caller gets
    # one notification per re-arm telling it which way the fd became ready.
    mask = EvRead or EvWrite
  reArmEvent(op.fd, mask, alreadyRegistered)

when defined(posix):
  import std / assertions
  from std/posix/posix import SockLen

  proc posixRead(fd: cint; buf: nil pointer; count: int): int {.importc: "read".}
  proc posixWrite(fd: cint; buf: nil pointer; count: int): int {.importc: "write".}
  proc posixAccept(s: cint; `addr`: pointer; addrlen: ptr SockLen): cint {.importc: "accept".}

  proc processFd*(fd: cint; firedEvents: int) {.nimcall.} =
    ## Dispatch every pending op on `fd` whose direction actually matches the
    ## readiness that just fired. `firedEvents` (EvRead/EvWrite, as delivered
    ## by the poller) is authoritative: a write-readiness wakeup must not
    ## drive a still-pending *read* op (and vice versa) — the fd may be
    ## registered for both directions at once (e.g. a socket with an
    ## in-flight read and an in-flight write), and only the direction that
    ## actually fired has data ready / a free send buffer.
    # O(k) in the number of ops on this fd, via the intrusive per-fd list,
    # instead of an O(MaxOps) scan of the whole arena.
    for j in gSlots[threadIdx].slotsForFd(fd):
      let s = addr gSlots[threadIdx].slots[j]
      case s.op.kind
      of opRead:
        if (firedEvents and EvRead) != 0:
          let r = posixRead(fd, s.op.buf, s.op.len)
          complete(j, if r >= 0: r else: -1)
      of opWrite:
        if (firedEvents and EvWrite) != 0:
          let r = posixWrite(fd, s.op.buf, s.op.len)
          complete(j, if r >= 0: r else: -1)
      of opAccept:
        if (firedEvents and EvRead) != 0:
          var addrLen = s.op.acceptLen
          let clientFd = posixAccept(fd, addr s.op.acceptAddr, addr addrLen)
          complete(j, if clientFd >= 0: clientFd else: -1)
      of opPollAdd:
        # Pure readiness notification: no I/O, just report which direction(s)
        # fired so the caller (e.g. libcurl's multi-socket engine) can decide
        # what to do next. The slot is freed by `complete`, so the caller
        # re-submits to re-arm (oneshot).
        complete(j, firedEvents and (EvRead or EvWrite))
      of opNop:
        discard
    # Re-arm for whatever directions still have an op pending on this fd
    # (completions above may have freed some slots already).
    var armMask = 0
    var stillPending = false
    for j in gSlots[threadIdx].slotsForFd(fd):
      stillPending = true
      let sk = gSlots[threadIdx].slots[j].op.kind
      if sk == opRead or sk == opAccept:
        armMask = armMask or EvRead
      if sk == opWrite:
        armMask = armMask or EvWrite
    if stillPending:
      reArmEvent(fd, armMask, true)
    # else: nothing left for this fd; the backend already consumed the
    # one-shot registration, and submit/registerEvent will re-add it the
    # next time an op targets this fd.
else:
  proc processFd*(fd: cint; firedEvents: int) {.nimcall.} =
    discard
