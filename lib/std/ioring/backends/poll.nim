# Shared poll-based backend helpers (epoll / kqueue).
# Provides submitForPoll() and processFd() for both epoll and kqueue backends.
# The global reArmEvent proc is set by each backend's init to dispatch to its
# own platform-specific implementation.

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
  
proc armMaskForFd*(fd: cint): int =
  ## The union of the directions every op currently pending on `fd` waits for.
  ##
  ## It has to be the union, never one op's own direction: the registration is
  ## keyed by fd, and epoll's `EPOLL_CTL_MOD` *replaces* the interest set. Arming
  ## with just the newest op's direction therefore silently disarms the others —
  ## `submitWrite(fd)` followed by `submitRead(fd)` would leave the fd watched
  ## for EPOLLIN only and the pending write would never be woken.
  result = 0
  let lane = ioLane()
  for j in gSlots[lane].slotsForFd(fd):
    case gSlots[lane].slots[j].op.kind
    of opRead, opAccept:
      result = result or EvRead
    of opWrite:
      result = result or EvWrite
    of opPollAdd:
      # Pure readiness probe: exactly the direction(s) the caller asked for.
      # Arming both regardless would wake a read-waiter on writability, and a
      # oneshot op re-armed on every spurious wake is a busy loop.
      result = result or gSlots[lane].slots[j].op.pollMask
    of opNop:
      discard

proc submitForPoll*(fd: cint; alreadyRegistered: bool = false) {.nimcall.} =
  ## Arm `fd` for every op pending on it, including the one just allocated by
  ## the caller (`allocSlot` has already linked it into the fd's list).
  reArmEvent(fd, armMaskForFd(fd), alreadyRegistered)

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
    let lane = ioLane()
    for j in gSlots[lane].slotsForFd(fd):
      let s = addr gSlots[lane].slots[j]
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
        #
        # Only the directions this op asked for count. A wake for a direction
        # it did not request leaves the slot pending, and the re-arm below
        # keeps watching for the one it did.
        let hit = firedEvents and s.op.pollMask
        if hit != 0:
          complete(j, hit)
      of opNop:
        discard
    # Re-arm for whatever directions still have an op pending on this fd
    # (completions above may have freed some slots already).
    if gSlots[lane].hasPendingForFd(fd):
      reArmEvent(fd, armMaskForFd(fd), true)
    # else: nothing left for this fd; the backend already consumed the
    # one-shot registration, and submit/registerEvent will re-add it the
    # next time an op targets this fd.
else:
  proc processFd*(fd: cint; firedEvents: int) {.nimcall.} =
    discard
