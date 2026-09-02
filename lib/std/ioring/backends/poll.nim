# Shared poll-based backend helpers (epoll / kqueue).
# Provides submitForPoll() and processFd() for both epoll and kqueue backends.
# The global reArmEvent proc is set by each backend's init to dispatch to its
# own platform-specific implementation.

import ../core/types
import ../core/slots
import ../core/backend

proc noopReArm(fd: cint; events: IoEvents, alreadyRegistered: bool): bool {.nimcall.} = true
var reArmEvent*: proc (fd: cint; events: IoEvents, alreadyRegistered: bool): bool {.nimcall.} = noopReArm
## Registers (or re-arms, for EPOLLONESHOT-style backends) readiness
## interest for `fd`. Takes only `fd` and the directions — never a
## specific slot index: a fd can have several ops in flight (e.g. a
## pending read *and* a pending write) sharing one epoll/kqueue
## registration, so the registration is keyed by fd, not by any one op.

proc armEventsForFd*(fd: cint): IoEvents =
  ## The union of the directions every op currently pending on `fd` waits for.
  ##
  ## It has to be the union, never one op's own direction: the registration is
  ## keyed by fd, and epoll's `EPOLL_CTL_MOD` *replaces* the interest set. Arming
  ## with just the newest op's direction therefore silently disarms the others —
  ## `submitWrite(fd)` followed by `submitRead(fd)` would leave the fd watched
  ## for EPOLLIN only and the pending write would never be woken.
  result = {}
  let lane = ioLane()
  for j in gSlots[lane].slotsForFd(fd):
    case gSlots[lane].slots[j].op.kind
    of opRead, opAccept:
      result.incl evRead
    of opWrite:
      result.incl evWrite
    of opPollAdd:
      # Pure readiness probe: exactly the direction(s) the caller asked for.
      # Arming both regardless would wake a read-waiter on writability, and a
      # oneshot op re-armed on every spurious wake is a busy loop.
      result = result + gSlots[lane].slots[j].op.pollMask
    of opNop:
      discard

const ArmFailed* = -1
  ## Completion result for an op on a fd that could not be armed — the same
  ## value a failed read or write reports.

proc failPendingForFd*(fd: cint) =
  ## Complete every op pending on `fd`. Nothing will make them ready once the
  ## fd cannot be armed, so otherwise they park forever.
  let lane = ioLane()
  for j in gSlots[lane].slotsForFd(fd):
    complete(j, ArmFailed)

proc submitForPoll*(fd: cint; alreadyRegistered: bool = false) {.nimcall.} =
  ## Arm `fd` for every op pending on it, including the one just allocated by
  ## the caller (`allocSlot` has already linked it into the fd's list).
  if not reArmEvent(fd, armEventsForFd(fd), alreadyRegistered):
    failPendingForFd(fd)

when defined(posix):
  import std / assertions
  from std/posix/posix import SockLen

  proc posixRead(fd: cint; buf: nil pointer; count: int): int {.importc: "read".}
  proc posixWrite(fd: cint; buf: nil pointer; count: int): int {.importc: "write".}
  proc posixAccept(s: cint; `addr`: pointer; addrlen: ptr SockLen): cint {.importc: "accept".}

  proc processFd*(fd: cint; firedEvents: IoEvents) {.nimcall.} =
    ## Dispatch every pending op on `fd` whose direction actually matches the
    ## readiness that just fired. `firedEvents` (as delivered by the poller)
    ## is authoritative: a write-readiness wakeup must not drive a
    ## still-pending *read* op (and vice versa) — the fd may be registered for
    ## both directions at once (e.g. a socket with an in-flight read and an
    ## in-flight write), and only the direction that actually fired has data
    ## ready / a free send buffer.
    # O(k) in the number of ops on this fd, via the intrusive per-fd list,
    # instead of an O(MaxOps) scan of the whole arena.
    let lane = ioLane()
    for j in gSlots[lane].slotsForFd(fd):
      let s = addr gSlots[lane].slots[j]
      case s.op.kind
      of opRead:
        if evRead in firedEvents:
          let r = posixRead(fd, s.op.buf, s.op.len)
          complete(j, if r >= 0: r else: -1)
      of opWrite:
        if evWrite in firedEvents:
          let r = posixWrite(fd, s.op.buf, s.op.len)
          complete(j, if r >= 0: r else: -1)
      of opAccept:
        if evRead in firedEvents:
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
        let hit = firedEvents * s.op.pollMask
        if hit != {}:
          complete(j, toEventMask(hit))
      of opNop:
        discard
    # Re-arm for whatever directions still have an op pending on this fd
    # (completions above may have freed some slots already).
    if gSlots[lane].hasPendingForFd(fd):
      if not reArmEvent(fd, armEventsForFd(fd), true):
        failPendingForFd(fd)
    # else: nothing left for this fd; the backend already consumed the
    # one-shot registration, and submit/registerEvent will re-add it the
    # next time an op targets this fd.
else:
  # Windows (the WSAPoll backend): the transfer calls are Winsock's, bound by
  # `dynlib` in the winlean house style so no `<winsock2.h>` has to be ordered
  # against `<Windows.h>` in the generated C.
  #
  # One deliberate difference from the POSIX arm: a `WSAEWOULDBLOCK` on a
  # readiness wake is not a failure. The ready state was consumed by another
  # op on the fd (or by another lane polling the same socket), so the op stays
  # pending and is re-armed below instead of completing with -1.
  type
    SocketHandle = uint   ## Winsock SOCKET (UINT_PTR)

  const
    SocketError = -1.cint
    InvalidSocket = not 0'u
    WSAEWOULDBLOCK = 10035.cint

  # `buf` is `nil pointer` to match `OpContext.buf` (the POSIX arm declares its
  # read/write the same way): the op layout is nilable and the transfer takes
  # it as-is.
  proc wsRecv(s: SocketHandle; buf: nil pointer; len, flags: cint): cint {.
    stdcall, importc: "recv", dynlib: "ws2_32.dll".}
  proc wsSend(s: SocketHandle; buf: nil pointer; len, flags: cint): cint {.
    stdcall, importc: "send", dynlib: "ws2_32.dll".}
  proc wsAccept(s: SocketHandle; name: pointer; namelen: ptr cint): SocketHandle {.
    stdcall, importc: "accept", dynlib: "ws2_32.dll".}
  proc wsaGetLastError(): cint {.
    stdcall, importc: "WSAGetLastError", dynlib: "ws2_32.dll".}

  proc socketOf(fd: cint): SocketHandle {.inline.} =
    ## The ring narrows a SOCKET to `cint` (ioring.nim, Windows arm); widen it
    ## back without sign extension.
    SocketHandle(cast[uint32](fd))

  proc clampLen(n: int): cint {.inline.} =
    if n > int(high(cint)): high(cint) else: cint(n)

  proc wouldBlock(): bool {.inline.} =
    wsaGetLastError() == WSAEWOULDBLOCK

  proc processFd*(fd: cint; firedEvents: IoEvents) {.nimcall.} =
    ## Windows twin of the POSIX `processFd` above: same per-direction
    ## dispatch over the fd's in-flight ops, Winsock transfers.
    let lane = ioLane()
    let s = socketOf(fd)
    for j in gSlots[lane].slotsForFd(fd):
      let sl = addr gSlots[lane].slots[j]
      case sl.op.kind
      of opRead:
        if evRead in firedEvents:
          let r = wsRecv(s, sl.op.buf, clampLen(sl.op.len), 0.cint)
          if r == SocketError:
            if not wouldBlock(): complete(j, -1)
          else:
            complete(j, int(r))
      of opWrite:
        if evWrite in firedEvents:
          let r = wsSend(s, sl.op.buf, clampLen(sl.op.len), 0.cint)
          if r == SocketError:
            if not wouldBlock(): complete(j, -1)
          else:
            complete(j, int(r))
      of opAccept:
        if evRead in firedEvents:
          var addrLen = cint(sl.op.acceptLen)
          let client = wsAccept(s, addr sl.op.acceptAddr, addr addrLen)
          if client == InvalidSocket:
            if not wouldBlock(): complete(j, -1)
          else:
            # The accepted SOCKET must survive the cint narrowing the ring's
            # API imposes; kernel handle values are small in practice (see
            # ioring.nim's Windows `listenTcp`).
            complete(j, int(cast[uint32](client)))
      of opPollAdd:
        let hit = firedEvents * sl.op.pollMask
        if hit != {}:
          complete(j, toEventMask(hit))
      of opNop:
        discard
    if gSlots[lane].hasPendingForFd(fd):
      if not reArmEvent(fd, armEventsForFd(fd), true):
        failPendingForFd(fd)
