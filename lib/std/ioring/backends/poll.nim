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
    of opConnect:
      # A non-blocking connect reports its outcome as writability.
      result.incl evWrite
    of opNop, opTimeout:
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
  from std/posix/posix import SockLen, EINPROGRESS

  # `posix.errno` is not usable here. Under `-d:nimNativeIo` it returns a
  # module-local variable that only that module's freestanding syscall
  # wrappers maintain (posix.nim says so itself), while everything the ring
  # calls — connect, read, write, accept — is a bare `importc` into libc and
  # sets *libc's* errno. Reading the wrong one is not a wrong error message,
  # it is a wrong branch: a failed connect reported `0` and completed as a
  # success. The ring already depends on libc for those calls, so it reads
  # libc's errno the way libc exposes it.
  when defined(osx):
    proc errnoLocation(): ptr cint {.importc: "__error", sideEffect.}
  else:
    proc errnoLocation(): ptr cint {.importc: "__errno_location", sideEffect.}
  proc sysErrno*(): cint {.inline.} = errnoLocation()[]

  proc posixRead(fd: cint; buf: nil pointer; count: int): int {.importc: "read".}
  proc posixWrite(fd: cint; buf: nil pointer; count: int): int {.importc: "write".}
  proc posixAccept(s: cint; `addr`: pointer; addrlen: ptr SockLen): cint {.importc: "accept".}
  proc getsockopt(s: cint; level, optname: cint; val: pointer;
                  vlen: ptr SockLen): cint {.importc: "getsockopt".}
  proc posixConnect(s: cint; name: pointer; namelen: SockLen): cint {.importc: "connect".}

  const
    SOL_SOCKET = (when defined(macosx): 0xFFFF.cint else: 1.cint)
    SO_ERROR = (when defined(macosx): 0x1007.cint else: 4.cint)

  proc startConnect*(fd: cint; idx: int): bool =
    ## Kick off a non-blocking connect on the op in slot `idx`. True when the
    ## attempt is under way and the poller should watch for writability.
    ##
    ## False means it is already over — and then this completes the slot,
    ## because nothing else will: an op that is never armed gets no readiness
    ## event, so leaving it here would park the caller until its deadline no
    ## matter how the connect actually went.
    let s = addr gSlots[ioLane()].slots[idx]
    let r = posixConnect(fd, addr s.op.sockAddr, s.op.sockAddrLen)
    if r == 0:
      complete(idx, 0)               # connected outright: loopback often does
      return false
    let e = sysErrno()
    if e == EINPROGRESS: return true
    complete(idx, -int(e))           # refused, unreachable, bad address …
    result = false

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
          var addrLen = s.op.sockAddrLen
          let clientFd = posixAccept(fd, addr s.op.sockAddr, addr addrLen)
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
      of opConnect:
        if evWrite in firedEvents:
          # Writability only says the attempt finished. `SO_ERROR` says how:
          # a refused connection is just as writable as an accepted one.
          var err: cint = 0
          var elen = SockLen(sizeof(err))
          if getsockopt(fd, SOL_SOCKET, SO_ERROR, addr err, addr elen) < 0:
            complete(j, -1)
          elif err != 0:
            complete(j, -int(err))
          else:
            complete(j, 0)
      of opNop, opTimeout:
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
  proc processFd*(fd: cint; firedEvents: IoEvents) {.nimcall.} =
    discard
