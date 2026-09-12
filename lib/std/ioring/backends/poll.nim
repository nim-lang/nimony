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
    of opRead, opAccept, opRecvFrom:
      result.incl evRead
    of opWrite, opSendTo:
      result.incl evWrite
    of opPollAdd:
      # Pure readiness probe: exactly the direction(s) the caller asked for.
      # Arming both regardless would wake a read-waiter on writability, and a
      # oneshot op re-armed on every spurious wake is a busy loop.
      result = result + gSlots[lane].slots[j].op.pollMask
    of opConnect:
      # A non-blocking connect reports its outcome as writability.
      result.incl evWrite
    of opNop, opTimeout, opOpen, opSocket, opSetSockOpt, opBind, opSetNonBlocking:
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

proc reArmOrTransfer(fd: cint; alreadyRegistered: bool) {.inline.} =
  ## The re-arm half of a submit or a delivered event: register `fd` for every
  ## direction still pending on it — or, when the backend will not deliver for
  ## it, satisfy those ops another way. A regular file is such a case: epoll
  ## refuses it outright (EPERM) and kqueue accepts the registration yet
  ## delivers at most a first event — a file must never be armed, because
  ## nothing after that first wake will ever come. Its read/write never blocks,
  ## so the transfer itself is the readiness and is performed here on the
  ## polling thread — exactly the role `processFd` plays for a descriptor that
  ## does deliver events. Anything else that cannot be armed never becomes
  ## ready, so its ops are failed.
  if transferIfRegularFile(fd): return
  if not reArmEvent(fd, armEventsForFd(fd), alreadyRegistered):
    failPendingForFd(fd)

proc submitForPoll*(fd: cint; alreadyRegistered: bool = false) {.nimcall.} =
  ## Arm `fd` for every op pending on it, including the one just allocated by
  ## the caller (`allocSlot` has already linked it into the fd's list).
  ##
  ## An op with no fd has nothing to arm, and arming anyway is not merely
  ## useless — the arena lists ops by fd, so every fd-less op shares the `-1`
  ## bucket. On epoll, `epoll_ctl` on `-1` fails with EBADF, which is read as
  ## "this fd will never deliver readiness" and fails *every* op in the
  ## bucket: one nop would complete every pending timer with an error. On
  ## kqueue the arm silently does nothing instead, so the same nop hangs
  ## forever. Neither is a bug the caller can do anything about, so fd-less
  ## ops do not come here at all.
  if fd < 0: return
  reArmOrTransfer(fd, alreadyRegistered)

when defined(posix):
  proc syncFileTransfers(fd: cint) {.inline.}

proc transferIfRegularFile(fd: cint): bool {.inline.} =
  ## True when `fd` names a regular file — nothing for the readiness backends
  ## to wait on — and its pending ops were satisfied by performing their
  ## transfers right here. Windows never reaches this: the ring's descriptors
  ## there are sockets, and files are served by the asyncio CRT arm without
  ## entering the ring at all.
  when defined(posix):
    result = isRegularFileFd(fd)
    if result:
      syncFileTransfers(fd)
  else:
    result = false

when defined(posix):
  import std / assertions
  from std/posix/posix import SockLen, FileHandle, EINPROGRESS, pcall, Mode, Stat, fstat, S_ISREG

  # No errno anywhere below. Every call the ring makes goes through
  # `posix.pcall`, which answers the raw Linux convention — the result, or
  # `-errno` — so a failure is a value the completion carries rather than a
  # global read at the right moment. That is also why the completions report
  # the actual error now instead of a flat `-1`.

  proc posixRead(fd: cint; buf: nil pointer; count: int): int {.importc: "read".}
  proc posixWrite(fd: cint; buf: nil pointer; count: int): int {.importc: "write".}
  proc posixAccept(s: cint; `addr`: pointer; addrlen: ptr SockLen): cint {.importc: "accept".}
  proc getsockopt(s: cint; level, optname: cint; val: pointer;
                  vlen: ptr SockLen): cint {.importc: "getsockopt".}
  proc posixConnect(s: cint; name: pointer; namelen: SockLen): cint {.importc: "connect".}
  proc posixSocket(domain, typ, proto: cint): cint {.importc: "socket".}
  proc posixSetsockopt(s: cint; level, optname: cint; val: nil pointer;
                       vlen: SockLen): cint {.importc: "setsockopt".}
  proc posixBind(s: cint; name: pointer; namelen: SockLen): cint {.importc: "bind".}
  proc posixFcntl(fd: cint; cmd: cint; arg: cint = 0): cint {.importc: "fcntl", sideEffect.}
    ## FIXED arity with a defaulted third argument, not `varargs`: the same
    ## shape and the same reason as iouring.nim's `fcntl` — nimony collapses
    ## same-named importc stubs, so a varargs fcntl would mono-morph into
    ## arities that fight over one register-signature stub. `F_GETFL` ignores
    ## the third argument, so passing `0` costs nothing.
  proc posixRecvfrom(s: cint; buf: nil pointer; count: int; flags: cint;
                     `addr`: pointer; addrlen: ptr SockLen): int {.importc: "recvfrom".}
  proc posixSendto(s: cint; buf: nil pointer; count: int; flags: cint;
                   `addr`: pointer; addrlen: SockLen): int {.importc: "sendto".}
  proc posixOpen(path: cstring; flags: cint): cint {.varargs, importc: "open", sideEffect.}
    ## A named `open`: `posix.open` and `syncio.open` clash wherever both are
    ## imported, and importing only the one this file needs would be a line of
    ## housekeeping for no benefit. `varargs` keeps the C prototype variadic,
    ## matching libc's `open` (`mode` passes through the `...`); a fixed-arity
    ## declaration instead lets the C compiler skip the variadic save area that
    ## libc's `open` reads `mode` from on AAPCS64 targets.

  proc completeOpen*(idx: int; path: cstring; flags: cint; mode: Mode) =
    ## The backend half of `submitOpen`: an `open` for an opOpen is performed
    ## here, on the polling thread, like every other command a backend runs —
    ## never by the caller, who would block on a filesystem-backed open. The
    ## fd (or `-errno`) is completed to the parked caller.
    complete(idx, int pcall(posixOpen(path, flags, mode)))

  const
    F_GETFL = 3.cint
    F_SETFL = 4.cint
    O_NONBLOCK = (when defined(linux): 0x0800.cint else: 0x0004.cint)

  proc completeSocket*(idx: int; domain, typ, proto: int32) =
    ## The backend half of `submitSocket`: socket(2) is one syscall with no
    ## readiness to wait on, so — exactly like `completeOpen` — the polling
    ## thread performs it and completes with the fd (or `-errno`). The flag
    ## just created has nothing to arm: its O_NONBLOCK comes from a ring op,
    ## not from waiting on it.
    complete(idx, int pcall(posixSocket(cint(domain), cint(typ), cint(proto))))

  proc completeSetSockOpt*(idx: int; fd: FileHandle; level, optName: int32;
                           optVal: nil pointer; optLen: SockLen) =
    ## The backend half of `submitSetSockOpt`: setsockopt(2) answers
    ## immediately (it is configuration, not I/O), so the polling thread makes
    ## the call and completes with `0` or `-errno`. The option value is read
    ## during this very call, long before the caller's frame could go away.
    complete(idx, int pcall(posixSetsockopt(fd, cint(level), cint(optName),
                                            optVal, optLen)))

  proc completeBind*(idx: int; fd: FileHandle; sa: pointer; saLen: SockLen) =
    ## The backend half of `submitBind`: bind(2) is one syscall the kernel
    ## answers at once — nothing to wait on, so the polling thread performs it
    ## and completes with `0` or `-errno`. The kernel copies the address out
    ## of `sa` during the call; it is not kept.
    complete(idx, int pcall(posixBind(fd, sa, saLen)))

  proc completeSetNonBlocking*(idx: int; fd: FileHandle) =
    ## The backend half of `submitSetNonBlocking`: fcntl(F_SETFL, … or
    ## O_NONBLOCK), two instant syscalls made here on the polling thread — a
    ## poller cannot watch for "not blocking", so this is done at issue, not
    ## awaited. Completes with `0` or `-errno`.
    let flags = pcall(posixFcntl(fd, F_GETFL))
    if flags >= 0:
      # `pcall` answers in `clong`; the flags are a small bit-mask, so the
      # cint narrowing is exact.
      complete(idx, int pcall(posixFcntl(fd, F_SETFL, cint(flags) or O_NONBLOCK)))
    else:
      complete(idx, int(flags))

  proc isRegularFileFd(fd: cint): bool =
    ## True when `fd` names a regular file. Nothing below gets to refuse a
    ## transfer because it cannot watch the descriptor — a regular file is not
    ## a bug in the caller's choice of I/O, it is a different readiness model.
    var st = default(Stat)
    if int(pcall(fstat(fd, st))) < 0: return false
    result = S_ISREG(st.st_mode)

  proc syncFileTransfers(fd: cint) =
    ## Perform, on the polling thread, the I/O of every op pending on a regular
    ## file `fd`. A regular file has no readiness to wait for — its read and
    ## write answer at once (never EAGAIN: the fd is not O_NONBLOCK) — so the
    ## transfer IS the event, and this is what `processFd` is for a descriptor
    ## that does deliver events. This runs instead of arming the fd: kqueue's
    ## registration of a file is accepted yet, after a first delivery, never
    ## fires again, so an armed file op would park forever on its second read.
    let lane = ioLane()
    for j in gSlots[lane].slotsForFd(fd):
      let s = addr gSlots[lane].slots[j]
      case s.op.kind
      of opRead:
        complete(j, int pcall(posixRead(fd, s.op.buf, s.op.len)))
      of opWrite:
        complete(j, int pcall(posixWrite(fd, s.op.buf, s.op.len)))
      of opPollAdd:
        # Always ready: a regular file stays readable (down to EOF) and
        # writable — there is no condition for the poller to wait on.
        complete(j, toEventMask(s.op.pollMask))
      else:
        discard   # opAccept/opConnect on a file is not a transfer we can make
    if gSlots[lane].hasPendingForFd(fd):
      failPendingForFd(fd)

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
    let r = int pcall(posixConnect(fd, addr s.op.sockAddr, s.op.sockAddrLen))
    if r == 0:
      complete(idx, 0)               # connected outright: loopback often does
      return false
    if r == -int(EINPROGRESS): return true
    complete(idx, r)                 # refused, unreachable, bad address …
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
          complete(j, int pcall(posixRead(fd, s.op.buf, s.op.len)))
      of opWrite:
        if evWrite in firedEvents:
          complete(j, int pcall(posixWrite(fd, s.op.buf, s.op.len)))
      of opAccept:
        if evRead in firedEvents:
          var addrLen = s.op.sockAddrLen
          let client = int pcall(posixAccept(fd, addr s.op.sockAddr, addr addrLen))
          # Write the length back. The kernel narrows it to what it actually
          # wrote, and `complete` hands the storage to the caller — a stale
          # `sizeof(sockaddr_storage)` here would describe a v4 address as
          # 128 bytes of one.
          s.op.sockAddrLen = addrLen
          complete(j, client)
      of opRecvFrom:
        if evRead in firedEvents:
          var addrLen = s.op.sockAddrLen
          let n = int pcall(posixRecvfrom(fd, s.op.buf, s.op.len, 0,
                                          addr s.op.sockAddr, addr addrLen))
          # Same narrowing as accept: `complete` hands the storage to `peer`,
          # so the length that describes it has to be what the kernel wrote.
          if n >= 0: s.op.sockAddrLen = addrLen
          complete(j, n)
      of opSendTo:
        if evWrite in firedEvents:
          complete(j, int pcall(posixSendto(fd, s.op.buf, s.op.len, 0,
                                            addr s.op.sockAddr, s.op.sockAddrLen)))
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
          let g = int pcall(getsockopt(fd, SOL_SOCKET, SO_ERROR, addr err, addr elen))
          if g < 0:
            complete(j, g)
          elif err != 0:
            complete(j, -int(err))
          else:
            complete(j, 0)
      of opNop, opTimeout, opOpen, opSocket, opSetSockOpt, opBind, opSetNonBlocking:
        discard
    # Re-arm for whatever directions still have an op pending on this fd
    # (completions above may have freed some slots already).
    if gSlots[lane].hasPendingForFd(fd):
      reArmOrTransfer(fd, true)
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
    WSAEINPROGRESS = 10036.cint
    WSAEALREADY = 10037.cint
    WSAEISCONN = 10056.cint
    SOL_SOCKET = 0xFFFF.cint
    SO_ERROR = 0x1007.cint

  # `buf` is `nil pointer` to match `OpContext.buf` (the POSIX arm declares its
  # read/write the same way): the op layout is nilable and the transfer takes
  # it as-is.
  proc wsRecv(s: SocketHandle; buf: nil pointer; len, flags: cint): cint {.
    stdcall, importc: "recv", dynlib: "ws2_32.dll".}
  proc wsSend(s: SocketHandle; buf: nil pointer; len, flags: cint): cint {.
    stdcall, importc: "send", dynlib: "ws2_32.dll".}
  proc wsRecvFrom(s: SocketHandle; buf: nil pointer; len, flags: cint;
                  name: pointer; namelen: ptr cint): cint {.
    stdcall, importc: "recvfrom", dynlib: "ws2_32.dll".}
  proc wsSendTo(s: SocketHandle; buf: nil pointer; len, flags: cint;
                name: pointer; namelen: cint): cint {.
    stdcall, importc: "sendto", dynlib: "ws2_32.dll".}
  proc wsAccept(s: SocketHandle; name: pointer; namelen: ptr cint): SocketHandle {.
    stdcall, importc: "accept", dynlib: "ws2_32.dll".}
  proc wsConnect(s: SocketHandle; name: pointer; namelen: cint): cint {.
    stdcall, importc: "connect", dynlib: "ws2_32.dll".}
  proc wsGetsockopt(s: SocketHandle; level, optname: cint; optval: pointer;
                    optlen: ptr cint): cint {.
    stdcall, importc: "getsockopt", dynlib: "ws2_32.dll".}
  proc wsaGetLastError(): cint {.
    stdcall, importc: "WSAGetLastError", dynlib: "ws2_32.dll".}
  proc wsSocket(af, typ, protocol: cint): SocketHandle {.
    stdcall, importc: "socket", dynlib: "ws2_32.dll".}
  proc wsSetsockopt(s: SocketHandle; level, optname: cint; optval: pointer;
                    optlen: cint): cint {.
    stdcall, importc: "setsockopt", dynlib: "ws2_32.dll".}
  proc wsBindS(s: SocketHandle; name: pointer; namelen: cint): cint {.
    stdcall, importc: "bind", dynlib: "ws2_32.dll".}
  proc wsIoctlsocket(s: SocketHandle; cmd: clong; argp: ptr culong): cint {.
    stdcall, importc: "ioctlsocket", dynlib: "ws2_32.dll".}
  const FIONBIO = cast[clong](0x8004667E'u32)   ## _IOW('f', 126, u_long)

  proc completeSocket*(idx: int; domain, typ, proto: int32) =
    ## Windows twin of the POSIX `completeSocket`: one instant syscall made on
    ## the polling thread; completes with the fd (or the negated Winsock code).
    let s = wsSocket(cint(domain), cint(typ), cint(proto))
    if s == InvalidSocket:
      complete(idx, -int(wsaGetLastError()))
    elif s > SocketHandle(high(cint)):
      discard wsClosesocket(s)      # the ring cannot hold the handle's cint
      complete(idx, -1)
    else:
      complete(idx, int(cast[uint32](s)))

  proc completeSetSockOpt*(idx: int; fd: FileHandle; level, optName: int32;
                           optVal: nil pointer; optLen: SockLen) =
    ## Windows twin of the POSIX `completeSetSockOpt`: one instant Winsock
    ## call; completes with `0` or the negated Winsock code.
    let r = wsSetsockopt(socketOf(fd), cint(level), cint(optName),
                         optVal, cint(optLen))
    if r == SocketError:
      complete(idx, -int(wsaGetLastError()))
    else:
      complete(idx, 0)

  proc completeBind*(idx: int; fd: FileHandle; sa: pointer; saLen: SockLen) =
    ## Windows twin of the POSIX `completeBind`: one instant Winsock call;
    ## completes with `0` or the negated Winsock code.
    let r = wsBindS(socketOf(fd), sa, cint(saLen))
    if r == SocketError:
      complete(idx, -int(wsaGetLastError()))
    else:
      complete(idx, 0)

  proc completeSetNonBlocking*(idx: int; fd: FileHandle) =
    ## Windows twin of the POSIX `completeSetNonBlocking`: ioctlsocket(FIONBIO);
    ## completes with `0` or the negated Winsock code.
    var one: culong = 1
    let r = wsIoctlsocket(socketOf(fd), FIONBIO, addr one)
    if r == SocketError:
      complete(idx, -int(wsaGetLastError()))
    else:
      complete(idx, 0)

  proc socketOf(fd: cint): SocketHandle {.inline.} =
    ## The ring narrows a SOCKET to `cint` (ioring.nim, Windows arm); widen it
    ## back without sign extension.
    SocketHandle(cast[uint32](fd))

  proc clampLen(n: int): cint {.inline.} =
    if n > int(high(cint)): high(cint) else: cint(n)

  proc wouldBlock(): bool {.inline.} =
    wsaGetLastError() == WSAEWOULDBLOCK

  proc startConnect*(fd: cint; idx: int): bool =
    ## Windows twin of the POSIX `startConnect`: kick off a non-blocking
    ## connect on the op in slot `idx`, true when the poller should now watch
    ## for writability. False means it is already over, and then this has
    ## completed the slot — an op that is never armed gets no readiness event,
    ## so leaving it would park the caller until its deadline however the
    ## connect actually went.
    ##
    ## The errors are negated Winsock codes (10035 …), not errnos: they do not
    ## share a numbering with the POSIX arm's, and the ring has no translation
    ## layer. A caller that must tell "refused" from "unreachable" apart on
    ## both platforms has to ask per-platform.
    let sl = addr gSlots[ioLane()].slots[idx]
    let r = wsConnect(socketOf(fd), addr sl.op.sockAddr, cint(sl.op.sockAddrLen))
    if r != SocketError:
      complete(idx, 0)               # connected outright: loopback often does
      return false
    let e = wsaGetLastError()
    if e == WSAEWOULDBLOCK or e == WSAEINPROGRESS or e == WSAEALREADY:
      return true
    if e == WSAEISCONN:
      complete(idx, 0)
      return false
    complete(idx, -int(e))           # refused, unreachable, bad address ...
    result = false

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
          var addrLen = cint(sl.op.sockAddrLen)
          let client = wsAccept(s, addr sl.op.sockAddr, addr addrLen)
          if client == InvalidSocket:
            if not wouldBlock(): complete(j, -1)
          else:
            sl.op.sockAddrLen = SockLen(addrLen)
            # The accepted SOCKET must survive the cint narrowing the ring's
            # API imposes; kernel handle values are small in practice (see
            # ioring.nim's Windows `listenTcp`).
            complete(j, int(cast[uint32](client)))
      of opRecvFrom:
        if evRead in firedEvents:
          var addrLen = cint(sl.op.sockAddrLen)
          let r = wsRecvFrom(s, sl.op.buf, clampLen(sl.op.len), 0.cint,
                             addr sl.op.sockAddr, addr addrLen)
          if r == SocketError:
            if not wouldBlock():
              complete(j, -1)
          else:
            sl.op.sockAddrLen = SockLen(addrLen)
            complete(j, int(r))
      of opSendTo:
        if evWrite in firedEvents:
          let r = wsSendTo(s, sl.op.buf, clampLen(sl.op.len), 0.cint,
                           addr sl.op.sockAddr, cint(sl.op.sockAddrLen))
          if r == SocketError:
            if not wouldBlock(): complete(j, -1)
          else:
            complete(j, int(r))
      of opPollAdd:
        let hit = firedEvents * sl.op.pollMask
        if hit != {}:
          complete(j, toEventMask(hit))
      of opConnect:
        if evWrite in firedEvents:
          # Writability only says the attempt finished. `SO_ERROR` says how:
          # a refused connection is just as writable as an accepted one.
          #
          # Winsock signals a *failed* connect in the exception set, which
          # WSAPoll reports as POLLERR — and does not, before Windows 10 2004
          # (the caveat in the backend header). On such a host the failure is
          # noticed when the deadline blows rather than at once, which is why
          # `submitConnect` insists on one.
          var err: cint = 0
          var elen = cint(sizeof(err))
          if wsGetsockopt(s, SOL_SOCKET, SO_ERROR, addr err, addr elen) != 0:
            complete(j, -int(wsaGetLastError()))
          elif err != 0:
            complete(j, -int(err))
          else:
            complete(j, 0)
      of opNop, opTimeout, opOpen, opSocket, opSetSockOpt, opBind, opSetNonBlocking:
        discard
    if gSlots[lane].hasPendingForFd(fd):
      if not reArmEvent(fd, armEventsForFd(fd), true):
        failPendingForFd(fd)
