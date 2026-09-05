# (c) 2025 Andreas Rumpf
# Shared completion-based I/O ring on top of threadpool.
#
# Any thread can submit I/O requests; completions are delivered either
# by resuming a suspended `.passive` proc (via continuation) or by
# pushing to a shared completion queue for polling.
#
# Usage:
#   initIoRing()
#   let listenFd = listenTcp(8080)
#   discard submitAccept(listenFd)
#   var comps: array[16, IoCompletion]
#   let n = waitCompletions(comps)
#   echo "client fd=", comps[0].result
#   shutdown()

import std / [atomics, threadpool, assertions, ticketlocks]
import ./ioring/core/[types, slots, backend]
export types.IoCompletion, types.IoOp, types.SeqNum, types.OpContext
export types.IoEvent, types.IoEvents, types.evRead, types.evWrite
export types.readyEvents, types.toIoEvents, types.toEventMask, types.ECancelled
export types.Deadline, types.never, types.earlier, types.monoNow
export types.after, types.afterMs, types.millisUntil, types.`<`, types.`<=`
export types.`==`
export backend.IoTimedOut
export backend.BackendRelays, backend.CqSize, backend.MaxOps
import ./ioring/platform
when defined(windows):
  when not defined(nimIoringWsaPoll):
    import ./ioring/backends/iocp   # iocpOwnerLane / iocpWake — lane routing
when defined(posix):
  from std/posix/posix import Sockaddr_storage, Sockaddr_in, SockLen, FileHandle,
                              SockAddr, InAddr, TSa_Family

var ringState: int = 0

when defined(windows):
  proc wsaStartup(wVersionRequested: uint16; lpWSAData: pointer): cint {.
    stdcall, importc: "WSAStartup", dynlib: "ws2_32.dll".}

  proc initWinsock() =
    ## Winsock 2.2, once per process. WSADATA is 408 bytes on x64; the buffer
    ## is oversized so a header revision cannot overrun it.
    var data = default(array[512, byte])
    discard wsaStartup(0x0202'u16, addr data[0])

proc setupRing() =
  initPool()
  when defined(windows):
    initWinsock()
  initOpQueues()
  initSlots()
  initTimers()
  gCq = newSeq[IoCompletion](CqSize)
  initPlatformBackend()
  gReactor = backendRelays.poll
  gReactorWaits = backendRelays.waits

proc initIoRing*() =
  ## Bring the default ring up. **Idempotent**, and it has to be: this module
  ## already initialises the ring at import time, so a second call — the usage
  ## example above tells callers to make one — would otherwise re-run
  ## `initOpQueues`/`initSlots`/`gCq = newSeq` while worker threads are live
  ## inside `poll`, holding indices into the seqs being replaced. That is a
  ## use-after-free plus the loss of every op in flight.
  if atomicLoad(ringState, moAcquire) == 2: return
  var expected = 0
  if atomicCompareExchange(ringState, expected, 1):
    setupRing()
    atomicStore(ringState, 2, moRelease)
  else:
    while atomicLoad(ringState, moAcquire) != 2:
      discard

proc shutdown*() =
  ## Stop the pool *first*, then tear the backend down: `close` closes the
  ## epoll/kqueue/io_uring descriptors the workers poll, so closing them while
  ## a worker is still inside `poll` leaves it waiting on — or re-registering
  ## against — a descriptor number the OS is free to hand to something else.
  ##
  ## Call it from a non-worker thread: it joins the workers, and a worker that
  ## joins itself deadlocks. It also stops the pool for everyone (`std/parfor`
  ## included), and the ring cannot be brought back up afterwards.
  shutdownPool()
  backendRelays.close()

proc nextSeqNum(): SeqNum =
  SeqNum(atomicFetchAdd(gNextSeq, 1'u32, moRelaxed))

proc enqueueOp(op: OpContext) =
  ## Non-lossy backpressure mirroring the task queue's "caller-runs"
  ## (threadpool.nim:69-95). When this thread's stripe is full, help drain it
  ## like a worker would — poll also processes completions and frees queue
  ## slots — then retry. The op is guaranteed to be accepted, so a
  ## continuation can never park forever on a dropped submission.
  ##
  ## `tryEnqueue` copies `op` by value and only consumes it on success
  ## (stripes.nim), so retrying with the same op is safe, and polling from a
  ## non-worker thread is the same pattern `waitCompletions` already uses.
  when hasIocp:
    # A socket belongs to the lane whose completion port it is bound to
    # (backends/iocp.nim): its ops must be issued there. Stripes are
    # lock-protected, so a foreign lane can enqueue; it then wakes the owner
    # out of its completion wait. The first op for a socket claims it for the
    # submitting lane.
    var lane = ioLane()
    if op.fd >= 0:
      let owner = iocpOwnerLane(op.fd)
      if owner >= 0: lane = owner
    while not gOpQueues[lane].tryEnqueue(op):
      discard backendRelays.poll(0)
    if lane != ioLane():
      iocpWake(lane)
  else:
    while not gOpQueues[ioLane()].tryEnqueue(op):
      discard backendRelays.poll(0)

proc submitNop*(deadline: Deadline; cont = Continuation(fn: nil, env: nil);
                resPtr: nil ptr int = nil): SeqNum =
  result = nextSeqNum()
  var op = OpContext(kind: opNop, fd: -1, seqnum: result,
    cont: cont, res: cast[int](resPtr), deadline: deadline)
  enqueueOp(op)

proc submitTimeout*(deadline: Deadline;
                    cont = Continuation(fn: nil, env: nil);
                    resPtr: nil ptr int = nil): SeqNum =
  ## Complete once `deadline` passes, with no I/O at all. Unlike every other
  ## op, reaching the deadline is this one's *success*: it completes with `0`
  ## rather than `IoTimedOut`.
  ##
  ## This is how a loop gets a turn on a schedule — `next(e, deadline)` in the
  ## HTTP design is one of these plus whatever else is pending.
  result = nextSeqNum()
  var op = OpContext(kind: opTimeout, fd: -1, seqnum: result,
    cont: cont, res: cast[int](resPtr), deadline: deadline)
  enqueueOp(op)

proc submitRead*(fd: cint; buf: pointer; len: int; deadline: Deadline;
                 cont = Continuation(fn: nil, env: nil);
                 resPtr: nil ptr int = nil): SeqNum =
  result = nextSeqNum()
  var op = OpContext(kind: opRead, fd: fd, seqnum: result, buf: buf, len: len,
    cont: cont, res: cast[int](resPtr), deadline: deadline)
  enqueueOp(op)

proc submitWrite*(fd: cint; buf: pointer; len: int; deadline: Deadline;
                 cont = Continuation(fn: nil, env: nil);
                 resPtr: nil ptr int = nil): SeqNum =
  result = nextSeqNum()
  var op = OpContext(kind: opWrite, fd: fd, seqnum: result, buf: buf, len: len,
    cont: cont, res: cast[int](resPtr), deadline: deadline)
  enqueueOp(op)

proc submitAccept*(listenFd: cint; deadline: Deadline;
                   cont = Continuation(fn: nil, env: nil);
                   resPtr: nil ptr int = nil): SeqNum =
  result = nextSeqNum()
  var op = OpContext(kind: opAccept, fd: listenFd, seqnum: result,
    cont: cont, res: cast[int](resPtr), deadline: deadline)
  op.sockAddr = Sockaddr_storage()
  op.sockAddrLen = SockLen(sizeof(op.sockAddr))
  enqueueOp(op)

proc submitConnect*(fd: cint; sa: Sockaddr_storage; saLen: SockLen;
                    deadline: Deadline;
                    cont = Continuation(fn: nil, env: nil);
                    resPtr: nil ptr int = nil): SeqNum =
  ## Connect `fd` to `sa`. Completes with `0` on success, or the negated
  ## errno — a refused connection is `-ECONNREFUSED`, not a generic -1,
  ## because the caller usually wants to tell "nobody listening" from "the
  ## network ate it".
  ##
  ## `fd` must already be non-blocking (`setNonBlocking`). The attempt is
  ## started on the polling thread, not here, so that the fd is being watched
  ## from the moment it is connecting.
  ##
  ## A connect with no deadline is the classic way to hold a slot forever: a
  ## SYN into a black hole never answers. Hence the parameter, and hence no
  ## default for it.
  result = nextSeqNum()
  var op = OpContext(kind: opConnect, fd: fd, seqnum: result,
    cont: cont, res: cast[int](resPtr), deadline: deadline)
  op.sockAddr = sa
  op.sockAddrLen = saLen
  enqueueOp(op)

proc submitPollAdd*(fd: cint; deadline: Deadline;
                    events: IoEvents = {evRead, evWrite};
                    cont = Continuation(fn: nil, env: nil);
                    resPtr: nil ptr int = nil): SeqNum =
  ## Register oneshot readiness interest in `fd` without issuing any I/O.
  ## When the fd becomes ready in one of the `events` directions a single
  ## completion fires whose `op` is `opPollAdd` and whose `readyEvents` are the
  ## directions that fired. Unlike `submitRead`/`submitWrite`, no transfer is
  ## performed — the caller decides what to do with the ready fd (e.g.
  ## libcurl's multi-socket engine). This is oneshot: `complete` frees the
  ## slot, so re-arm by calling `submitPollAdd` again after handling the event.
  ##
  ## **Pass the direction you actually want.** The default watches both, which
  ## is right for a probe with no preference — but a caller waiting to *read*
  ## a socket is woken by mere writability on every arm (a connected socket is
  ## writable nearly always), and because the op is oneshot its re-arm then
  ## spins as fast as the loop can poll. libcurl's multi-socket engine always
  ## states its direction (`CURL_POLL_IN`/`CURL_POLL_OUT`); pass it through.
  ##
  ## A `resPtr` receives the same directions as a bit mask instead of a set,
  ## being a `ptr int`; decode it with `toIoEvents`.
  result = nextSeqNum()
  var op = OpContext(kind: opPollAdd, fd: fd, seqnum: result,
    cont: cont, res: cast[int](resPtr), pollMask: events, deadline: deadline)
  enqueueOp(op)

proc pollCompletions*(comps: var openArray[IoCompletion]): int =
  ## Non-blocking: drive this lane's backend once — issue the ops queued on
  ## it and collect whatever the kernel has finished — then hand back up to
  ## `comps.len` completions from the shared queue. Returns 0 when nothing has
  ## completed. It used to only drain the queue, so a caller that submitted
  ## and then polled had not issued anything, and a `closeFd` in between
  ## found no slot to cancel.
  discard backendRelays.poll(0)
  result = 0
  gCqLock.acquire()
  while result < comps.len and gCqCount > 0:
    comps[result] = gCq[gCqHead]
    gCqHead = (gCqHead + 1) and (CqSize - 1)
    dec gCqCount
    inc result
  gCqLock.release()

proc waitCompletions*(comps: var openArray[IoCompletion]): int =
  ## `pollCompletions` until at least one completion has landed.
  result = 0
  while true:
    result = pollCompletions(comps)
    if result > 0: return

proc cancelPendingOps(fd: cint) =
  ## The platform-neutral half of `closeFd`: cancel any ops still in flight on
  ## `fd` so their continuations are resumed (with a cancellation result)
  ## instead of leaking, and deregister the fd from the backend — all BEFORE
  ## the actual close. Previously `closeFd` only called close(2): the backend
  ## never found out (so epoll/kqueue kept a registration for a possibly-reused
  ## fd number) and any pending slot for this fd stayed in use forever — a
  ## permanent slot-arena leak for every fd closed with an op in flight.
  ##
  ## Order matters: deregister from the backend *before* the close, so a fresh
  ## fd that the OS immediately reuses for the same number cannot race with a
  ## stale registration/slot that still refers to it.
  ##
  ## **Scope: this thread's lane only** (see `ioLane`). Slot arenas are
  ## per-lane and unlocked, so a fd must be closed from the same thread that
  ## submitted its ops; ops another lane still holds for `fd` are not
  ## cancelled here and would leak the way described above. Cancelling those
  ## needs a cross-lane request the owning lane drains from its own `poll`
  ## (and, on io_uring, an `IORING_OP_ASYNC_CANCEL` — the kernel still owns
  ## the slot's buffers until it acknowledges), which this does not do yet.
  let lane = ioLane()
  if backendRelays.forgetFd != nil:
    backendRelays.forgetFd(fd)
  for idx in gSlots[lane].slotsForFd(fd):
    # The shared completion path: writes `ECancelled`, resumes the continuation
    # or — for an op without one — pushes a completion-queue entry, then frees
    # the slot. This used to resume continuations only, so a cancelled op that
    # had none (a `waitCompletions` driver) vanished and its waiter hung.
    complete(idx, ECancelled)

proc htons(x: uint16): uint16 {.inline.} =
  ## Header macro/libc shim; a byte swap on the little-endian targets.
  when defined(bigEndian):
    result = x
  else:
    result = (x shl 8) or (x shr 8)

when defined(posix):
  proc posixClose(fd: cint): cint {.importc: "close".}
  proc fcntl(fd: cint; cmd: cint): cint {.varargs, importc.}
  const F_GETFL* = 3.cint
  const F_SETFL* = 4.cint
  when defined(linux):
    const O_NONBLOCK* = 0x0800.cint
  else:
    const O_NONBLOCK* = 0x0004.cint
  proc setNonBlocking*(fd: cint) =
    var flags = fcntl(fd, F_GETFL)
    discard fcntl(fd, F_SETFL, flags or O_NONBLOCK)
  proc closeFdRaw*(fd: cint) =
    discard posixClose(fd)

  proc closeFd*(fd: cint) =
    ## Close `fd`: cancel this lane's in-flight ops on it (see
    ## `cancelPendingOps`), deregister it from the backend, then close(2).
    cancelPendingOps(fd)
    discard posixClose(fd)

when defined(posix):
  const
    AF_INET* = 2.cint
    SOCK_STREAM* = 1.cint
    IPPROTO_TCP* = 6.cint
    SOL_SOCKET* = (when defined(macosx): 0xFFFF.cint else: 1.cint)
    SO_REUSEADDR* = (when defined(macosx): 4.cint else: 2.cint)
    INADDR_ANY* = 0'u32
  proc socket(domain, typ, protocol: cint): cint {.importc: "socket".}
  proc setsockopt(s: cint; level, optname: cint; val: pointer; vlen: SockLen): cint {.importc: "setsockopt".}
  proc bindAddr(s: cint; name: ptr SockAddr; namelen: SockLen): cint {.importc: "bind".}
  proc listen(s: cint; backlog: cint): cint {.importc: "listen".}
  proc socketNonBlocking*(): cint =
    ## A non-blocking TCP socket, which is what `submitConnect` requires: a
    ## blocking one would finish the connect inside the syscall and there
    ## would be nothing for the ring to wait on.
    result = socket(AF_INET, SOCK_STREAM, IPPROTO_TCP)
    if result >= 0: setNonBlocking(result)

  proc loopbackAddr*(sa: var Sockaddr_storage; saLen: var SockLen;
                     port: uint16) =
    ## Fill `sa` with `127.0.0.1:port`, ready for `submitConnect`.
    const Loopback =
      when defined(bigEndian): 0x7F000001'u32 else: 0x0100007F'u32
    var a4 = default(Sockaddr_in)
    a4.sin_family = TSa_Family(AF_INET)
    a4.sin_port = htons(port)
    a4.sin_addr.s_addr = Loopback
    sa = default(Sockaddr_storage)
    copyMem(addr sa, addr a4, sizeof(a4))
    saLen = SockLen(sizeof(a4))

  proc getsockname(s: cint; name: ptr SockAddr; namelen: ptr SockLen): cint {.importc: "getsockname".}

  proc boundPort*(fd: cint): uint16 =
    ## The port `fd` is actually bound to. With `listenTcp(0)` the kernel picks
    ## one, and asking for it afterwards is the only way a test can listen
    ## without inventing a fixed number that a parallel run — or a socket still
    ## in TIME_WAIT — will collide with.
    var sa = default(Sockaddr_storage)
    var slen = SockLen(sizeof(sa))
    if getsockname(fd, cast[ptr SockAddr](addr sa), addr slen) != 0: return 0'u16
    let raw = cast[ptr UncheckedArray[uint8]](addr sa)
    # Network byte order, read as bytes so no host-endianness assumption is
    # needed: sin_port sits at offset 2 in both sockaddr_in and sockaddr_in6.
    result = (uint16(raw[2]) shl 8) or uint16(raw[3])

  proc listenTcp*(port: uint16; backlog = 128): cint =
    let fd = socket(AF_INET, SOCK_STREAM, IPPROTO_TCP)
    assert fd >= 0, "socket() failed"
    var yes: cint = 1
    discard setsockopt(fd, SOL_SOCKET, SO_REUSEADDR, addr yes, SockLen(sizeof(yes)))
    var addr4 = default(Sockaddr_in)
    addr4.sin_family = TSa_Family(AF_INET)
    addr4.sin_port = htons(port)
    addr4.sin_addr.s_addr = INADDR_ANY
    assert bindAddr(fd, cast[ptr SockAddr](addr addr4),
                    SockLen(sizeof(addr4))) == 0, "bind failed"
    assert listen(fd, backlog.cint) == 0, "listen failed"
    setNonBlocking(fd)
    result = fd

when defined(windows):
  # Winsock socket surface — the ring's fd is a SOCKET narrowed to `cint`.
  #
  # `dynlib` externs are static imports on every backend, so the import library
  # has to be on the link line; MinGW does not add ws2_32 by default.
  {.passL: "-lws2_32".}
  #
  # `SOCKET` is `UINT_PTR`, but the values are kernel handles: small, 4-aligned
  # integers well inside 31 bits in practice (undocumented, so `listenTcp` and
  # the accept path assert it). Keeping the public `cint` API means every
  # consumer (hashi, harness) stays platform-neutral; the IOCP backend can
  # revisit this with completion keys (hashi/doc/iocp-ioring-briefing.md, open
  # question 1).
  # The POSIX arm's consumers see these names through `std/posix/posix`; give
  # the Windows arm the same exported surface so a server written against the
  # ring (hashi) stays platform-neutral.
  export types.SockLen, types.Sockaddr_storage, types.FileHandle
  type
    SocketHandle = uint
    InAddr* = object            ## struct in_addr
      s_addr*: uint32
    Sockaddr_in* = object       ## struct sockaddr_in (16 bytes, natural alignment)
      sin_family*: cushort
      sin_port*: uint16
      sin_addr*: InAddr
      sin_zero*: array[8, char]
    SockAddr* = object          ## struct sockaddr (opaque, 16 bytes)
      sa_family*: cushort
      sa_data*: array[14, char]

  const
    InvalidSocket = not 0'u
    AF_INET* = 2.cint
    SOCK_STREAM* = 1.cint
    IPPROTO_TCP* = 6.cint
    SOL_SOCKET* = 0xFFFF.cint
    SO_REUSEADDR* = 4.cint
    INADDR_ANY* = 0'u32
    FIONBIO = cast[clong](0x8004667E'u32)   ## _IOW('f', 126, u_long)

  proc wsSocket(af, typ, protocol: cint): SocketHandle {.
    stdcall, importc: "socket", dynlib: "ws2_32.dll".}
  proc wsBind(s: SocketHandle; name: pointer; namelen: cint): cint {.
    stdcall, importc: "bind", dynlib: "ws2_32.dll".}
  proc wsListen(s: SocketHandle; backlog: cint): cint {.
    stdcall, importc: "listen", dynlib: "ws2_32.dll".}
  proc wsIoctlsocket(s: SocketHandle; cmd: clong; argp: ptr culong): cint {.
    stdcall, importc: "ioctlsocket", dynlib: "ws2_32.dll".}
  proc wsClosesocket(s: SocketHandle): cint {.
    stdcall, importc: "closesocket", dynlib: "ws2_32.dll".}
  proc wsGetsockname(s: SocketHandle; name: pointer; namelen: ptr cint): cint {.
    stdcall, importc: "getsockname", dynlib: "ws2_32.dll".}

  proc socketOf(fd: cint): SocketHandle {.inline.} =
    SocketHandle(cast[uint32](fd))

  proc setNonBlocking*(fd: cint) =
    var one: culong = 1
    discard wsIoctlsocket(socketOf(fd), FIONBIO, addr one)

  proc closeFdRaw*(fd: cint) =
    discard wsClosesocket(socketOf(fd))

  proc closeFd*(fd: cint) =
    ## Close `fd`. Readiness backend: cancel this lane's in-flight ops on it
    ## (see `cancelPendingOps`), then `closesocket`. IOCP: the socket's ops live
    ## on its owner lane, where `closesocket` aborts them — whichever lane
    ## issued them — and the drain reports each as `ECancelled`, freeing the
    ## slot; only the lane association is dropped here (backends/iocp.nim,
    ## "Cancellation").
    when hasIocp:
      if backendRelays.forgetFd != nil:
        backendRelays.forgetFd(fd)
    else:
      cancelPendingOps(fd)
    discard wsClosesocket(socketOf(fd))

  proc listenTcp*(port: uint16; backlog = 128): cint =
    ## IPv4 wildcard listener. No SO_REUSEADDR: on Winsock that option allows a
    ## second bind to hijack a live listener, so the POSIX "restart without
    ## TIME_WAIT" semantics are not what it means here.
    let s = wsSocket(AF_INET, SOCK_STREAM, IPPROTO_TCP)
    assert s != InvalidSocket, "socket() failed"
    assert s <= SocketHandle(high(cint)), "SOCKET handle exceeds the ring's cint fd space"
    var addr4 = default(Sockaddr_in)
    addr4.sin_family = cushort(AF_INET)
    addr4.sin_port = htons(port)
    addr4.sin_addr.s_addr = INADDR_ANY
    assert wsBind(s, addr addr4, cint(sizeof(addr4))) == 0, "bind failed"
    assert wsListen(s, backlog.cint) == 0, "listen failed"
    result = cint(cast[uint32](s))
    setNonBlocking(result)

  # The POSIX arm's connect helpers, in their Winsock shapes. They exist here
  # for the same reason the socket types do: `submitConnect` is public on both
  # platforms, so what a caller needs to *use* it must be too.

  proc socketNonBlocking*(): cint =
    ## A non-blocking TCP socket, which is what `submitConnect` requires: a
    ## blocking one would finish the connect inside the syscall and there
    ## would be nothing for the ring to wait on.
    let s = wsSocket(AF_INET, SOCK_STREAM, IPPROTO_TCP)
    if s == InvalidSocket or s > SocketHandle(high(cint)): return -1
    result = cint(cast[uint32](s))
    setNonBlocking(result)

  proc loopbackAddr*(sa: var Sockaddr_storage; saLen: var SockLen;
                     port: uint16) =
    ## Fill `sa` with `127.0.0.1:port`, ready for `submitConnect`.
    const Loopback =
      when defined(bigEndian): 0x7F000001'u32 else: 0x0100007F'u32
    var a4 = default(Sockaddr_in)
    a4.sin_family = cushort(AF_INET)
    a4.sin_port = htons(port)
    a4.sin_addr.s_addr = Loopback
    sa = default(Sockaddr_storage)
    copyMem(addr sa, addr a4, sizeof(a4))
    saLen = SockLen(sizeof(a4))

  proc boundPort*(fd: cint): uint16 =
    ## The port `fd` is actually bound to. With `listenTcp(0)` the kernel picks
    ## one, and asking for it afterwards is the only way a test can listen
    ## without inventing a fixed number that a parallel run — or a socket still
    ## in TIME_WAIT — will collide with.
    var sa = default(Sockaddr_storage)
    var slen = cint(sizeof(sa))
    if wsGetsockname(socketOf(fd), addr sa, addr slen) != 0: return 0'u16
    let raw = cast[ptr UncheckedArray[uint8]](addr sa)
    # Network byte order, read as bytes so no host-endianness assumption is
    # needed: sin_port sits at offset 2 in both sockaddr_in and sockaddr_in6.
    result = (uint16(raw[2]) shl 8) or uint16(raw[3])

initIoRing()
