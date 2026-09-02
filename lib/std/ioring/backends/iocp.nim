# Windows IOCP backend — a proactor, selected with `-d:nimIoringIocp`.
#
# The ring's public contract is already completion-shaped (submit an op, get
# resumed with its byte count); IOCP *is* that contract, so this backend has
# no readiness emulation: `poll` drains this lane's op queue, issues each op
# as an overlapped WSARecv/WSASend/AcceptEx, and blocks in
# GetQueuedCompletionStatusEx until the kernel reports completions, which it
# hands to the shared `complete` (same resume path as every other backend).
#
# Lane affinity (the briefing's open question): a socket is associated with
# exactly ONE completion port for its lifetime, so every op for a socket must
# be issued — and its slot allocated — on the lane whose port the socket is
# bound to. `ioring.enqueueOp` asks `iocpOwnerLane` and routes to that lane's
# op queue (stripes are lock-protected, so cross-lane enqueue is safe), then
# `iocpWake`s it. The first op for a socket claims it for the submitting lane.
# Arenas therefore stay per-lane and unlocked, exactly as on POSIX.
#
# The OVERLAPPED blocks live in a per-lane, per-slot side arena owned by this
# module (`gAux`) rather than in `OpContext`: the other platforms pay nothing
# for them, and the storage is stable because it is sized to `MaxOps` up
# front (the slot arena's growth beyond MaxOps is a documented cold path the
# io_uring backend already rules out; asserted here).
#
# `opPollAdd` (readiness, used by harness/http/curl) has no IOCP form; those
# slots are served by a WSAPoll(0) pass on each poll while any are pending,
# with the completion wait shortened to 1 ms so they are re-checked promptly.
#
# Cancellation: `closeFd` may run on any lane. It drops the ownership record
# and `closesocket`s; the kernel then aborts every overlapped op pending on
# the socket — whichever lane issued it — and delivers each to the owner's
# port with STATUS_CANCELLED, which the drain reports as `ECancelled`, freeing
# the slot through the normal path. That is the cross-lane cancellation the
# readiness backends lack (their `cancelPendingOps` reaches the closing lane's
# slots only). An op still *queued* on the owner lane at close time fails at
# issue — the handle is gone, so the association fails and it completes as
# `ECancelled` too — unless Winsock has already reused the handle value for a
# new socket that claimed the same lane: the fd-reuse hazard every backend
# shares, so callers must not submit on a socket they are closing.
#
# Waiting: `poll` blocks in GetQueuedCompletionStatusEx for the worker loop's
# idle budget and reports a blocking wait as served, so the loop does not add
# its Sleep(1) on top — on Windows both round up to the ~15.6 ms scheduler
# tick, and the double wait cost cross-thread task hand-offs a second tick.
# Socket completions wake the wait at once, so no timeBeginPeriod: measured on
# a MinGW builder (hashi ws_echo), a fresh connection's first request is
# answered in ~0.4 ms here versus one full tick under the readiness backend,
# and the echo path is ~0.4 ms per frame under both. Only timer resumes and
# foreign-thread task submits still see the tick (they land in a stripe the
# sleeping worker scans on its next cycle); waking the target lane's port from
# `submit` would close that, and is a threadpool-level follow-up.
#
# Winsock/kernel32 are bound by `dynlib` (static imports on this toolchain;
# ioring.nim links ws2_32). Known v1 limits: one AcceptEx per submitAccept,
# no FILE_SKIP_COMPLETION_PORT_ON_SUCCESS (one delivery path).

when defined(windows):
  {.feature: "lenientnils".}   # nil proc values (the AcceptEx pointer)
  import std/[threadpool, ticketlocks, tables, syncio, assertions]
  import std/windows/winlean   # Handle, DWORD, closeHandle, INVALID_HANDLE_VALUE
  import ../core/types
  import ../core/slots
  import ../core/backend

  type
    SocketHandle = uint            ## Winsock SOCKET (UINT_PTR)
    Overlapped {.pure.} = object   ## OVERLAPPED
      internal, internalHigh: uint
      offset, offsetHigh: uint32
      hEvent: nil pointer
    IocpOv {.pure.} = object
      ov: Overlapped               ## first: the drain casts lpOverlapped back
      lane: int32
      slot: int32
    WsaBuf {.pure.} = object       ## WSABUF
      len: uint32
      buf: nil pointer
    OverlappedEntry {.pure.} = object   ## OVERLAPPED_ENTRY
      key: uint
      ov: nil ptr Overlapped
      internal: uint
      bytes: uint32
    Guid {.pure.} = object
      data1: uint32
      data2, data3: uint16
      data4: array[8, uint8]
    Aux = object                   ## per-slot side state (stable storage)
      wov: IocpOv
      wsabuf: WsaBuf
      acceptSock: SocketHandle     ## opAccept: the pre-created socket AcceptEx fills
      acceptBuf: array[2 * (128 + 16), uint8]   ## AcceptEx local+remote address scratch
    AcceptExFn = proc (listen, accept: SocketHandle; buf: pointer;
                       recvLen, localLen, remoteLen: uint32; received: ptr uint32;
                       ov: ptr Overlapped): int32 {.stdcall.}

  const
    InvalidSocket = not 0'u
    SocketError = -1.cint
    WSA_IO_PENDING = 997.cint
    SOL_SOCKET = 0xFFFF.cint
    SO_UPDATE_ACCEPT_CONTEXT = 0x700B.cint
    SO_PROTOCOL_INFOW = 0x2005.cint
    SIO_GET_EXTENSION_FUNCTION_POINTER = 0xC8000006'u32
    WSA_FLAG_OVERLAPPED = 0x01'u32
    MaxEntries = 64
    DrainBatch = 128
    WakeKey = not 0'u              ## completion key of a PostQueuedCompletionStatus wake
    StatusCancelled = 0xC0000120'u32   ## NTSTATUS in OVERLAPPED.Internal of an aborted op
    OpKey = 1'u                    ## completion key of every real socket completion
    POLLRDNORM = 0x0100
    POLLWRNORM = 0x0010
    PollFailMask = 0x0001 or 0x0002 or 0x0004   # POLLERR | POLLHUP | POLLNVAL
    AddrLen = 128 + 16             ## AcceptEx: sizeof(sockaddr_storage) + 16

  proc createIoCompletionPort(fileHandle: Handle; port: Handle; key: uint;
                              threads: uint32): Handle {.
    stdcall, importc: "CreateIoCompletionPort", dynlib: "kernel32".}
  proc getQueuedCompletionStatusEx(port: Handle; entries: ptr OverlappedEntry;
                                   count: uint32; removed: ptr uint32; timeoutMs: uint32;
                                   alertable: int32): int32 {.
    stdcall, importc: "GetQueuedCompletionStatusEx", dynlib: "kernel32".}
  proc postQueuedCompletionStatus(port: Handle; bytes: uint32; key: uint;
                                  ov: nil ptr Overlapped): int32 {.
    stdcall, importc: "PostQueuedCompletionStatus", dynlib: "kernel32".}
  proc wsaRecv(s: SocketHandle; bufs: ptr WsaBuf; count: uint32; recvd: ptr uint32;
               flags: ptr uint32; ov: ptr Overlapped; routine: nil pointer): cint {.
    stdcall, importc: "WSARecv", dynlib: "ws2_32.dll".}
  proc wsaSend(s: SocketHandle; bufs: ptr WsaBuf; count: uint32; sent: ptr uint32;
               flags: uint32; ov: ptr Overlapped; routine: nil pointer): cint {.
    stdcall, importc: "WSASend", dynlib: "ws2_32.dll".}
  proc wsaIoctl(s: SocketHandle; code: uint32; inbuf: pointer; inlen: uint32;
                outbuf: pointer; outlen: uint32; ret: ptr uint32; ov: nil pointer;
                routine: nil pointer): cint {.
    stdcall, importc: "WSAIoctl", dynlib: "ws2_32.dll".}
  proc wsaSocketW(af, typ, protocol: cint; info: nil pointer; group: uint32;
                  flags: uint32): SocketHandle {.
    stdcall, importc: "WSASocketW", dynlib: "ws2_32.dll".}
  proc wsaGetLastError(): cint {.
    stdcall, importc: "WSAGetLastError", dynlib: "ws2_32.dll".}
  proc wsaGetOverlappedResult(s: SocketHandle; ov: ptr Overlapped; bytes: ptr uint32;
                              wait: int32; flags: ptr uint32): int32 {.
    stdcall, importc: "WSAGetOverlappedResult", dynlib: "ws2_32.dll".}
  proc wsSetsockopt(s: SocketHandle; level, optname: cint; optval: pointer;
                    optlen: cint): cint {.
    stdcall, importc: "setsockopt", dynlib: "ws2_32.dll".}
  proc wsGetsockopt(s: SocketHandle; level, optname: cint; optval: pointer;
                    optlen: ptr cint): cint {.
    stdcall, importc: "getsockopt", dynlib: "ws2_32.dll".}
  proc wsClosesocket(s: SocketHandle): cint {.
    stdcall, importc: "closesocket", dynlib: "ws2_32.dll".}
  type WsaPollFd {.pure.} = object
    fd: SocketHandle
    events: cshort
    revents: cshort
  proc wsaPoll(fdArray: ptr WsaPollFd; nfds: culong; timeout: cint): cint {.
    stdcall, importc: "WSAPoll", dynlib: "ws2_32.dll".}

  var
    gPorts: seq[Handle]            ## one completion port per lane
    gAux: seq[seq[Aux]]            ## per lane, MaxOps entries
    gPollAdds: seq[seq[int]]       ## per lane: slot indices of pending opPollAdd ops
    gOwner: Table[cint, int]       ## socket → owning lane (port association)
    gOwnerLock: TicketLock
    gAcceptEx: AcceptExFn

  proc socketOf(fd: cint): SocketHandle {.inline.} = SocketHandle(cast[uint32](fd))
  proc fdOf(s: SocketHandle): cint {.inline.} = cint(cast[uint32](s))

  proc iocpOwnerLane*(fd: cint): int =
    ## The lane whose port `fd` is associated with; -1 when not yet claimed.
    gOwnerLock.acquire()
    result = gOwner.getOrDefault(fd, -1)
    gOwnerLock.release()

  proc iocpWake*(lane: int) =
    ## Wake `lane` out of its completion wait (a cross-lane enqueue).
    discard postQueuedCompletionStatus(gPorts[lane], 0'u32, WakeKey, nil)

  proc ensureAssociated(fd: cint; lane: int): bool =
    ## First op for a socket: bind it to this lane's port and record the owner.
    ## False when the op must not be issued: the association failed (a handle
    ## closed while its op was still queued, or one bound to another port), or
    ## the socket belongs to another lane after all (its handle value was
    ## closed and reused between routing and issue). Ownership is recorded only
    ## on success — a stale record would make the next socket to get this
    ## handle value skip association, and its ops would never complete.
    gOwnerLock.acquire()
    let owner = gOwner.getOrDefault(fd, -1)
    if owner < 0:
      result = createIoCompletionPort(cast[Handle](socketOf(fd)), gPorts[lane],
                                      OpKey, 0'u32) != cast[Handle](0)
      if result: gOwner[fd] = lane
    else:
      result = owner == lane
    gOwnerLock.release()

  proc loadAcceptEx(s: SocketHandle) =
    ## AcceptEx is an extension: fetched once per process through WSAIoctl.
    if gAcceptEx != nil: return
    var guid = Guid(data1: 0xb5367df1'u32, data2: 0xcbac'u16, data3: 0x11cf'u16,
                    data4: [0x95'u8, 0xca'u8, 0x00'u8, 0x80'u8, 0x5f'u8, 0x48'u8, 0xa1'u8, 0x92'u8])
    # WSAIoctl writes the function pointer straight into a proc-typed slot: a
    # pointer→proc cast is not allowed on this toolchain.
    var fn: AcceptExFn = nil
    var got = 0'u32
    if wsaIoctl(s, SIO_GET_EXTENSION_FUNCTION_POINTER, addr guid, uint32(sizeof(guid)),
                addr fn, uint32(sizeof(fn)), addr got, nil, nil) == 0:
      gAcceptEx = fn

  proc listenerFamily(s: SocketHandle): cint =
    ## The listener's address family, so the accept socket matches it.
    var info = default(array[640, uint8])   # WSAPROTOCOL_INFOW (628 bytes)
    var len = cint(info.len)
    if wsGetsockopt(s, SOL_SOCKET, SO_PROTOCOL_INFOW, addr info[0], addr len) == 0:
      # iAddressFamily is the INT at offset 4*4 + 4 (dwServiceFlags1..4, dwProviderFlags,
      # ProviderId(16), dwCatalogEntryId, ProtocolChain(4+7*4=32)) — see WSAPROTOCOL_INFOW:
      # offset = 20 + 16 + 4 + 32 = 72
      result = cast[ptr cint](addr info[72])[]
    else:
      result = 2.cint   # AF_INET

  proc clampLen(n: int): uint32 {.inline.} =
    if n > int(high(int32)): uint32(high(int32)) else: uint32(n)

  proc issue(lane, slotIdx: int) =
    ## Issue the op in slot `slotIdx` as an overlapped operation.
    let op = addr gSlots[lane].slots[slotIdx].op
    let a = addr gAux[lane][slotIdx]
    a.wov = IocpOv(lane: int32(lane), slot: int32(slotIdx))
    let s = socketOf(op.fd)
    case op.kind
    of opNop:
      complete(slotIdx, 0)
    of opRead:
      a.wsabuf = WsaBuf(len: clampLen(op.len), buf: op.buf)
      var n = 0'u32
      var flags = 0'u32
      let r = wsaRecv(s, addr a.wsabuf, 1'u32, addr n, addr flags, addr a.wov.ov, nil)
      if r == SocketError and wsaGetLastError() != WSA_IO_PENDING:
        complete(slotIdx, -1)
    of opWrite:
      a.wsabuf = WsaBuf(len: clampLen(op.len), buf: op.buf)
      var n = 0'u32
      let r = wsaSend(s, addr a.wsabuf, 1'u32, addr n, 0'u32, addr a.wov.ov, nil)
      if r == SocketError and wsaGetLastError() != WSA_IO_PENDING:
        complete(slotIdx, -1)
    of opAccept:
      loadAcceptEx(s)
      if gAcceptEx == nil:
        complete(slotIdx, -1)
      else:
        a.acceptSock = wsaSocketW(listenerFamily(s), 1.cint, 6.cint, nil, 0'u32, WSA_FLAG_OVERLAPPED)
        if a.acceptSock == InvalidSocket:
          complete(slotIdx, -1)
        else:
          var n = 0'u32
          let ok = gAcceptEx(s, a.acceptSock, addr a.acceptBuf[0], 0'u32,
                             uint32(AddrLen), uint32(AddrLen), addr n, addr a.wov.ov)
          if ok == 0 and wsaGetLastError() != WSA_IO_PENDING:
            discard wsClosesocket(a.acceptSock)
            a.acceptSock = InvalidSocket
            complete(slotIdx, -1)
    of opPollAdd:
      gPollAdds[lane].add slotIdx

  proc servePollAdds(lane: int): bool =
    ## Readiness probes have no IOCP form: one WSAPoll(0) over the pending
    ## opPollAdd slots, completing the ones whose requested direction fired.
    result = false
    if gPollAdds[lane].len == 0: return
    var pfds = newSeq[WsaPollFd](gPollAdds[lane].len)
    var i = 0
    while i < gPollAdds[lane].len:
      let op = addr gSlots[lane].slots[gPollAdds[lane][i]].op
      var ev = 0
      if evRead in op.pollMask: ev = ev or POLLRDNORM
      if evWrite in op.pollMask: ev = ev or POLLWRNORM
      pfds[i] = WsaPollFd(fd: socketOf(op.fd), events: cshort(ev), revents: cshort(0))
      i = i + 1
    if wsaPoll(addr pfds[0], culong(pfds.len), 0.cint) <= 0: return
    var keep: seq[int] = @[]
    i = 0
    while i < gPollAdds[lane].len:
      let slotIdx = gPollAdds[lane][i]
      let re = int(pfds[i].revents)
      var fired: IoEvents = {}
      if (re and POLLRDNORM) != 0: fired.incl evRead
      if (re and POLLWRNORM) != 0: fired.incl evWrite
      if (re and PollFailMask) != 0: fired = {evRead, evWrite}
      let hit = fired * gSlots[lane].slots[slotIdx].op.pollMask
      if hit != {}:
        complete(slotIdx, toEventMask(hit))
        result = true
      else:
        keep.add slotIdx
      i = i + 1
    gPollAdds[lane] = keep

  proc iocpPoll(timeoutMs: int): bool {.nimcall.} =
    let lane = ioLane()
    var buf {.noinit.}: array[DrainBatch, OpContext]
    let n = gOpQueues[lane].tryBulkDequeue(DrainBatch, buf)
    var i = 0
    while i < n:
      let slotIdx = gSlots[lane].allocSlot(buf[i])
      assert slotIdx < MaxOps, "ioring/iocp: slot arena grew past MaxOps (OVERLAPPED storage)"
      if buf[i].kind != opNop and buf[i].kind != opPollAdd and
          not ensureAssociated(buf[i].fd, lane):
        complete(slotIdx, ECancelled)   # closed or foreign handle: never issued
      else:
        issue(lane, slotIdx)
      i = i + 1
    result = servePollAdds(lane)
    # Readiness probes are re-checked every millisecond while any are pending.
    let wait = if gPollAdds[lane].len > 0 and timeoutMs > 1: 1 else: timeoutMs
    var entries {.noinit.}: array[MaxEntries, OverlappedEntry]
    var got = 0'u32
    if getQueuedCompletionStatusEx(gPorts[lane], addr entries[0], uint32(MaxEntries),
                                   addr got, uint32(wait), 0'i32) == 0:
      # Timed out: a blocking wait was this worker's idle sleep (see header).
      return result or wait > 0
    var k = 0
    while k < int(got):
      let e = addr entries[k]
      k = k + 1
      if e.key == WakeKey or e.ov == nil: continue
      let wov = cast[ptr IocpOv](e.ov)
      assert int(wov.lane) == lane, "ioring/iocp: completion delivered to a foreign lane"
      let slotIdx = int(wov.slot)
      let op = addr gSlots[lane].slots[slotIdx].op
      let a = addr gAux[lane][slotIdx]
      var res = -1
      if e.internal == 0'u:
        case op.kind
        of opAccept:
          var listenSock = socketOf(op.fd)
          discard wsSetsockopt(a.acceptSock, SOL_SOCKET, SO_UPDATE_ACCEPT_CONTEXT,
                               addr listenSock, cint(sizeof(listenSock)))
          if a.acceptSock <= SocketHandle(high(cint)):
            res = int(fdOf(a.acceptSock))
          else:
            discard wsClosesocket(a.acceptSock)   # cannot be narrowed to the cint API
          a.acceptSock = InvalidSocket
        else:
          res = int(e.bytes)
      else:
        if op.kind == opAccept and a.acceptSock != InvalidSocket:
          discard wsClosesocket(a.acceptSock)
          a.acceptSock = InvalidSocket
        # Aborted by closesocket (see "Cancellation"): the ring's cancellation
        # result, not a generic failure. Internal holds the NTSTATUS.
        if uint32(e.internal and 0xFFFFFFFF'u) == StatusCancelled:
          res = ECancelled
      complete(slotIdx, res)
      result = true

  proc iocpClose() {.nimcall.} =
    var i = 0
    while i < gPorts.len:
      discard closeHandle(gPorts[i])
      i = i + 1

  proc iocpForgetFd(fd: cint) {.nimcall.} =
    ## The socket is being closed: drop its lane association. Its pending
    ## overlapped ops complete with ERROR_OPERATION_ABORTED on the owner lane,
    ## which frees their slots through the normal drain.
    gOwnerLock.acquire()
    gOwner.del(fd)
    gOwnerLock.release()

  proc initIocpBackendRelays*(): BackendRelays =
    let lanes = ioLanes()
    gPorts = newSeq[Handle](lanes)
    gAux = newSeq[seq[Aux]](lanes)
    gPollAdds = newSeq[seq[int]](lanes)
    var i = 0
    while i < lanes:
      gPorts[i] = createIoCompletionPort(INVALID_HANDLE_VALUE, cast[Handle](0), 0'u, 1'u32)
      gAux[i] = newSeq[Aux](MaxOps)
      gPollAdds[i] = @[]
      i = i + 1
    gOwner = initTable[cint, int]()
    result = BackendRelays(
      poll: iocpPoll,
      close: iocpClose,
      forgetFd: iocpForgetFd,
    )
