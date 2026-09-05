# Windows IOCP backend — a proactor, the Windows default (platform.nim;
# `-d:nimIoringWsaPoll` selects the readiness backend instead).
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
# The OVERLAPPED blocks live in a side arena owned by this module (`gAux`)
# rather than in `OpContext`: the other platforms pay nothing for them, and the
# storage is stable because it is sized to `MaxOps` up front (the slot arena's
# growth beyond MaxOps is a documented cold path the io_uring backend already
# rules out; asserted here).
#
# An Aux block is NOT indexed by slot, and that is load-bearing. A slot can be
# freed while the kernel still owns the op's OVERLAPPED — a blown deadline
# completes the op locally (core/backend.expireDeadlines) and the kernel finds
# out only later — and the next op into that slot would then overwrite an
# OVERLAPPED the kernel is still writing through. So blocks come from a free
# list and go back only when their completion is drained; a block names the
# slot AND the generation it was issued for, and a completion whose slot has
# moved on is dropped, the same shape as io_uring's `user_data` tag.
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
# idle budget — or until this lane's earliest deadline, whichever is sooner
# (`waitMillis`; `expireDeadlines` then completes whatever ran out of time, so
# a timer fires whether or not any I/O did). The relay declares `waits`, so the
# loop does not add its Sleep(1) on top — on Windows both round up to the
# ~15.6 ms scheduler tick, and the double wait cost cross-thread task hand-offs
# a second tick.
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
  import std/[threadpool, ticketlocks, tables, syncio, assertions, atomics]
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
      aux: int32                   ## the Aux block this lives in, for the drain
      gen: uint32                  ## the slot's generation when the op was issued
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
    Aux = object                   ## per-op side state (stable storage)
      wov: IocpOv
      wsabuf: WsaBuf
      acceptSock: SocketHandle     ## opAccept: the pre-created socket AcceptEx fills
      acceptBuf: array[2 * (128 + 16), uint8]   ## AcceptEx local+remote address scratch
    PendingPoll = object           ## an opPollAdd awaiting the WSAPoll pass
      slot: int32
      gen: uint32                  ## so an expired probe's slot is not re-read
    AcceptExFn = proc (listen, accept: SocketHandle; buf: pointer;
                       recvLen, localLen, remoteLen: uint32; received: ptr uint32;
                       ov: ptr Overlapped): int32 {.stdcall.}
    ConnectExFn = proc (s: SocketHandle; name: pointer; namelen: cint;
                        sendBuf: pointer; sendLen: uint32; sent: ptr uint32;
                        ov: ptr Overlapped): int32 {.stdcall.}

  const
    InvalidSocket = not 0'u
    SocketError = -1.cint
    WSA_IO_PENDING = 997.cint
    SOL_SOCKET = 0xFFFF.cint
    SO_UPDATE_ACCEPT_CONTEXT = 0x700B.cint
    SO_UPDATE_CONNECT_CONTEXT = 0x7010.cint
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
    NoAux = -1'i32                 ## "holds no OVERLAPPED block"

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
  proc wsBind(s: SocketHandle; name: pointer; namelen: cint): cint {.
    stdcall, importc: "bind", dynlib: "ws2_32.dll".}
  proc cancelIoEx(file: Handle; ov: nil ptr Overlapped): int32 {.
    stdcall, importc: "CancelIoEx", dynlib: "kernel32".}
  type WsaPollFd {.pure.} = object
    fd: SocketHandle
    events: cshort
    revents: cshort
  proc wsaPoll(fdArray: ptr WsaPollFd; nfds: culong; timeout: cint): cint {.
    stdcall, importc: "WSAPoll", dynlib: "ws2_32.dll".}

  var
    gPorts: seq[Handle]            ## one completion port per lane
    gAux: seq[seq[Aux]]            ## per lane, MaxOps entries
    gAuxFree: seq[seq[int32]]      ## per lane: Aux blocks the kernel is not using
    gSlotAux: seq[seq[int32]]      ## per lane, by slot: the block its op was issued in
    gPollAdds: seq[seq[PendingPoll]]  ## per lane: opPollAdd ops awaiting the WSAPoll pass
    gOwner: Table[cint, int]       ## socket → owning lane (port association)
    gOwnerLock: TicketLock
    gAcceptEx: AcceptExFn
    gConnectEx: ConnectExFn
    gAcceptExState: int            ## 0 unloaded, 1 loading, 2 published
    gConnectExState: int           ## — see `loadAcceptEx`

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

  # WSAIoctl writes the function pointer straight into the proc-typed GLOBAL: a
  # pointer→proc cast is not allowed on this toolchain, so there is nothing to
  # assign across from a local.
  #
  # Which means the STATE, not the pointer, is what carries the load between
  # lanes. Every lane's first accept/connect runs this, and neither the guard
  # (`gAcceptEx != nil`) nor ws2_32's write into those eight bytes is atomic or
  # ordered on its own — two lanes could both pass the guard and both call
  # WSAIoctl while a third reads the pointer mid-write. The CAS makes the load
  # happen once per process; the release/acquire pair is what makes the pointer
  # it wrote visible to everyone else. Same shape as `ioring.init` and
  # `threadpool.initPool`, and for the same reason.
  #
  # A failed WSAIoctl writes nothing, so the global stays nil and the state
  # still reaches 2: callers test the pointer, and "not available" is as final
  # an answer as an address.
  proc loadAcceptEx(s: SocketHandle) =
    ## AcceptEx is an extension: fetched once per process through WSAIoctl.
    if atomicLoad(gAcceptExState, moAcquire) == 2: return
    var expected = 0
    if not atomicCompareExchange(gAcceptExState, expected, 1):
      while atomicLoad(gAcceptExState, moAcquire) != 2: discard
      return
    var guid = Guid(data1: 0xb5367df1'u32, data2: 0xcbac'u16, data3: 0x11cf'u16,
                    data4: [0x95'u8, 0xca'u8, 0x00'u8, 0x80'u8, 0x5f'u8, 0x48'u8, 0xa1'u8, 0x92'u8])
    var got = 0'u32
    discard wsaIoctl(s, SIO_GET_EXTENSION_FUNCTION_POINTER, addr guid, uint32(sizeof(guid)),
                     addr gAcceptEx, uint32(sizeof(gAcceptEx)), addr got, nil, nil)
    atomicStore(gAcceptExState, 2, moRelease)

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

  proc loadConnectEx(s: SocketHandle) =
    ## ConnectEx, fetched the same way as AcceptEx and for the same reason: it
    ## is a Winsock extension, not an export — including how it is published
    ## (see `loadAcceptEx`).
    if atomicLoad(gConnectExState, moAcquire) == 2: return
    var expected = 0
    if not atomicCompareExchange(gConnectExState, expected, 1):
      while atomicLoad(gConnectExState, moAcquire) != 2: discard
      return
    var guid = Guid(data1: 0x25a207b9'u32, data2: 0xddf3'u16, data3: 0x4660'u16,
                    data4: [0x8e'u8, 0xe9'u8, 0x76'u8, 0xe5'u8, 0x8c'u8, 0x74'u8, 0x06'u8, 0x3e'u8])
    var got = 0'u32
    discard wsaIoctl(s, SIO_GET_EXTENSION_FUNCTION_POINTER, addr guid, uint32(sizeof(guid)),
                     addr gConnectEx, uint32(sizeof(gConnectEx)), addr got, nil, nil)
    atomicStore(gConnectExState, 2, moRelease)

  proc allocAux(lane: int): int32 =
    ## An OVERLAPPED block the kernel is not using. `NoAux` when every block is
    ## outstanding — see `issue`, which is the only caller and fails the op.
    if gAuxFree[lane].len == 0: return NoAux
    result = gAuxFree[lane].pop()

  proc freeAux(lane: int; ai: int32) {.inline.} =
    ## The kernel is done with this OVERLAPPED — from the drain, or from an
    ## issue that never reached the kernel at all.
    if ai != NoAux: gAuxFree[lane].add ai

  proc abortIssue(lane, slotIdx: int; ai: int32; res: int) =
    ## An op that never reached the kernel. The slot's binding is cleared
    ## BEFORE the slot is freed: a stale `gSlotAux` entry would let
    ## `iocpCancelInFlight` reach through it later and CancelIoEx an OVERLAPPED
    ## that by then belongs to a live op of somebody else's.
    gSlotAux[lane][slotIdx] = NoAux
    freeAux(lane, ai)
    complete(slotIdx, res)

  proc clampLen(n: int): uint32 {.inline.} =
    if n > int(high(int32)): uint32(high(int32)) else: uint32(n)

  proc issue(lane, slotIdx: int) =
    ## Issue the op in slot `slotIdx` as an overlapped operation.
    ##
    ## Every path that does NOT leave an OVERLAPPED with the kernel must give
    ## the Aux block back before it returns, or the free list bleeds a block
    ## per failed issue.
    let op = addr gSlots[lane].slots[slotIdx].op
    case op.kind
    of opNop:
      complete(slotIdx, 0)
      return
    of opTimeout:
      return               # nothing to issue: the lane's deadline heap is the op
    of opPollAdd:
      gPollAdds[lane].add PendingPoll(slot: int32(slotIdx),
                                      gen: gSlots[lane].slots[slotIdx].gen)
      return
    else:
      discard
    let ai = allocAux(lane)
    if ai == NoAux:
      # Every block is with the kernel. Only reachable once more ops are
      # outstanding-or-orphaned than `MaxOps`, which the arena itself bounds
      # in the steady state; failing the op is still better than reusing a
      # block the kernel writes through.
      complete(slotIdx, -1)
      return
    gSlotAux[lane][slotIdx] = ai
    let a = addr gAux[lane][ai]
    a.wov = IocpOv(lane: int32(lane), slot: int32(slotIdx), aux: ai,
                   gen: gSlots[lane].slots[slotIdx].gen)
    # Blocks are recycled, so the accept socket is reset here rather than
    # trusted to be clean: a stale value would be closed by the drain's
    # stale-completion path, and by then it names somebody else's socket.
    a.acceptSock = InvalidSocket
    let s = socketOf(op.fd)
    case op.kind
    of opRead:
      a.wsabuf = WsaBuf(len: clampLen(op.len), buf: op.buf)
      var n = 0'u32
      var flags = 0'u32
      let r = wsaRecv(s, addr a.wsabuf, 1'u32, addr n, addr flags, addr a.wov.ov, nil)
      if r == SocketError and wsaGetLastError() != WSA_IO_PENDING:
        abortIssue(lane, slotIdx, ai, -1)
    of opWrite:
      a.wsabuf = WsaBuf(len: clampLen(op.len), buf: op.buf)
      var n = 0'u32
      let r = wsaSend(s, addr a.wsabuf, 1'u32, addr n, 0'u32, addr a.wov.ov, nil)
      if r == SocketError and wsaGetLastError() != WSA_IO_PENDING:
        abortIssue(lane, slotIdx, ai, -1)
    of opAccept:
      loadAcceptEx(s)
      if gAcceptEx == nil:
        abortIssue(lane, slotIdx, ai, -1)
      else:
        a.acceptSock = wsaSocketW(listenerFamily(s), 1.cint, 6.cint, nil, 0'u32, WSA_FLAG_OVERLAPPED)
        if a.acceptSock == InvalidSocket:
          abortIssue(lane, slotIdx, ai, -1)
        else:
          var n = 0'u32
          let ok = gAcceptEx(s, a.acceptSock, addr a.acceptBuf[0], 0'u32,
                             uint32(AddrLen), uint32(AddrLen), addr n, addr a.wov.ov)
          if ok == 0 and wsaGetLastError() != WSA_IO_PENDING:
            discard wsClosesocket(a.acceptSock)
            a.acceptSock = InvalidSocket
            abortIssue(lane, slotIdx, ai, -1)
    of opConnect:
      loadConnectEx(s)
      if gConnectEx == nil:
        abortIssue(lane, slotIdx, ai, -1)
      else:
        # ConnectEx demands a bound socket, which a caller has no reason to
        # have done — `socketNonBlocking` just creates one. Bind the wildcard
        # here; a socket that IS already bound answers WSAEINVAL, which is why
        # the result is discarded rather than checked.
        var any = default(array[16, uint8])
        any[0] = 2'u8               # sin_family = AF_INET, the rest zero
        discard wsBind(s, addr any[0], cint(any.len))
        var sent = 0'u32
        let ok = gConnectEx(s, addr op.sockAddr, cint(op.sockAddrLen), nil, 0'u32,
                            addr sent, addr a.wov.ov)
        let err = if ok == 0: wsaGetLastError() else: 0.cint
        if ok == 0 and err != WSA_IO_PENDING:
          # A negated Winsock code, not an errno — see poll.nim's Windows
          # `startConnect` for why the two arms cannot share a numbering.
          abortIssue(lane, slotIdx, ai, -int(err))
    else:
      # opNop/opTimeout/opPollAdd returned above; this arm exists so a new op
      # kind fails loudly here instead of silently never being issued.
      abortIssue(lane, slotIdx, ai, -1)

  proc liveProbe(lane: int; e: PendingPoll): bool {.inline.} =
    ## Is this entry still the op it was recorded for? A probe can be completed
    ## behind this list's back — a blown deadline expires it, `cancelPendingOps`
    ## does not reach here — and its slot then holds someone else's op.
    let s = addr gSlots[lane].slots[e.slot.int]
    s.inUse and s.gen == e.gen

  proc servePollAdds(lane: int): bool =
    ## Readiness probes have no IOCP form: one WSAPoll(0) over the pending
    ## opPollAdd slots, completing the ones whose requested direction fired.
    result = false
    if gPollAdds[lane].len == 0: return
    var live: seq[PendingPoll] = @[]
    var i = 0
    while i < gPollAdds[lane].len:
      if liveProbe(lane, gPollAdds[lane][i]): live.add gPollAdds[lane][i]
      i = i + 1
    gPollAdds[lane] = live
    if live.len == 0: return
    var pfds = newSeq[WsaPollFd](live.len)
    i = 0
    while i < live.len:
      let op = addr gSlots[lane].slots[live[i].slot.int].op
      var ev = 0
      if evRead in op.pollMask: ev = ev or POLLRDNORM
      if evWrite in op.pollMask: ev = ev or POLLWRNORM
      pfds[i] = WsaPollFd(fd: socketOf(op.fd), events: cshort(ev), revents: cshort(0))
      i = i + 1
    if wsaPoll(addr pfds[0], culong(pfds.len), 0.cint) <= 0: return
    var keep: seq[PendingPoll] = @[]
    i = 0
    while i < live.len:
      let slotIdx = live[i].slot.int
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
        keep.add live[i]
      i = i + 1
    gPollAdds[lane] = keep

  proc iocpPoll(timeoutMs: int): bool {.nimcall.} =
    let lane = ioLane()
    var buf {.noinit.}: array[DrainBatch, OpContext]
    let n = gOpQueues[lane].tryBulkDequeue(DrainBatch, buf)
    var i = 0
    while i < n:
      let slotIdx = gSlots[lane].allocSlot(buf[i])
      if slotIdx >= MaxOps:
        # The arena took its documented cold path and grew past the OVERLAPPED
        # storage sized for it (`gAux`/`gSlotAux` are exactly `MaxOps` long and
        # never grow, so that pointers into them stay valid while the kernel
        # writes through one). There is no block for this slot, so the op
        # cannot be issued and failing it is the only answer left. A CHECK and
        # not an `assert`: under `-d:danger` the alternative is a silent
        # out-of-bounds write into `gSlotAux` one line below.
        complete(slotIdx, -1)
      else:
        # The slot is fresh, so whatever its previous op bound here is gone; the
        # kinds `issue` handles without an OVERLAPPED never write it at all.
        gSlotAux[lane][slotIdx] = NoAux
        armDeadline(lane, slotIdx)
        # An fd-less op (nop, timer) has no socket to associate, and a readiness
        # probe is served by WSAPoll rather than by the port.
        if buf[i].kind != opNop and buf[i].kind != opTimeout and
            buf[i].kind != opPollAdd and not ensureAssociated(buf[i].fd, lane):
          complete(slotIdx, ECancelled) # closed or foreign handle: never issued
        else:
          issue(lane, slotIdx)
      i = i + 1
    result = servePollAdds(lane)
    # Readiness probes are re-checked every millisecond while any are pending,
    # and no wait outlasts the earliest deadline on this lane.
    var wait = if gPollAdds[lane].len > 0 and timeoutMs > 1: 1 else: timeoutMs
    wait = waitMillis(lane, wait)
    var entries {.noinit.}: array[MaxEntries, OverlappedEntry]
    var got = 0'u32
    if getQueuedCompletionStatusEx(gPorts[lane], addr entries[0], uint32(MaxEntries),
                                   addr got, uint32(wait), 0'i32) == 0:
      # Timed out: a blocking wait was this worker's idle sleep (see header).
      expireDeadlines(lane)
      return result
    var k = 0
    while k < int(got):
      let e = addr entries[k]
      k = k + 1
      if e.key == WakeKey or e.ov == nil: continue
      let wov = cast[ptr IocpOv](e.ov)
      assert int(wov.lane) == lane, "ioring/iocp: completion delivered to a foreign lane"
      let slotIdx = int(wov.slot)
      let auxIdx = wov.aux
      assert auxIdx != NoAux, "ioring/iocp: completion for an op that was never issued"
      let a = addr gAux[lane][auxIdx.int]
      if slotIdx >= gSlots[lane].slots.len or
         not gSlots[lane].slots[slotIdx].inUse or
         gSlots[lane].slots[slotIdx].gen != wov.gen:
        # The op this completion belongs to was already accounted for here — a
        # blown deadline expired it — and its slot has moved on. Applying the
        # completion would report the kernel's result against an unrelated op
        # and free a slot that is not ours to free. Reclaim the block (and the
        # socket AcceptEx made, which now has no owner) and drop it.
        if a.acceptSock != InvalidSocket:
          discard wsClosesocket(a.acceptSock)
          a.acceptSock = InvalidSocket
        freeAux(lane, auxIdx)
        continue
      let op = addr gSlots[lane].slots[slotIdx].op
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
        of opConnect:
          # Until the socket is told its own connect finished, getpeername,
          # shutdown and half the option surface keep reporting it unconnected.
          discard wsSetsockopt(socketOf(op.fd), SOL_SOCKET, SO_UPDATE_CONNECT_CONTEXT,
                               nil, 0.cint)
          res = 0
        else:
          res = int(e.bytes)
      else:
        if op.kind == opAccept and a.acceptSock != InvalidSocket:
          discard wsClosesocket(a.acceptSock)
          a.acceptSock = InvalidSocket
        # Closed under the op (see "Cancellation"): the ring's cancellation
        # result, not a generic failure. The kernel's status is not the test —
        # an aborted AcceptEx completes STATUS_CANCELLED but a WSARecv pending
        # on the closed socket completes with a disconnect status (measured) —
        # ownership is: `closeFd` drops the record before closesocket, and
        # nothing else drops it.
        if uint32(e.internal and 0xFFFFFFFF'u) == StatusCancelled or
            iocpOwnerLane(op.fd) < 0:
          res = ECancelled
        elif op.kind == opConnect:
          # A refused or unreachable peer is the whole reason a caller asked
          # for a connect, so this one failure is worth naming. The status is
          # an NTSTATUS; `WSAGetOverlappedResult` gives back the Winsock code.
          var bytes = 0'u32
          var flags = 0'u32
          discard wsaGetOverlappedResult(socketOf(op.fd), addr a.wov.ov,
                                         addr bytes, 0'i32, addr flags)
          res = -int(wsaGetLastError())
      gSlotAux[lane][slotIdx] = NoAux
      freeAux(lane, auxIdx)
      complete(slotIdx, res)
      result = true
    expireDeadlines(lane)

  proc iocpClose() {.nimcall.} =
    var i = 0
    while i < gPorts.len:
      discard closeHandle(gPorts[i])
      i = i + 1

  proc iocpCancelInFlight(slotIdx: int; gen: uint32) {.nimcall.} =
    ## This lane's deadline heap is about to complete the op locally and free
    ## its slot, but the kernel still owns the OVERLAPPED and the buffer behind
    ## it. Ask for the op back. `CancelIoEx` is asynchronous — the completion
    ## still arrives — so the Aux block stays out until the drain sees it, and
    ## the generation check there recognises it as already accounted for.
    let lane = ioLane()
    if lane >= gSlotAux.len: return
    let ai = gSlotAux[lane][slotIdx]
    if ai == NoAux: return
    gSlotAux[lane][slotIdx] = NoAux
    let s = socketOf(gSlots[lane].slots[slotIdx].op.fd)
    discard cancelIoEx(cast[Handle](s), addr gAux[lane][ai.int].wov.ov)

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
    gAuxFree = newSeq[seq[int32]](lanes)
    gSlotAux = newSeq[seq[int32]](lanes)
    gPollAdds = newSeq[seq[PendingPoll]](lanes)
    var i = 0
    while i < lanes:
      gPorts[i] = createIoCompletionPort(INVALID_HANDLE_VALUE, cast[Handle](0), 0'u, 1'u32)
      gAux[i] = newSeq[Aux](MaxOps)
      var free = newSeq[int32](MaxOps)
      var slotAux = newSeq[int32](MaxOps)
      var k = 0
      while k < MaxOps:
        gAux[i][k].acceptSock = InvalidSocket
        free[k] = int32(MaxOps - 1 - k)   # popped low-index first
        slotAux[k] = NoAux
        k = k + 1
      gAuxFree[i] = free
      gSlotAux[i] = slotAux
      gPollAdds[i] = @[]
      i = i + 1
    gOwner = initTable[cint, int]()
    gCancelInFlight = iocpCancelInFlight
    result = BackendRelays(
      poll: iocpPoll,
      waits: true,          # `GetQueuedCompletionStatusEx(wait)` is a real wait
      close: iocpClose,
      forgetFd: iocpForgetFd,
    )
