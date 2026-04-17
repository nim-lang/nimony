# (c) 2025 Andreas Rumpf
# Shared completion-based I/O ring on top of threadpool.
#
# Any thread can submit I/O requests; completions are delivered either
# by resuming a suspended `.passive` proc (via continuation) or by
# pushing to a shared completion queue for polling.
#
# Usage:
#   initPool()
#   initIoRing()
#   let listenFd = listenTcp(8080)
#   discard submitAccept(listenFd)
#   var comps: array[16, IoCompletion]
#   let n = waitCompletions(comps)
#   echo "client fd=", comps[0].result
#   shutdownPool()

import std / [atomics, threadpool]
export threadpool.initPool, threadpool.shutdownPool, threadpool.poolStopped

when defined(posix):
  proc posixRead(fd: cint; buf: pointer; count: csize_t): int {.
    importc: "read", header: "<unistd.h>".}
  proc posixWrite(fd: cint; buf: pointer; count: csize_t): int {.
    importc: "write", header: "<unistd.h>".}
  proc posixClose(fd: cint): cint {.importc: "close", header: "<unistd.h>".}

  proc fcntl(fd: cint; cmd: cint): cint {.varargs, importc, header: "<fcntl.h>".}
  const F_GETFL* = 3.cint
  const F_SETFL* = 4.cint
  when defined(linux):
    const O_NONBLOCK* = 0x0800.cint
  else:
    const O_NONBLOCK* = 0x0004.cint

  type
    SockLen* = cuint
    Sockaddr_storage* {.importc: "struct sockaddr_storage",
                        header: "<sys/socket.h>".} = object
    SockAddr* {.importc: "struct sockaddr", header: "<sys/socket.h>".} = object
    Sockaddr_in* {.importc: "struct sockaddr_in", header: "<netinet/in.h>".} = object
      sin_family*: cushort
      sin_port*: cushort
      sin_addr*: InAddr
    InAddr* {.importc: "struct in_addr", header: "<netinet/in.h>".} = object
      s_addr*: uint32

  const
    AF_INET* = 2.cint
    SOCK_STREAM* = 1.cint
    IPPROTO_TCP* = 6.cint
    SOL_SOCKET* = (when defined(macosx): 0xFFFF.cint else: 1.cint)
    SO_REUSEADDR* = (when defined(macosx): 4.cint else: 2.cint)
    INADDR_ANY* = 0'u32

  proc socket(domain, typ, protocol: cint): cint {.importc, header: "<sys/socket.h>".}
  proc setsockopt(s: cint; level, optname: cint; optval: pointer; optlen: SockLen): cint {.
    importc, header: "<sys/socket.h>".}
  proc bindAddr(s: cint; name: ptr SockAddr; namelen: SockLen): cint {.
    importc: "bind", header: "<sys/socket.h>".}
  proc listen(s: cint; backlog: cint): cint {.importc, header: "<sys/socket.h>".}
  proc accept(s: cint; `addr`: ptr SockAddr; addrlen: ptr SockLen): cint {.
    importc, header: "<sys/socket.h>".}
  proc htons(x: uint16): uint16 {.importc, header: "<arpa/inet.h>".}

const
  MaxFds* = 8192  ## fd-indexed slot table size.
  CqSize = 4096                 ## Completion queue capacity; must be power of 2.

type
  IoOp* = enum
    opRead, opWrite, opAccept

  SeqNum* = uint32

  IoCompletion* = object
    id*: SeqNum         ## Sequence number from submission.
    op*: IoOp           ## Which operation completed.
    fd*: cint           ## The fd the request was submitted for.
    result*: int        ## Bytes read/written, new client fd (accept), or -errno.

  FdSlot = object
    handler: IoHandler  # must be first field -- tpool casts ptr IoHandler back
    readId: SeqNum
    readOp: IoOp
    readBuf: pointer
    readLen: int
    readCont: Continuation  ## Continuation to resume on read completion.
    hasRead: bool
    writeId: SeqNum
    writeBuf: pointer
    writeLen: int
    writeCont: Continuation ## Continuation to resume on write completion.
    hasWrite: bool
    registered: bool    # whether fd is in the poller (for epoll ADD vs MOD)

var
  gNextSeq: uint32  # accessed atomically
  fdSlots: array[MaxFds, FdSlot]

  cqLock: TicketLock
  cq: array[CqSize, IoCompletion]
  cqHead, cqTail, cqCount: int

# --- completion delivery ---

proc pushCompletion(c: IoCompletion) =
  cqLock.acquire()
  if cqCount < CqSize:
    cq[cqTail] = c
    cqTail = (cqTail + 1) and (CqSize - 1)
    inc cqCount
  cqLock.release()

proc deliver(c: IoCompletion; cont: Continuation) {.inline.} =
  if cont.fn != nil:
    # Resume the suspended passive proc by submitting its continuation
    # back to the threadpool.
    submit(cont)
  else:
    pushCompletion(c)

# --- IoHandler callback (runs on worker threads) ---

proc onFdReady(self: ptr IoHandler; events: uint32) {.nimcall.} =
  let slot = cast[ptr FdSlot](self)
  let fd = slot.handler.fd

  when defined(posix):
    # Complete read/accept if readable
    if (events and EvRead) != 0 and slot.hasRead:
      var c = IoCompletion(id: slot.readId, op: slot.readOp, fd: fd)
      case slot.readOp
      of opRead:
        c.result = posixRead(fd, slot.readBuf, slot.readLen.csize_t)
        if c.result < 0:
          c.result = -1
      of opAccept:
        var clientAddr: Sockaddr_storage
        var addrLen = SockLen(sizeof(clientAddr))
        let clientFd = accept(fd, cast[ptr SockAddr](addr clientAddr), addr addrLen)
        c.result = clientFd
      of opWrite: discard
      let cont = slot.readCont
      slot.hasRead = false
      slot.readCont = Continuation(fn: nil, env: nil)
      deliver(c, cont)

    # Complete write if writable
    if (events and EvWrite) != 0 and slot.hasWrite:
      var c = IoCompletion(id: slot.writeId, op: opWrite, fd: fd)
      c.result = posixWrite(fd, slot.writeBuf, slot.writeLen.csize_t)
      if c.result < 0:
        c.result = -1
      let cont = slot.writeCont
      slot.hasWrite = false
      slot.writeCont = Continuation(fn: nil, env: nil)
      deliver(c, cont)

  # Re-arm if any requests still pending
  var evMask: uint32 = 0
  if slot.hasRead:  evMask = evMask or EvRead
  if slot.hasWrite: evMask = evMask or EvWrite
  if evMask != 0:
    rearmFd(fd, addr slot.handler, evMask)

# --- submission API ---

proc nextSeqNum(): SeqNum =
  SeqNum(atomicFetchAdd(gNextSeq, 1'u32, moRelaxed))

proc armSlot(fd: cint) =
  let slot = addr fdSlots[fd]
  var evMask: uint32 = 0
  if slot.hasRead:  evMask = evMask or EvRead
  if slot.hasWrite: evMask = evMask or EvWrite
  if evMask == 0: return
  if slot.registered:
    rearmFd(fd, addr slot.handler, evMask)
  else:
    registerFd(fd, addr slot.handler, evMask)
    slot.registered = true

proc submitRead*(fd: cint; buf: pointer; len: int;
                 cont = Continuation(fn: nil, env: nil)): SeqNum =
  ## Submit a read request. If `cont` has a non-nil fn, the continuation
  ## is resumed on a worker thread when the read completes. Otherwise
  ## the completion goes to the shared CQ.
  result = nextSeqNum()
  let slot = addr fdSlots[fd]
  slot.handler.fd = fd
  slot.handler.cb = onFdReady
  slot.readId = result
  slot.readOp = opRead
  slot.readBuf = buf
  slot.readLen = len
  slot.readCont = cont
  slot.hasRead = true
  armSlot(fd)

proc submitWrite*(fd: cint; buf: pointer; len: int;
                  cont = Continuation(fn: nil, env: nil)): SeqNum =
  ## Submit a write request. If `cont` has a non-nil fn, the continuation
  ## is resumed when the write completes. Otherwise completion goes to CQ.
  result = nextSeqNum()
  let slot = addr fdSlots[fd]
  slot.handler.fd = fd
  slot.handler.cb = onFdReady
  slot.writeId = result
  slot.writeBuf = buf
  slot.writeLen = len
  slot.writeCont = cont
  slot.hasWrite = true
  armSlot(fd)

proc submitAccept*(listenFd: cint;
                   cont = Continuation(fn: nil, env: nil)): SeqNum =
  ## Submit an accept request. Completion's `result` is the new client fd.
  result = nextSeqNum()
  let slot = addr fdSlots[listenFd]
  slot.handler.fd = listenFd
  slot.handler.cb = onFdReady
  slot.readId = result
  slot.readOp = opAccept
  slot.readBuf = nil
  slot.readLen = 0
  slot.readCont = cont
  slot.hasRead = true
  armSlot(listenFd)

# --- completion harvesting (CQ path) ---

proc pollCompletions*(comps: var openArray[IoCompletion]): int =
  ## Non-blocking drain of the completion queue. Returns count.
  result = 0
  cqLock.acquire()
  while result < comps.len and cqCount > 0:
    comps[result] = cq[cqHead]
    cqHead = (cqHead + 1) and (CqSize - 1)
    dec cqCount
    inc result
  cqLock.release()

proc waitCompletions*(comps: var openArray[IoCompletion]): int =
  ## Block until at least one completion is ready.
  while true:
    result = pollCompletions(comps)
    if result > 0: return
    sleep(0)

# --- lifecycle ---

proc initIoRing*() =
  atomicStore(gNextSeq, 1'u32, moRelaxed)

# --- convenience ---

proc closeFd*(fd: cint) =
  ## Close fd, unregister from poller, clear slot.
  let slot = addr fdSlots[fd]
  if slot.registered:
    unregisterFd(fd)
    slot.registered = false
  slot.hasRead = false
  slot.hasWrite = false
  slot.readCont = Continuation(fn: nil, env: nil)
  slot.writeCont = Continuation(fn: nil, env: nil)
  when defined(posix):
    discard posixClose(fd)

proc setNonBlocking*(fd: cint) =
  when defined(posix):
    var flags = fcntl(fd, F_GETFL)
    discard fcntl(fd, F_SETFL, flags or O_NONBLOCK)

proc listenTcp*(port: uint16; backlog = 128): cint =
  ## Create a non-blocking TCP listen socket. Returns the fd.
  when defined(posix):
    let fd = socket(AF_INET, SOCK_STREAM, IPPROTO_TCP)
    assert fd >= 0, "socket() failed"
    var yes: cint = 1
    discard setsockopt(fd, SOL_SOCKET, SO_REUSEADDR, addr yes, SockLen(sizeof(yes)))
    var addr4: Sockaddr_in
    addr4.sin_family = cushort(AF_INET)
    addr4.sin_port = htons(port)
    addr4.sin_addr.s_addr = INADDR_ANY
    assert bindAddr(fd, cast[ptr SockAddr](addr addr4),
                    SockLen(sizeof(addr4))) == 0, "bind failed"
    assert listen(fd, backlog.cint) == 0, "listen failed"
    setNonBlocking(fd)
    result = fd
