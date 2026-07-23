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

import std / [atomics, threadpool, assertions, ticketlocks]
export threadpool.initPool, threadpool.shutdownPool, threadpool.poolStopped

when defined(linux) and hasIouring:
  import std/posix/io_uring
  from std/posix/posix import SockAddr
when defined(posix):
  from std/posix/posix import SockAddr

when defined(windows):
  import windows/winlean
else:
  proc sched_yield(): cint {.importc, header: "<sched.h>".}

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
  MaxOps* = 8192  ## Max concurrent operations (slot table size).
  CqSize = 4096   ## Completion queue capacity; must be power of 2.

type
  IoOp* = enum
    opRead, opWrite, opAccept

  SeqNum* = uint32

  IoCompletion* = object
    id*: SeqNum         ## Sequence number from submission.
    op*: IoOp           ## Which operation completed.
    fd*: cint           ## The fd the request was submitted for.
    result*: int        ## Bytes read/written, new client fd (accept), or -errno.

  OpContext = object
    inUse: bool
    kind: IoOp
    fd: cint
    seqnum: SeqNum
    buf: pointer
    len: int
    cont: Continuation
    res: nil ptr int
    acceptAddr: Sockaddr_storage
    acceptLen: SockLen

  SlabArena = object
    slots: array[MaxOps, OpContext]
    freelist: seq[int]

var
  gNextSeq: uint32
  gArena: SlabArena
  fdPendingCount: array[MaxOps, int32]
  cqLock: TicketLock
  cq: array[CqSize, IoCompletion]
  cqHead, cqTail, cqCount: int

# --- slab allocator ---

proc allocSlot(): int =
  result = gArena.freelist.pop()
  gArena.slots[result].inUse = true

proc freeSlot(idx: int) =
  gArena.slots[idx].inUse = false
  gArena.slots[idx].cont = Continuation(fn: nil, env: nil)
  gArena.slots[idx].res = nil
  gArena.slots[idx].buf = nil
  gArena.freelist.add(idx)

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
    submit(cont, int(c.fd))
  else:
    pushCompletion(c)

# --- event dispatch (epoll/kqueue path) ---

proc findAndRearm(fd: cint) =
  var evMask: uint32 = 0
  var repIdx: int = 0
  var found = false
  for i in 0..<MaxOps:
    if gArena.slots[i].inUse and gArena.slots[i].fd == fd:
      if not found:
        repIdx = i
        found = true
      case gArena.slots[i].kind
      of opRead, opAccept: evMask = evMask or EvRead
      of opWrite: evMask = evMask or EvWrite
  if found:
    rearmFd(fd, repIdx, evMask)

proc processCompletion(idx: int; res: int) =
  let slot = addr gArena.slots[idx]
  var c = IoCompletion(id: slot.seqnum, op: slot.kind, fd: slot.fd)
  c.result = res
  if c.result < 0: c.result = -1

  let rp = slot.res
  if rp != nil:
    rp[] = c.result

  let cont = slot.cont
  slot.cont = Continuation(fn: nil, env: nil)

  freeSlot(idx)
  dec fdPendingCount[slot.fd]

  deliver(c, cont)

proc onEvent(slotIdx: int; events: uint32) {.nimcall.} =
  let fd = gArena.slots[slotIdx].fd

  when defined(posix):
    for i in 0..<MaxOps:
      if not gArena.slots[i].inUse or gArena.slots[i].fd != fd:
        continue
      case gArena.slots[i].kind
      of opRead:
        if (events and EvRead) != 0:
          let r = posixRead(fd, gArena.slots[i].buf, gArena.slots[i].len.csize_t)
          processCompletion(i, r)
      of opWrite:
        if (events and EvWrite) != 0:
          let r = posixWrite(fd, gArena.slots[i].buf, gArena.slots[i].len.csize_t)
          processCompletion(i, r)
      of opAccept:
        if (events and EvRead) != 0:
          let clientFd = accept(fd, cast[ptr SockAddr](addr gArena.slots[i].acceptAddr),
                                addr gArena.slots[i].acceptLen)
          processCompletion(i, clientFd)

  if fdPendingCount[fd] > 0:
    findAndRearm(fd)

when defined(linux) and hasIouring:
  proc handleUrCqe(userData: uint64; res: int32) {.nimcall.} =
    let idx = int(userData shr 1)
    let slot = addr gArena.slots[idx]
    assert slot.inUse

    var c = IoCompletion(id: slot.seqnum, op: slot.kind, fd: slot.fd)
    c.result = res
    if c.result < 0: c.result = -1

    let rp = slot.res
    if rp != nil:
      rp[] = c.result

    let cont = slot.cont
    slot.cont = Continuation(fn: nil, env: nil)

    freeSlot(idx)
    deliver(c, cont)

# --- submission API ---

proc nextSeqNum(): SeqNum =
  SeqNum(atomicFetchAdd(gNextSeq, 1'u32, moRelaxed))

proc tryIoUring(slotIdx: int; fd: cint; kind: IoOp; buf: nil pointer; len: int): bool =
  when defined(linux) and hasIouring:
    if gRingAvail:
      gRingLock.acquire()
      let q = getQueuePtr()
      let sqe = q[].getSqe()
      if sqe != nil:
        case kind
        of opRead:
          discard sqe.read(fd, buf, len, 0)
          discard sqe.setUserData(uint64(slotIdx shl 1) or 0'u)
        of opWrite:
          discard sqe.write(fd, buf, len, 0)
          discard sqe.setUserData(uint64(slotIdx shl 1) or 1'u)
        of opAccept:
          let s = addr gArena.slots[slotIdx]
          s.acceptAddr = default(Sockaddr_storage)
          s.acceptLen = SockLen(sizeof(s.acceptAddr))
          discard accept(sqe, fd, cast[ptr SockAddr](addr s.acceptAddr),
                         addr s.acceptLen, 0)
          discard sqe.setUserData(uint64(slotIdx shl 1) or 0'u)
        inc gPendingSqes
        gRingLock.release()
        return true
      gRingLock.release()
  return false

proc submitRead*(fd: cint; buf: pointer; len: int;
                 cont = Continuation(fn: nil, env: nil);
                 resPtr: nil ptr int = nil): SeqNum =
  result = nextSeqNum()
  let idx = allocSlot()
  var slot = addr gArena.slots[idx]
  slot.kind = opRead
  slot.fd = fd
  slot.seqnum = result
  slot.buf = buf
  slot.len = len
  slot.cont = cont
  slot.res = resPtr

  if not tryIoUring(idx, fd, opRead, buf, len):
    if fdPendingCount[fd] == 0:
      registerFd(fd, idx, EvRead)
    inc fdPendingCount[fd]

proc submitWrite*(fd: cint; buf: pointer; len: int;
                  cont = Continuation(fn: nil, env: nil);
                  resPtr: nil ptr int = nil): SeqNum =
  result = nextSeqNum()
  let idx = allocSlot()
  var slot = addr gArena.slots[idx]
  slot.kind = opWrite
  slot.fd = fd
  slot.seqnum = result
  slot.buf = buf
  slot.len = len
  slot.cont = cont
  slot.res = resPtr

  if not tryIoUring(idx, fd, opWrite, buf, len):
    if fdPendingCount[fd] == 0:
      registerFd(fd, idx, EvWrite)
    inc fdPendingCount[fd]

proc submitAccept*(listenFd: cint;
                   cont = Continuation(fn: nil, env: nil);
                   resPtr: nil ptr int = nil): SeqNum =
  result = nextSeqNum()
  let idx = allocSlot()
  var slot = addr gArena.slots[idx]
  slot.kind = opAccept
  slot.fd = listenFd
  slot.seqnum = result
  slot.cont = cont
  slot.res = resPtr

  if not tryIoUring(idx, listenFd, opAccept, nil, 0):
    if fdPendingCount[listenFd] == 0:
      registerFd(listenFd, idx, EvRead)
    inc fdPendingCount[listenFd]

# --- completion harvesting ---

proc pollCompletions*(comps: var openArray[IoCompletion]): int =
  when defined(linux) and hasIouring:
    submitUrSqes()
    drainUrCqes()

  result = 0
  cqLock.acquire()
  while result < comps.len and cqCount > 0:
    comps[result] = cq[cqHead]
    cqHead = (cqHead + 1) and (CqSize - 1)
    dec cqCount
    inc result
  cqLock.release()

proc waitCompletions*(comps: var openArray[IoCompletion]): int =
  result = 0
  while true:
    result = pollCompletions(comps)
    if result > 0: return
    discard poolPollIo(0)
    when defined(windows):
      sleep(0'u32)
    else:
      discard sched_yield()

# --- lifecycle ---

proc initIoRing*() =
  atomicStore(gNextSeq, 1'u32, moRelaxed)
  gArena.freelist = newSeq[int]()
  for i in 0..<MaxOps:
    gArena.freelist.add(i)
  gPollCb = onEvent
  when defined(linux) and hasIouring:
    gUrCqeCb = handleUrCqe

# --- convenience ---

proc closeFd*(fd: cint) =
  unregisterFd(fd)
  for i in 0..<MaxOps:
    if gArena.slots[i].inUse and gArena.slots[i].fd == fd:
      gArena.slots[i].cont = Continuation(fn: nil, env: nil)
      gArena.slots[i].res = nil
      freeSlot(i)
      dec fdPendingCount[fd]
  fdPendingCount[fd] = 0
  when defined(posix):
    discard posixClose(fd)

proc setNonBlocking*(fd: cint) =
  when defined(posix):
    var flags = fcntl(fd, F_GETFL)
    discard fcntl(fd, F_SETFL, flags or O_NONBLOCK)

proc listenTcp*(port: uint16; backlog = 128): cint =
  when defined(posix):
    let fd = socket(AF_INET, SOCK_STREAM, IPPROTO_TCP)
    assert fd >= 0, "socket() failed"
    var yes: cint = 1
    discard setsockopt(fd, SOL_SOCKET, SO_REUSEADDR, addr yes, SockLen(sizeof(yes)))
    var addr4 = default(Sockaddr_in)
    addr4.sin_family = cushort(AF_INET)
    addr4.sin_port = htons(port)
    addr4.sin_addr.s_addr = INADDR_ANY
    assert bindAddr(fd, cast[ptr SockAddr](addr addr4),
                    SockLen(sizeof(addr4))) == 0, "bind failed"
    assert listen(fd, backlog.cint) == 0, "listen failed"
    setNonBlocking(fd)
    result = fd
