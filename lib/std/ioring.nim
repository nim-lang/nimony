# (c) 2025 Andreas Rumpf
# Shared completion-based I/O ring on top of threadpool.
#
# Any thread can submit I/O requests; completions are delivered either
# by resuming a suspended `.passive` proc (via continuation) or by
# pushing to a shared completion queue for polling.
#
# Usage:
#   let ring = initIoRing()
#   let listenFd = ring.listenTcp(8080)
#   discard ring.submitAccept(listenFd)
#   var comps: array[16, IoCompletion]
#   let n = ring.waitCompletions(comps)
#   echo "client fd=", comps[0].result
#   ring.shutdown()

import std / [atomics, threadpool, assertions, ticketlocks]
import ./ioring/core/[types, slots]
export types.IoCompletion, types.IoOp, types.SeqNum, types.OpContext
import ./ioring/platform
from std/posix/posix import Sockaddr_storage, SockLen, FileHandle, SockAddr, InAddr

# ---------------------------------------------------------------------------
# IoPool — overrides Pool.poll to drive the I/O backend from worker threads
# ---------------------------------------------------------------------------

type IoPool* = ref object of Pool
  backend*: Backend

method poll*(p: IoPool; timeoutMs: int): bool =
  result = p.backend.poll(timeoutMs)

# ---------------------------------------------------------------------------
# IoRing
# ---------------------------------------------------------------------------

const CqSize = 4096

type
  IoRing* = ref object of RootObj
    slots: SlotArena
    nextSeq: SeqNum
    backend: Backend
    pool: IoPool
    cqLock: TicketLock
    cq: array[CqSize, IoCompletion]
    cqHead, cqTail, cqCount: int

# --- completion delivery (called from backend's poll via completeFn) ---

proc completeCb(slotIdx: int; res: int; env: int) {.nimcall.} =
  let ring = cast[IoRing](env)
  let slot = addr ring.slots.slots[slotIdx]

  var c = IoCompletion(id: slot.seqnum, op: slot.kind, fd: slot.fd)
  c.result = res

  if slot.res != 0:
    cast[ptr int](slot.res)[] = c.result

  let cont = slot.cont
  slot.cont = Continuation(fn: nil, env: nil)

  ring.slots.freeSlot(slotIdx)

  if cont.fn != nil:
    ring.pool.submit(cont, int(c.fd))
  else:
    ring.cqLock.acquire()
    if ring.cqCount < CqSize:
      ring.cq[ring.cqTail] = c
      ring.cqTail = (ring.cqTail + 1) and (CqSize - 1)
      inc ring.cqCount
    ring.cqLock.release()

# --- lifecycle ---

proc initIoRing*(): IoRing =
  new result
  result.slots.init()
  result.nextSeq = 1

  when hasIouring:
    import ./ioring/backends/iouring
    result.backend = initIoUringBackend(cast[int](addr result.slots))
  elif hasIoPoll:
    import ./ioring/core/backends
    when hasEpoll:
      import ./ioring/backends/epoll
      result.backend = initEpollBackend(cast[int](addr result.slots))
    elif hasKqueue:
      import ./ioring/backends/kqueue
      result.backend = initKqueueBackend(cast[int](addr result.slots))
  else:
    {.error: "No I/O backend available for this platform".}

  result.backend.completeFn = completeCb
  result.backend.completeEnv = cast[int](result)
  result.pool = IoPool()
  result.pool.backend = result.backend
  result.pool.init()

proc shutdown*(ring: IoRing) =
  ring.pool.shutdown()
  ring.backend.close()

# --- sequence numbers ---

proc nextSeqNum(ring: IoRing): SeqNum =
  SeqNum(atomicFetchAdd(ring.nextSeq, 1'u32, moRelaxed))

# --- submission API ---

proc submitRead*(ring: IoRing; fd: cint; buf: pointer; len: int;
                 cont = Continuation(fn: nil, env: nil);
                 resPtr: nil ptr int = nil): SeqNum =
  result = ring.nextSeqNum()
  let idx = ring.slots.allocSlot()
  let op = ring.slots.addrSlot(idx)
  op.kind = opRead
  op.fd = fd
  op.seqnum = result
  op.buf = buf
  op.len = len
  op.cont = cont
  op.res = cast[int](resPtr)
  ring.backend.submit(idx, op)

proc submitWrite*(ring: IoRing; fd: cint; buf: pointer; len: int;
                  cont = Continuation(fn: nil, env: nil);
                  resPtr: nil ptr int = nil): SeqNum =
  result = ring.nextSeqNum()
  let idx = ring.slots.allocSlot()
  let op = ring.slots.addrSlot(idx)
  op.kind = opWrite
  op.fd = fd
  op.seqnum = result
  op.buf = buf
  op.len = len
  op.cont = cont
  op.res = cast[int](resPtr)
  ring.backend.submit(idx, op)

proc submitAccept*(ring: IoRing; listenFd: cint;
                   cont = Continuation(fn: nil, env: nil);
                   resPtr: nil ptr int = nil): SeqNum =
  result = ring.nextSeqNum()
  let idx = ring.slots.allocSlot()
  let op = ring.slots.addrSlot(idx)
  op.kind = opAccept
  op.fd = listenFd
  op.seqnum = result
  op.cont = cont
  op.res = cast[int](resPtr)
  op.acceptAddr = Sockaddr_storage()
  op.acceptLen = SockLen(sizeof(op.acceptAddr))
  ring.backend.submit(idx, op)

# --- completion harvesting ---

proc pollCompletions*(ring: IoRing; comps: var openArray[IoCompletion]): int =
  result = 0
  ring.cqLock.acquire()
  while result < comps.len and ring.cqCount > 0:
    comps[result] = ring.cq[ring.cqHead]
    ring.cqHead = (ring.cqHead + 1) and (CqSize - 1)
    dec ring.cqCount
    inc result
  ring.cqLock.release()

proc waitCompletions*(ring: IoRing; comps: var openArray[IoCompletion]): int =
  result = 0
  while true:
    result = ring.pollCompletions(comps)
    if result > 0: return
    discard ring.backend.poll(0)

# --- convenience: fd helpers ---

when defined(posix):
  proc posixClose(fd: cint): cint {.importc: "close", header: "<unistd.h>".}
  proc fcntl(fd: cint; cmd: cint): cint {.varargs, importc, header: "<fcntl.h>".}
  const F_GETFL* = 3.cint
  const F_SETFL* = 4.cint
  when defined(linux):
    const O_NONBLOCK* = 0x0800.cint
  else:
    const O_NONBLOCK* = 0x0004.cint

  proc setNonBlocking*(fd: cint) =
    var flags = fcntl(fd, F_GETFL)
    discard fcntl(fd, F_SETFL, flags or O_NONBLOCK)

  proc closeFd*(fd: cint) =
    discard posixClose(fd)

when defined(posix):
  type
    Sockaddr_in* {.importc: "struct sockaddr_in", header: "<netinet/in.h>".} = object
      sin_family*: cushort
      sin_port*: cushort
      sin_addr*: InAddr

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
  proc htons(x: uint16): uint16 {.importc, header: "<arpa/inet.h>".}

  proc listenTcp*(ring: IoRing; port: uint16; backlog = 128): cint =
    let fd = socket(AF_INET, SOCK_STREAM, IPPROTO_TCP)
    assert fd >= 0, "socket() failed"
    var yes: cint = 1
    discard setsockopt(fd, SOL_SOCKET, SO_REUSEADDR, addr yes, SockLen(sizeof(yes)))
    var addr4 = Sockaddr_in(sin_family: cushort(AF_INET), sin_port: htons(port),
                            sin_addr: InAddr(s_addr: INADDR_ANY))
    assert bindAddr(fd, cast[ptr SockAddr](addr addr4),
                    SockLen(sizeof(addr4))) == 0, "bind failed"
    assert listen(fd, backlog.cint) == 0, "listen failed"
    setNonBlocking(fd)
    result = fd
