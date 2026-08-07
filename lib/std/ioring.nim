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
export backend.BackendRelays, backend.CqSize
import ./ioring/platform
from std/posix/posix import Sockaddr_storage, SockLen, FileHandle, SockAddr, InAddr

proc poll(timeoutMs: int): bool {.nimcall.} =
  backendRelays.poll(timeoutMs)

proc initIoRing*() =
  gSlots = SlotArena()
  gSlots.init()
  gCq = newSeq[IoCompletion](CqSize)
  gNextSeq = 1
  initPlatformBackend()
  gReactor = poll
  initPool()

proc shutdown*() =
  atomicStore(gClosed, true, moRelaxed)
  backendRelays.close()

proc nextSeqNum(): SeqNum =
  SeqNum(atomicFetchAdd(gNextSeq, 1'u32, moRelaxed))

proc submitNop*(cont = Continuation(fn: nil, env: nil);
                resPtr: nil ptr int = nil): SeqNum =
  result = nextSeqNum()
  let idx = gSlots.allocSlot(-1)
  let op = gSlots.addrSlot(idx)
  op.kind = opNop
  op.fd = -1
  op.seqnum = result
  op.cont = cont
  op.res = cast[int](resPtr)
  backendRelays.submit(idx, op)

proc submitRead*(fd: cint; buf: pointer; len: int;
                 cont = Continuation(fn: nil, env: nil);
                 resPtr: nil ptr int = nil): SeqNum =
  result = nextSeqNum()
  let idx = gSlots.allocSlot(fd)
  let op = gSlots.addrSlot(idx)
  op.kind = opRead
  op.fd = fd
  op.seqnum = result
  op.buf = buf
  op.len = len
  op.cont = cont
  op.res = cast[int](resPtr)
  backendRelays.submit(idx, op)

proc submitWrite*(fd: cint; buf: pointer; len: int;
                  cont = Continuation(fn: nil, env: nil);
                  resPtr: nil ptr int = nil): SeqNum =
  result = nextSeqNum()
  let idx = gSlots.allocSlot(fd)
  let op = gSlots.addrSlot(idx)
  op.kind = opWrite
  op.fd = fd
  op.seqnum = result
  op.buf = buf
  op.len = len
  op.cont = cont
  op.res = cast[int](resPtr)
  backendRelays.submit(idx, op)

proc submitAccept*(listenFd: cint;
                   cont = Continuation(fn: nil, env: nil);
                   resPtr: nil ptr int = nil): SeqNum =
  result = nextSeqNum()
  let idx = gSlots.allocSlot(listenFd)
  let op = gSlots.addrSlot(idx)
  op.kind = opAccept
  op.fd = listenFd
  op.seqnum = result
  op.cont = cont
  op.res = cast[int](resPtr)
  op.acceptAddr = Sockaddr_storage()
  op.acceptLen = SockLen(sizeof(op.acceptAddr))
  backendRelays.submit(idx, op)

proc pollCompletions*(comps: var openArray[IoCompletion]): int =
  result = 0
  gCqLock.acquire()
  while result < comps.len and gCqCount > 0:
    comps[result] = gCq[gCqHead]
    gCqHead = (gCqHead + 1) and (CqSize - 1)
    dec gCqCount
    inc result
  gCqLock.release()

proc waitCompletions*(comps: var openArray[IoCompletion]): int =
  result = 0
  while true:
    result = pollCompletions(comps)
    if result > 0: return
    discard backendRelays.poll(0)

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
    ## Close `fd`, first cancelling any ops still in flight on it so their
    ## continuations are resumed (with a cancellation result) instead of
    ## leaking, and deregistering it from the backend before the actual
    ## close(). Previously `closeFd` only called close(2): the backend never
    ## found out (so epoll/kqueue kept a registration for a possibly-reused
    ## fd number) and any pending slot for this fd stayed `inUse` forever —
    ## a permanent slot-arena leak for every fd closed with an op in flight.
    ##
    ## Order matters: deregister from the backend *before* close(2), so a
    ## fresh fd that the OS immediately reuses for the same number cannot
    ## race with a stale registration/slot that still refers to it.
    backendRelays.forgetFd(fd)
    let onCancel = proc(idx: int) {.closure.} =
      let slot = addr gSlots.slots[idx]
      const ECancelled = -125
      if slot.res != 0:
        cast[ptr int](slot.res)[] = ECancelled
      let cont = slot.cont
      if cont.fn != nil:
        submit(cont, int(fd))
    gSlots.cancelAllForFd(fd, onCancel)
    discard posixClose(fd)

when defined(posix):
  type
    Sockaddr_in* {.importc: "sockaddr_in".} = object
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
  proc socket(domain, typ, protocol: cint): cint {.importc: "socket".}
  proc setsockopt(s: cint; level, optname: cint; val: pointer; vlen: SockLen): cint {.importc: "setsockopt".}
  proc bindAddr(s: cint; name: ptr SockAddr; namelen: SockLen): cint {.importc: "bind".}
  proc listen(s: cint; backlog: cint): cint {.importc: "listen".}
  proc htons(x: uint16): uint16 {.inline.} =
    ## Header macro/libc shim; a byte swap on the little-endian targets.
    when defined(bigEndian):
      result = x
    else:
      result = (x shl 8) or (x shr 8)

  proc listenTcp*(port: uint16; backlog = 128): cint =
    let fd = socket(AF_INET, SOCK_STREAM, IPPROTO_TCP)
    assert fd >= 0, "socket() failed"
    var yes: cint = 1
    discard setsockopt(fd, SOL_SOCKET, SO_REUSEADDR, addr yes, SockLen(sizeof(yes)))
    var addr4 = default(Sockaddr_in)
    when defined(linux):
      addr4.sin_family = cushort(AF_INET)
    else:
      addr4.sin_family = uint8(AF_INET)
    addr4.sin_port = htons(port)
    addr4.sin_addr.s_addr = INADDR_ANY
    assert bindAddr(fd, cast[ptr SockAddr](addr addr4),
                    SockLen(sizeof(addr4))) == 0, "bind failed"
    assert listen(fd, backlog.cint) == 0, "listen failed"
    setNonBlocking(fd)
    result = fd

var ringState: int = 0
proc initDefaultRing() =
  if atomicLoad(ringState, moAcquire) == 2: return
  var expected = 0
  if atomicCompareExchange(ringState, expected, 1):
    initIoRing()
    atomicStore(ringState, 2, moRelease)
  else:
    while atomicLoad(ringState, moAcquire) != 2:
      discard
initDefaultRing()
