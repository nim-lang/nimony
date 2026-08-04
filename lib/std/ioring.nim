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
import ./ioring/core/[types, slots, backend]
export types.IoCompletion, types.IoOp, types.SeqNum, types.OpContext
export backend.Ring, backend.Backend, backend.CqSize
import ./ioring/platform
from std/posix/posix import Sockaddr_storage, SockLen, FileHandle, SockAddr, InAddr

proc initIoRing*(pool: nil Pool = nil): Ring =
  ## `pool`, if given, is the worker pool whose idle loop will drive this
  ## ring's I/O backend (via `Pool.registerReactor`) — pass one you built
  ## with `createPool()` if this ring's reactor should have dedicated
  ## threads (e.g. to keep IO-wait threads separate from CPU-bound `parfor`
  ## work). Left as `nil` (the default), the ring shares the process-wide
  ## `defaultPool()` with everything else that hasn't asked for isolation —
  ## previously every `Ring` unconditionally started its own private
  ## `WorkerCount` threads, so N rings meant N*WorkerCount OS threads
  ## contending for the same cores, on top of whatever `parfor` started.
  new result
  result.slots = SlotArena()
  result.slots.init()
  result.cq = newSeq[IoCompletion](CqSize)
  result.nextSeq = 1
  initPlatformBackend(result)
  result.pool = if pool != nil: pool else: defaultPool()
  let ring = result
  result.pool.registerReactor(proc(timeoutMs: int): bool {.closure.} =
    if atomicLoad(ring.closed, moRelaxed): return false
    ring.backend.poll(timeoutMs)
  )

proc shutdown*(ring: Ring) =
  ## Closes this ring's backend only. Does **not** shut down `ring.pool`:
  ## that pool may be the shared `defaultPool()` (or one the caller passed
  ## in and still owns), so tearing it down here would kill worker threads
  ## out from under every other user of that pool. Whoever created the pool
  ## (via `createPool()`) owns its `shutdown()`; `defaultPool()` is
  ## intended to live for the process's lifetime.
  atomicStore(ring.closed, true, moRelaxed)
  ring.backend.close()

proc nextSeqNum(ring: Ring): SeqNum =
  SeqNum(atomicFetchAdd(ring.nextSeq, 1'u32, moRelaxed))

proc submitRead*(ring: Ring; fd: cint; buf: pointer; len: int;
                 cont = Continuation(fn: nil, env: nil);
                 resPtr: nil ptr int = nil): SeqNum =
  result = ring.nextSeqNum()
  let idx = ring.slots.allocSlot(fd)
  let op = ring.slots.addrSlot(idx)
  op.kind = opRead
  op.fd = fd
  op.seqnum = result
  op.buf = buf
  op.len = len
  op.cont = cont
  op.res = cast[int](resPtr)
  ring.backend.submit(idx, op)

proc submitWrite*(ring: Ring; fd: cint; buf: pointer; len: int;
                  cont = Continuation(fn: nil, env: nil);
                  resPtr: nil ptr int = nil): SeqNum =
  result = ring.nextSeqNum()
  let idx = ring.slots.allocSlot(fd)
  let op = ring.slots.addrSlot(idx)
  op.kind = opWrite
  op.fd = fd
  op.seqnum = result
  op.buf = buf
  op.len = len
  op.cont = cont
  op.res = cast[int](resPtr)
  ring.backend.submit(idx, op)

proc submitAccept*(ring: Ring; listenFd: cint;
                   cont = Continuation(fn: nil, env: nil);
                   resPtr: nil ptr int = nil): SeqNum =
  result = ring.nextSeqNum()
  let idx = ring.slots.allocSlot(listenFd)
  let op = ring.slots.addrSlot(idx)
  op.kind = opAccept
  op.fd = listenFd
  op.seqnum = result
  op.cont = cont
  op.res = cast[int](resPtr)
  op.acceptAddr = Sockaddr_storage()
  op.acceptLen = SockLen(sizeof(op.acceptAddr))
  ring.backend.submit(idx, op)

proc pollCompletions*(ring: Ring; comps: var openArray[IoCompletion]): int =
  result = 0
  ring.cqLock.acquire()
  while result < comps.len and ring.cqCount > 0:
    comps[result] = ring.cq[ring.cqHead]
    ring.cqHead = (ring.cqHead + 1) and (CqSize - 1)
    dec ring.cqCount
    inc result
  ring.cqLock.release()

proc waitCompletions*(ring: Ring; comps: var openArray[IoCompletion]): int =
  result = 0
  while true:
    result = ring.pollCompletions(comps)
    if result > 0: return
    discard ring.backend.poll(0)

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
    ## Close a fd that is known to have no in-flight ring ops on it (e.g. one
    ## that was never submitted through `ring`). Prefer `ring.closeFd` for
    ## any fd that may have pending reads/writes/accepts.
    discard posixClose(fd)

  proc closeFd*(ring: Ring; fd: cint) =
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
    ring.backend.forgetFd(fd)
    let onCancel = proc(idx: int) {.closure.} =
      let slot = addr ring.slots.slots[idx]
      const ECancelled = -125 # -ECANCELED
      if slot.res != 0:
        cast[ptr int](slot.res)[] = ECancelled
      let cont = slot.cont
      if cont.fn != nil:
        ring.pool.submit(cont, int(fd))
    ring.slots.cancelAllForFd(fd, onCancel)
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
    
  proc listenTcp*(ring: Ring; port: uint16; backlog = 128): cint =
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
