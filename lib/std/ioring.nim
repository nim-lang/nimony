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
export types.EvRead, types.EvWrite
export backend.BackendRelays, backend.CqSize, backend.MaxOps
import ./ioring/platform
from std/posix/posix import Sockaddr_storage, Sockaddr_in, SockLen, FileHandle,
                            SockAddr, InAddr, TSa_Family

var ringState: int = 0

proc setupRing() =
  initPool()
  initOpQueues()
  initSlots()
  gCq = newSeq[IoCompletion](CqSize)
  initPlatformBackend()
  gReactor = backendRelays.poll

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
  while not gOpQueues[ioLane()].tryEnqueue(op):
    discard backendRelays.poll(0)

proc submitNop*(cont = Continuation(fn: nil, env: nil);
                resPtr: nil ptr int = nil): SeqNum =
  result = nextSeqNum()
  var op = OpContext(kind: opNop, fd: -1, seqnum: result,
    cont: cont, res: cast[int](resPtr))
  enqueueOp(op)

proc submitRead*(fd: cint; buf: pointer; len: int;
                 cont = Continuation(fn: nil, env: nil);
                 resPtr: nil ptr int = nil): SeqNum =
  result = nextSeqNum()
  var op = OpContext(kind: opRead, fd: fd, seqnum: result, buf: buf, len: len,
    cont: cont, res: cast[int](resPtr))
  enqueueOp(op)

proc submitWrite*(fd: cint; buf: pointer; len: int;
                 cont = Continuation(fn: nil, env: nil);
                 resPtr: nil ptr int = nil): SeqNum =
  result = nextSeqNum()
  var op = OpContext(kind: opWrite, fd: fd, seqnum: result, buf: buf, len: len,
    cont: cont, res: cast[int](resPtr))
  enqueueOp(op)

proc submitAccept*(listenFd: cint;
                   cont = Continuation(fn: nil, env: nil);
                   resPtr: nil ptr int = nil): SeqNum =
  result = nextSeqNum()
  var op = OpContext(kind: opAccept, fd: listenFd, seqnum: result,
    cont: cont, res: cast[int](resPtr))
  op.acceptAddr = Sockaddr_storage()
  op.acceptLen = SockLen(sizeof(op.acceptAddr))
  enqueueOp(op)

proc submitPollAdd*(fd: cint; mask = EvRead or EvWrite;
                    cont = Continuation(fn: nil, env: nil);
                    resPtr: nil ptr int = nil): SeqNum =
  ## Register oneshot readiness interest in `fd` without issuing any I/O.
  ## When the fd becomes ready in one of the `mask` directions a single
  ## completion fires whose `op` is `opPollAdd` and whose `result` holds the
  ## ready-direction mask (bits `EvRead` and/or `EvWrite`). Unlike
  ## `submitRead`/`submitWrite`, no transfer is performed — the caller decides
  ## what to do with the ready fd (e.g. libcurl's multi-socket engine). This
  ## is oneshot: `complete` frees the slot, so re-arm by calling
  ## `submitPollAdd` again after handling the event.
  ##
  ## **Pass the direction you actually want.** The default watches both, which
  ## is right for a probe with no preference — but a caller waiting to *read*
  ## a socket is woken by mere writability on every arm (a connected socket is
  ## writable nearly always), and because the op is oneshot its re-arm then
  ## spins as fast as the loop can poll. libcurl's multi-socket engine always
  ## states its direction (`CURL_POLL_IN`/`CURL_POLL_OUT`); pass it through.
  result = nextSeqNum()
  var op = OpContext(kind: opPollAdd, fd: fd, seqnum: result,
    cont: cont, res: cast[int](resPtr), pollMask: mask)
  enqueueOp(op)

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
    ## fd number) and any pending slot for this fd stayed in use forever —
    ## a permanent slot-arena leak for every fd closed with an op in flight.
    ##
    ## Order matters: deregister from the backend *before* close(2), so a
    ## fresh fd that the OS immediately reuses for the same number cannot
    ## race with a stale registration/slot that still refers to it.
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
      let slot = addr gSlots[lane].slots[idx]
      const ECancelled = -125
      if slot.op.res != 0:
        cast[ptr int](slot.op.res)[] = ECancelled
      let cont = slot.op.cont
      if cont.fn != nil:
        submit(cont, int(fd))
      gSlots[lane].freeSlot(idx)
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
    addr4.sin_family = TSa_Family(AF_INET)
    addr4.sin_port = htons(port)
    addr4.sin_addr.s_addr = INADDR_ANY
    assert bindAddr(fd, cast[ptr SockAddr](addr addr4),
                    SockLen(sizeof(addr4))) == 0, "bind failed"
    assert listen(fd, backlog.cint) == 0, "listen failed"
    setNonBlocking(fd)
    result = fd

initIoRing()
