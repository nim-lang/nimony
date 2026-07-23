# (c) 2025 Andreas Rumpf
# Lock-striped thread pool with continuation-based scheduling.
#
# Workers contend on independent stripes to reduce lock pressure.
# Each worker polls I/O every iteration; the timeout doubles as idle sleep.
# Supports epoll (Linux), kqueue (macOS/BSD), or plain sleep fallback.
#
# A Task wraps a Continuation plus metadata. The pool schedules Tasks;
# the worker trampolines the inner continuation.

{.feature: "lenientnils".}

import std / [atomics, rawthreads, assertions, ticketlocks, private/syslocks]

when defined(linux):
  const hasEpoll = true
  const hasKqueue = false
  when not defined(nimony):
    const hasIouring* = true
  else:
    const hasIouring* = false
elif defined(macosx) or defined(freebsd) or defined(netbsd) or
     defined(openbsd) or defined(dragonfly):
  const hasEpoll = false
  const hasKqueue = true
  const hasIouring* = false
else:
  const hasEpoll = false
  const hasKqueue = false
  const hasIouring* = false

const hasIoPoll* = hasEpoll or hasKqueue

when hasIouring:
  from std/posix/io_uring import Queue, submit, copyCqes, Cqe, newQueue,
    getSqe, read, write, accept, setUserData
  from std/posix/posix import SockAddr, SockLen

  type
    UrCqeCallback* = proc (userData: uint64; res: int32): void {.nimcall.}

  var
    gRing: Queue
    # XXX: each thread better have its own gRing
    gRingLock*: TicketLock
    gRingAvail*: bool
    gPendingSqes*: int
    gUrCqeCb*: UrCqeCallback

  proc getQueuePtr*(): ptr Queue {.inline.} = cast[ptr Queue](gRing)

when not hasIoPoll:
  when defined(windows):
    import windows/winlean
  else:
    proc usleepMicroseconds(usec: cuint): cint {.importc, header: "<unistd.h>".}

# --- Epoll bindings ---

when hasEpoll:
  const
    EPOLLIN* = 0x001'u32
    EPOLLOUT* = 0x004'u32
    EPOLLONESHOT* = 1'u32 shl 30

  type
    EpollData {.importc: "epoll_data_t", header: "<sys/epoll.h>".} = object
      p* {.importc: "ptr".}: pointer

    EpollEvent* {.importc: "struct epoll_event", header: "<sys/epoll.h>".} = object
      events*: uint32
      data*: EpollData

  const
    EPOLL_CTL_ADD* = 1.cint
    EPOLL_CTL_DEL* = 2.cint
    EPOLL_CTL_MOD* = 3.cint

  proc epoll_create1(flags: cint): cint {.importc, header: "<sys/epoll.h>".}
  proc epoll_ctl(epfd: cint; op: cint; fd: cint; event: ptr EpollEvent): cint {.
    importc, header: "<sys/epoll.h>".}
  proc epoll_wait(epfd: cint; events: ptr EpollEvent; maxevents: cint;
                  timeout: cint): cint {.importc, header: "<sys/epoll.h>".}

# --- Kqueue bindings ---

when hasKqueue:
  when not declared(Time):
    when defined(linux):
      type Time = clong
    else:
      type Time = int

  type
    Timespec {.importc: "struct timespec", header: "<time.h>".} = object
      tv_sec: Time
      tv_nsec: clong

  const
    EVFILT_READ* = cshort(-1)
    EVFILT_WRITE* = cshort(-2)
    EV_ADD* = cushort(0x0001)
    EV_DELETE* = cushort(0x0002)
    EV_ONESHOT* = cushort(0x0010)
    EV_ENABLE* = cushort(0x0004)

  type
    KEvent* {.importc: "struct kevent", header: "<sys/event.h>".} = object
      ident*: csize_t     ## identifier for this event (uintptr_t)
      filter*: cshort     ## filter for event
      flags*: cushort     ## action flags for kqueue
      fflags*: cuint      ## filter flag value
      data*: int          ## filter data value
      udata*: pointer     ## opaque user data identifier

  proc kqueue*(): cint {.importc, header: "<sys/event.h>".}
  proc kevent*(kq: cint; changelist: ptr KEvent; nchanges: cint;
               eventlist: ptr KEvent; nevents: cint;
               timeout: ptr Timespec): cint {.importc, header: "<sys/event.h>".}

# --- Unified event mask ---

const
  EvRead*  = 1u32  ## Readable event.
  EvWrite* = 4u32  ## Writable event.

# --- Configuration ---

const
  StripeCount* = 8    ## Must be a power of 2.
  StripeSize*  = 128  ## Tasks per stripe; must be a power of 2.
  WorkerCount* = 8
  MaxIoEvents  = 64
  BulkSize*    = 16   ## Max tasks drained per bulk dequeue.

# --- Task = Continuation + metadata ---

type
  Task* = object
    ## A schedulable unit of work. Wraps a CPS Continuation so that
    ## workers can trampoline `.passive` procs. Extra fields can be
    ## added here for priority, cancellation tokens, diagnostics, etc.
    con*: Continuation

proc toTask*(c: Continuation): Task {.inline.} =
  Task(con: c)

# --- Task queue ---

type
  Stripe = object
    L: TicketLock
    head, tail, count: int
    data: array[StripeSize, Task]

  IoHandler* = object
    ## Heap-allocate and pass ptr to registerFd.
    ## Kept alive until unregisterFd is called.
    ## Embed as the first field of a larger struct to carry per-connection state,
    ## then cast `ptr IoHandler` back to your struct pointer inside the callback.
    fd*: cint
    cb*: proc (self: ptr IoHandler; events: uint32) {.nimcall.}

# --- IoRequest (backend-agnostic I/O request) ---

type
  IoOpKind* = enum opIoRead, opIoWrite, opIoAccept

  IoRequest* = object
    case kind*: IoOpKind
    of opIoRead:
      readFd*: cint
      readBuf*: pointer
      readLen*: int
      readUserData*: uint64
    of opIoWrite:
      writeFd*: cint
      writeBuf*: pointer
      writeLen*: int
      writeUserData*: uint64
    of opIoAccept:
      acceptFd*: cint
      acceptAddrBuf*: pointer
      acceptAddrLen*: pointer
      acceptUserData*: uint64

const IoReqQueueSize = 256  ## Power of 2.

type
  IoReqQueue = object
    L: SysLock
    notFull: SysCond
    head, tail, count: int
    data: array[IoReqQueueSize, IoRequest]

var ioReqQueue: IoReqQueue
var armFdCallback*: proc (fd: cint) {.nimcall.}
var gPollCb*: proc (slotIdx: int; events: uint32) {.nimcall.}

var
  stripes: array[StripeCount, Stripe]
  workers {.noinit.}: array[WorkerCount, RawThread]
  gIoFd: cint
  stopFlag: bool  # accessed atomically

# --- Submit / dequeue ---

proc tryEnqueue(s: int; t: Task): bool {.inline.} =
  ## Push `t` onto stripe `s` if it has room; `false` when the stripe is full.
  let i = s and (StripeCount - 1)
  stripes[i].L.acquire()
  result = stripes[i].count < StripeSize
  if result:
    stripes[i].data[stripes[i].tail] = t
    stripes[i].tail = (stripes[i].tail + 1) and (StripeSize - 1)
    inc stripes[i].count
  stripes[i].L.release()

proc submit*(t: Task; hint = 0) =
  ## Submit a task to the pool. **Non-lossy with "caller-runs" backpressure:**
  ## try the hinted stripe, then the others (absorbing bursts); if *every*
  ## stripe is full, run the continuation inline — trampolining it on the
  ## calling thread and handing the remainder back to the pool the moment a
  ## slot frees — rather than dropping it or blocking.
  ##
  ## Why caller-runs and not a blocking wait: workers re-submit continuations
  ## from inside the trampoline (see `workerLoop`). A blocking `submit` could
  ## park every worker on a full queue with none left to drain it -> deadlock.
  ## Caller-runs instead guarantees forward progress (a producer that outruns
  ## the pool simply does the work), so the queue stays bounded at
  ## `StripeCount*StripeSize` and no submitter ever stalls.
  let h = hint and (StripeCount - 1)
  if tryEnqueue(h, t): return
  for off in 1 ..< StripeCount:
    if tryEnqueue(h + off, t): return
  # Saturated: run it here. `c.fn` returns the next continuation, or one whose
  # `fn` is nil when the task completes or parks (I/O will resume a parked one).
  var c = t.con
  while true:
    let next = c.fn(c.env)
    if next.fn == nil: break
    if tryEnqueue(h, toTask(next)): break
    c = next

proc submit*(c: Continuation; hint = 0) {.inline.} =
  ## Convenience: submit a bare continuation as a task.
  submit(toTask(c), hint)

proc tryBulkDequeue(stripe: int; buf: var array[BulkSize, Task]): int =
  let s = stripe and (StripeCount - 1)
  stripes[s].L.acquire()
  result = min(stripes[s].count, BulkSize)
  for i in 0 ..< result:
    buf[i] = stripes[s].data[stripes[s].head]
    stripes[s].head = (stripes[s].head + 1) and (StripeSize - 1)
  dec stripes[s].count, result
  stripes[s].L.release()

proc drainOnce(startStripe: int): bool =
  ## Dequeue the first non-empty stripe (searching from `startStripe`, for
  ## locality) and trampoline its tasks on the calling thread, re-submitting any
  ## continuation that yields more work. Returns true if a batch ran. Shared by
  ## the worker loop and `poolHelp`.
  var buf {.noinit.}: array[BulkSize, Task]
  for attempt in 0 ..< StripeCount:
    let s = (startStripe + attempt) and (StripeCount - 1)
    let n = tryBulkDequeue(s, buf)
    if n > 0:
      for i in 0 ..< n:
        let c = buf[i].con
        let next = c.fn(c.env)
        if next.fn != nil:
          submit(next, s)
      return true
  result = false

proc poolHelp*(): bool {.inline.} =
  ## Run one batch of queued tasks on the calling thread. A thread blocked on a
  ## join (`parWait`) calls this instead of idle-spinning, so it *helps* drain
  ## the pool — without it, nested parallel regions deadlock once every worker
  ## is spin-waiting in a join with its sub-tasks unrun in the queue (fork-join
  ## work-donation). Returns true if any task ran.
  drainOnce(0)

# --- I/O registration ---

proc ioFd*(): cint {.inline.} = gIoFd
  ## The shared I/O poller file descriptor (epoll fd or kqueue fd).

proc registerFd*(fd: cint; slotIdx: int; events: uint32) =
  ## Register fd with the shared I/O instance.
  ## Oneshot semantics: exactly one worker handles each fired event.
  when hasEpoll:
    var mask = EPOLLONESHOT
    if (events and EvRead) != 0: mask = mask or EPOLLIN
    if (events and EvWrite) != 0: mask = mask or EPOLLOUT
    var ev = EpollEvent(events: mask)
    ev.data.p = cast[pointer](uint(slotIdx))
    discard epoll_ctl(gIoFd, EPOLL_CTL_ADD, fd, addr ev)
  elif hasKqueue:
    var kevs = default array[2, KEvent]
    var n = 0
    if (events and EvRead) != 0:
      kevs[n].ident = fd.csize_t
      kevs[n].filter = EVFILT_READ
      kevs[n].flags = EV_ADD or EV_ONESHOT
      kevs[n].udata = cast[pointer](uint(slotIdx))
      inc n
    if (events and EvWrite) != 0:
      kevs[n].ident = fd.csize_t
      kevs[n].filter = EVFILT_WRITE
      kevs[n].flags = EV_ADD or EV_ONESHOT
      kevs[n].udata = cast[pointer](uint(slotIdx))
      inc n
    if n > 0:
      discard kevent(gIoFd, addr kevs[0], n.cint, nil, 0, nil)

proc rearmFd*(fd: cint; slotIdx: int; events: uint32) =
  ## Re-arm a oneshot fd after it has fired.
  when hasEpoll:
    var mask = EPOLLONESHOT
    if (events and EvRead) != 0: mask = mask or EPOLLIN
    if (events and EvWrite) != 0: mask = mask or EPOLLOUT
    var ev = EpollEvent(events: mask)
    ev.data.p = cast[pointer](uint(slotIdx))
    discard epoll_ctl(gIoFd, EPOLL_CTL_MOD, fd, addr ev)
  elif hasKqueue:
    var kevs = default array[2, KEvent]
    var n = 0
    if (events and EvRead) != 0:
      kevs[n].ident = fd.csize_t
      kevs[n].filter = EVFILT_READ
      kevs[n].flags = EV_ADD or EV_ONESHOT or EV_ENABLE
      kevs[n].udata = cast[pointer](uint(slotIdx))
      inc n
    if (events and EvWrite) != 0:
      kevs[n].ident = fd.csize_t
      kevs[n].filter = EVFILT_WRITE
      kevs[n].flags = EV_ADD or EV_ONESHOT or EV_ENABLE
      kevs[n].udata = cast[pointer](uint(slotIdx))
      inc n
    if n > 0:
      discard kevent(gIoFd, addr kevs[0], n.cint, nil, 0, nil)

proc unregisterFd*(fd: cint) =
  when hasEpoll:
    discard epoll_ctl(gIoFd, EPOLL_CTL_DEL, fd, nil)
  elif hasKqueue:
    var kevs = default array[2, KEvent]
    kevs[0].ident = fd.csize_t
    kevs[0].filter = EVFILT_READ
    kevs[0].flags = EV_DELETE
    kevs[1].ident = fd.csize_t
    kevs[1].filter = EVFILT_WRITE
    kevs[1].flags = EV_DELETE
    discard kevent(gIoFd, addr kevs[0], 2, nil, 0, nil)

# --- io_uring helpers ---

when hasIouring:
  proc submitUrSqes*() =
    if gRingAvail and gPendingSqes > 0:
      gRingLock.acquire()
      try:
        let q = getQueuePtr()
        discard submit(q[], 0)
      except:
        discard
      gPendingSqes = 0
      gRingLock.release()

  proc drainUrCqes*() =
    if gRingAvail and gUrCqeCb != nil:
      gRingLock.acquire()
      var cqes: seq[Cqe]
      try:
        let q = getQueuePtr()
        cqes = q[].copyCqes(waitNr = 0'u)
      except:
        cqes = newSeq[Cqe]()
      gRingLock.release()
      for cqe in cqes:
        gUrCqeCb(cqe.userData, cqe.res)

# --- IoRequest queue ---

proc initIoReqQueue*() =
  initSysLock(ioReqQueue.L)
  initSysCond(ioReqQueue.notFull)

proc enqueueIoRequest*(req: IoRequest) =
  ## Caller thread. Blocks if the io_uring request queue is full.
  acquireSys(ioReqQueue.L)
  while ioReqQueue.count >= IoReqQueueSize:
    waitSysCond(ioReqQueue.notFull, ioReqQueue.L)
  ioReqQueue.data[ioReqQueue.tail] = req
  ioReqQueue.tail = (ioReqQueue.tail + 1) and (IoReqQueueSize - 1)
  inc ioReqQueue.count
  releaseSys(ioReqQueue.L)

proc drainIoRequests*(): bool =
  ## Worker thread. Dequeue io_uring requests and fill SQEs.
  ## For epoll/kqueue backends, calls armFdCallback instead.
  ## Returns true if any requests were processed.
  var buf {.noinit.}: array[32, IoRequest]
  var n = 0
  acquireSys(ioReqQueue.L)
  n = min(ioReqQueue.count, 32)
  for i in 0 ..< n:
    buf[i] = ioReqQueue.data[ioReqQueue.head]
    ioReqQueue.head = (ioReqQueue.head + 1) and (IoReqQueueSize - 1)
  dec ioReqQueue.count, n
  if ioReqQueue.count < IoReqQueueSize:
    signalSysCond(ioReqQueue.notFull)
  releaseSys(ioReqQueue.L)

  for i in 0 ..< n:
    let req = buf[i]
    when hasIouring:
      if gRingAvail:
        gRingLock.acquire()
        let q = getQueuePtr()
        let sqe = q[].getSqe()
        if sqe != nil:
          case req.kind
          of opIoRead:
            discard sqe.read(req.readFd, req.readBuf, req.readLen, 0)
            discard sqe.setUserData(req.readUserData)
          of opIoWrite:
            discard sqe.write(req.writeFd, req.writeBuf, req.writeLen, 0)
            discard sqe.setUserData(req.writeUserData)
          of opIoAccept:
            discard accept(sqe, req.acceptFd,
              cast[ptr SockAddr](req.acceptAddrBuf),
              cast[ptr SockLen](req.acceptAddrLen), 0)
            discard sqe.setUserData(req.acceptUserData)
          inc gPendingSqes
        gRingLock.release()
        continue
    # Fallback: arm fd for epoll/kqueue
    if armFdCallback != nil:
      let fd = case req.kind
        of opIoRead: req.readFd
        of opIoWrite: req.writeFd
        of opIoAccept: req.acceptFd
      armFdCallback(fd)

  result = n > 0

# --- I/O polling ---

proc poolPollIo*(timeoutMs: cint): bool =
  ## Poll the shared I/O instance once and dispatch every ready handler on the
  ## calling thread. `timeoutMs` is how long to block waiting for an event (`0`
  ## = non-blocking peek). Returns true if at least one handler fired. Safe to
  ## call from any thread (oneshot semantics: each event goes to exactly one
  ## caller) — used both by the worker loop and by `parWait` so a thread blocked
  ## on a join can still advance the I/O its parked chunks are waiting on.
  result = false
  when hasIouring:
    discard drainIoRequests()
    submitUrSqes()
    drainUrCqes()
  when hasEpoll:
    var ioEvents {.noinit.}: array[MaxIoEvents, EpollEvent]
    let n = epoll_wait(gIoFd, addr ioEvents[0], MaxIoEvents.cint, timeoutMs)
    for i in 0 ..< n:
      let slotIdx = int(cast[uint](ioEvents[i].data.p))
      if gPollCb != nil:
        gPollCb(slotIdx, ioEvents[i].events)
    result = n > 0
  elif hasKqueue:
    var ioEvents {.noinit.}: array[MaxIoEvents, KEvent]
    var ts = default Timespec
    if timeoutMs > 0:
      ts.tv_nsec = 1_000_000  # 1 ms (we only ever pass 0 or 1)
    let n = kevent(gIoFd, nil, 0, addr ioEvents[0], MaxIoEvents.cint, addr ts)
    for i in 0 ..< n:
      let slotIdx = int(cast[uint](ioEvents[i].udata))
      let evMask = case ioEvents[i].filter
        of EVFILT_READ: EvRead
        of EVFILT_WRITE: EvWrite
        else: 0u32
      if gPollCb != nil:
        gPollCb(slotIdx, evMask)
    result = n > 0
  else:
    if timeoutMs > 0:
      when defined(windows):
        sleep(timeoutMs.uint32)
      else:
        discard usleepMicroseconds(timeoutMs.uint32 * 1000'u32)

# --- Worker loop ---

proc workerLoop(arg: pointer) {.nimcall.} =
  let threadIdx = cast[int](arg)   # index passed by value via the pointer slot (see initPool)
  while not atomicLoad(stopFlag, moRelaxed):
    # 1. Bulk-drain tasks: own stripe first, then steal from others. Trampolines
    #    each continuation, re-submitting any that yield more work.
    let busy = drainOnce(threadIdx)
    # 2. Poll I/O — non-blocking when we just ran work, 1ms wait when idle.
    discard poolPollIo(if busy: 0.cint else: 1.cint)

# --- Lifecycle ---

proc initPool*() =
  ## Initialize the I/O poller and start worker threads.
  when hasEpoll:
    gIoFd = epoll_create1(0)
    assert gIoFd >= 0, "epoll_create1 failed"
  when hasIouring:
    initIoReqQueue()
    try:
      gRing = newQueue(4098)
      gRingAvail = true
    except:
      gRingAvail = false
  when hasKqueue:
    gIoFd = kqueue()
    assert gIoFd >= 0, "kqueue failed"
  for i in 0 ..< WorkerCount:
    try:
      # Pass the worker index BY VALUE through the pointer slot. It used to be
      # `addr indexes[i]` into a stack-local array that dangled the moment
      # initPool returned — run()'s sleep frame then reused that memory and the
      # workers read garbage thread indices (valgrind: uninitialised value in
      # tryBulkDequeue/workerLoop, origin the reused frame). No storage, no
      # lifetime, no race.
      create workers[i], workerLoop, cast[pointer](i)
    except:
      discard

proc poolStopped*(): bool {.inline.} =
  atomicLoad(stopFlag, moRelaxed)

proc shutdownPool*() =
  ## Signal all workers to stop and join threads.
  atomicStore(stopFlag, true, moRelaxed)
  for i in 0 ..< WorkerCount:
    workers[i].join()
  when hasIoPoll:
    proc close(fd: cint): cint {.importc, header: "<unistd.h>".}
    discard close(gIoFd)
