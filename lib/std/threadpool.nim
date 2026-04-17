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

import std / [atomics, rawthreads, assertions, ticketlocks]

when defined(linux):
  const hasEpoll = true
  const hasKqueue = false
elif defined(macosx) or defined(freebsd) or defined(netbsd) or
     defined(openbsd) or defined(dragonfly):
  const hasEpoll = false
  const hasKqueue = true
else:
  const hasEpoll = false
  const hasKqueue = false

const hasIoPoll* = hasEpoll or hasKqueue

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

var
  stripes: array[StripeCount, Stripe]
  workers {.noinit.}: array[WorkerCount, RawThread]
  gIoFd: cint
  stopFlag: bool  # accessed atomically

# --- Submit / dequeue ---

proc submit*(t: Task; hint = 0) =
  ## Submit a task to the pool. If the stripe is full the task is
  ## dropped -- callers should track this externally if needed.
  let s = hint and (StripeCount - 1)
  stripes[s].L.acquire()
  if stripes[s].count < StripeSize:
    stripes[s].data[stripes[s].tail] = t
    stripes[s].tail = (stripes[s].tail + 1) and (StripeSize - 1)
    inc stripes[s].count
  stripes[s].L.release()

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

# --- I/O registration ---

proc ioFd*(): cint {.inline.} = gIoFd
  ## The shared I/O poller file descriptor (epoll fd or kqueue fd).

proc registerFd*(fd: cint; handler: ptr IoHandler; events: uint32) =
  ## Register fd with the shared I/O instance.
  ## Oneshot semantics: exactly one worker handles each fired event.
  when hasEpoll:
    var mask = EPOLLONESHOT
    if (events and EvRead) != 0: mask = mask or EPOLLIN
    if (events and EvWrite) != 0: mask = mask or EPOLLOUT
    var ev = EpollEvent(events: mask)
    ev.data.p = handler
    discard epoll_ctl(gIoFd, EPOLL_CTL_ADD, fd, addr ev)
  elif hasKqueue:
    var kevs = default array[2, KEvent]
    var n = 0
    if (events and EvRead) != 0:
      kevs[n].ident = fd.csize_t
      kevs[n].filter = EVFILT_READ
      kevs[n].flags = EV_ADD or EV_ONESHOT
      kevs[n].udata = handler
      inc n
    if (events and EvWrite) != 0:
      kevs[n].ident = fd.csize_t
      kevs[n].filter = EVFILT_WRITE
      kevs[n].flags = EV_ADD or EV_ONESHOT
      kevs[n].udata = handler
      inc n
    if n > 0:
      discard kevent(gIoFd, addr kevs[0], n.cint, nil, 0, nil)

proc rearmFd*(fd: cint; handler: ptr IoHandler; events: uint32) =
  ## Re-arm a oneshot fd after it has fired.
  when hasEpoll:
    var mask = EPOLLONESHOT
    if (events and EvRead) != 0: mask = mask or EPOLLIN
    if (events and EvWrite) != 0: mask = mask or EPOLLOUT
    var ev = EpollEvent(events: mask)
    ev.data.p = handler
    discard epoll_ctl(gIoFd, EPOLL_CTL_MOD, fd, addr ev)
  elif hasKqueue:
    var kevs = default array[2, KEvent]
    var n = 0
    if (events and EvRead) != 0:
      kevs[n].ident = fd.csize_t
      kevs[n].filter = EVFILT_READ
      kevs[n].flags = EV_ADD or EV_ONESHOT or EV_ENABLE
      kevs[n].udata = handler
      inc n
    if (events and EvWrite) != 0:
      kevs[n].ident = fd.csize_t
      kevs[n].filter = EVFILT_WRITE
      kevs[n].flags = EV_ADD or EV_ONESHOT or EV_ENABLE
      kevs[n].udata = handler
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

# --- Worker loop ---

proc workerLoop(arg: pointer) {.nimcall.} =
  let threadIdx = cast[ptr int](arg)[]
  var taskBuf {.noinit.}: array[BulkSize, Task]
  when hasEpoll:
    var ioEvents {.noinit.}: array[MaxIoEvents, EpollEvent]
  elif hasKqueue:
    var ioEvents {.noinit.}: array[MaxIoEvents, KEvent]
  while not atomicLoad(stopFlag, moRelaxed):
    # 1. Bulk-drain tasks: own stripe first, then steal from others.
    var busy = false
    for attempt in 0 ..< StripeCount:
      let s = (threadIdx + attempt) and (StripeCount - 1)
      let n = tryBulkDequeue(s, taskBuf)
      if n > 0:
        for i in 0 ..< n:
          # Trampoline: run the continuation. If it returns a non-nil
          # continuation (i.e. the passive proc yielded more work),
          # re-submit it.
          let c = taskBuf[i].con
          let next = c.fn(c.env)
          if next.fn != nil:
            submit(next, threadIdx)
        busy = true
        break

    # 2. Poll I/O.
    #    Non-blocking when busy; 1ms wait when idle.
    when hasEpoll:
      let timeout: cint = if busy: 0 else: 1
      let n = epoll_wait(gIoFd, addr ioEvents[0], MaxIoEvents.cint, timeout)
      for i in 0 ..< n:
        let h = cast[ptr IoHandler](ioEvents[i].data.p)
        if h != nil:
          h.cb(h, ioEvents[i].events)
    elif hasKqueue:
      var ts = default Timespec
      if not busy:
        ts.tv_nsec = 1_000_000  # 1 ms
      let n = kevent(gIoFd, nil, 0, addr ioEvents[0], MaxIoEvents.cint, addr ts)
      for i in 0 ..< n:
        let h = cast[ptr IoHandler](ioEvents[i].udata)
        if h != nil:
          let evMask = case ioEvents[i].filter
            of EVFILT_READ: EvRead
            of EVFILT_WRITE: EvWrite
            else: 0u32
          h.cb(h, evMask)
    else:
      if not busy:
        sleep(1) # 1 ms idle sleep fallback

# --- Lifecycle ---

proc initPool*() =
  ## Initialize the I/O poller and start worker threads.
  when hasEpoll:
    gIoFd = epoll_create1(0)
    assert gIoFd >= 0, "epoll_create1 failed"
  elif hasKqueue:
    gIoFd = kqueue()
    assert gIoFd >= 0, "kqueue failed"
  var indexes = default array[WorkerCount, int]
  for i in 0 ..< WorkerCount:
    indexes[i] = i
    try:
      create workers[i], workerLoop, addr indexes[i]
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
