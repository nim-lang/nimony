# (c) 2025 Andreas Rumpf
# Lock-striped thread pool with continuation-based scheduling.
#
# Workers contend on independent stripes to reduce lock pressure.
# Each worker polls I/O every iteration; the timeout doubles as idle sleep.
#
# A Task wraps a Continuation plus metadata. The pool schedules Tasks;
# the worker trampolines the inner continuation.

import std / [atomics, rawthreads, assertions, ticketlocks, private/syslocks]

when not defined(windows):
  import std/posix/posix
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

  ReactorProc* = proc(timeoutMs: int): bool {.closure.}
    ## Polls one I/O backend once; returns true if it delivered anything.
    ## `std/ioring` registers one of these per `Ring` via `registerReactor`.

  Pool* = ref object of RootObj
    stripes: array[StripeCount, Stripe]
    workers: array[WorkerCount, RawThread]
    stopFlag: bool # accessed atomically
    reactors: seq[ReactorProc]
    reactorsLock: TicketLock

proc registerReactor*(p: Pool; r: ReactorProc) =
  ## Register an I/O backend to be polled by every worker's idle loop (and by
  ## `parWait`). Lets independent subsystems (e.g. several `std/ioring`
  ## `Ring`s) share one `Pool` — and its worker threads — instead of each
  ## spinning up its own, which previously meant every `Ring` (and `parfor`'s
  ## own pool) duplicated `WorkerCount` OS threads contending over the same
  ## physical cores. Safe to call concurrently; registration is expected to
  ## be rare (typically once per `Ring`), so a lock here costs nothing on the
  ## hot path (`poll`, below, only takes the lock to snapshot the list).
  withLock p.reactorsLock:
    p.reactors.add(r)

proc poll*(p: Pool; timeoutMs: int): bool =
  ## Poll every registered I/O backend once and dispatch ready handlers on
  ## the calling thread. `timeoutMs` bounds how long the *first* reactor may
  ## block waiting for an event (`0` = non-blocking peek); subsequent
  ## reactors in the same call are polled non-blockingly so that N
  ## registered backends don't add up to N*timeoutMs of latency. Returns
  ## true if any handler fired. Safe to call from any thread (oneshot
  ## semantics: each event goes to exactly one caller) — used both by the
  ## worker loop and by `parWait` so a thread blocked on a join can still
  ## advance the I/O its parked chunks are waiting on.
  result = false
  var snapshot: seq[ReactorProc]
  withLock p.reactorsLock:
    if p.reactors.len == 0: return false
    snapshot = p.reactors
  for i, r in snapshot:
    if r(if i == 0: timeoutMs else: 0):
      result = true

# --- Submit / dequeue ---

proc tryEnqueue(p: Pool; s: int; t: Task): bool {.inline.} =
  ## Push `t` onto stripe `s` if it has room; `false` when the stripe is full.
  let i = s and (StripeCount - 1)
  p.stripes[i].L.acquire()
  result = p.stripes[i].count < StripeSize
  if result:
    p.stripes[i].data[p.stripes[i].tail] = t
    p.stripes[i].tail = (p.stripes[i].tail + 1) and (StripeSize - 1)
    inc p.stripes[i].count
  p.stripes[i].L.release()

proc submit*(p: Pool; t: Task; hint = 0) =
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
  if p.tryEnqueue(h, t): return
  for off in 1 ..< StripeCount:
    if p.tryEnqueue(h + off, t): return
  # Saturated: run it here. `c.fn` returns the next continuation, or one whose
  # `fn` is nil when the task completes or parks (I/O will resume a parked one).
  var c = t.con
  while true:
    let next = c.fn(c.env)
    if next.fn == nil: break
    if p.tryEnqueue(h, toTask(next)): break
    c = next

proc submit*(p: Pool; c: Continuation; hint = 0) {.inline.} =
  ## Convenience: submit a bare continuation as a task.
  p.submit(toTask(c), hint)

proc tryBulkDequeue(p: Pool; stripe: int; buf: var array[BulkSize, Task]): int =
  let s = stripe and (StripeCount - 1)
  p.stripes[s].L.acquire()
  result = min(p.stripes[s].count, BulkSize)
  for i in 0 ..< result:
    buf[i] = p.stripes[s].data[p.stripes[s].head]
    p.stripes[s].head = (p.stripes[s].head + 1) and (StripeSize - 1)
  dec p.stripes[s].count, result
  p.stripes[s].L.release()

proc drainOnce(p: Pool; startStripe: int): bool =
  ## Dequeue the first non-empty stripe (searching from `startStripe`, for
  ## locality) and trampoline its tasks on the calling thread, re-submitting any
  ## continuation that yields more work. Returns true if a batch ran. Shared by
  ## the worker loop and `pool.help`.
  var buf {.noinit.}: array[BulkSize, Task]
  for attempt in 0 ..< StripeCount:
    let s = (startStripe + attempt) and (StripeCount - 1)
    let n = p.tryBulkDequeue(s, buf)
    if n > 0:
      for i in 0 ..< n:
        let c = buf[i].con
        let next = c.fn(c.env)
        if next.fn != nil:
          p.submit(next, s)
      return true
  result = false

proc help*(p: Pool): bool {.inline.} =
  ## Run one batch of queued tasks on the calling thread. A thread blocked on a
  ## join (`parWait`) calls this instead of idle-spinning, so it *helps* drain
  ## the pool — without it, nested parallel regions deadlock once every worker
  ## is spin-waiting in a join with its sub-tasks unrun in the queue (fork-join
  ## work-donation). Returns true if any task ran.
  p.drainOnce(0)

type TArg = tuple[p: Pool, threadIdx: int]

proc workerLoop(arg: pointer) {.nimcall.} =
  # index passed by value via the pointer slot
  let (p, threadIdx) = cast[ptr TArg](arg)[]
  dealloc(arg)
  while not atomicLoad(p.stopFlag, moRelaxed):
    # 1. Bulk-drain tasks: own stripe first, then steal from others. Trampolines
    #    each continuation, re-submitting any that yield more work.
    let busy = p.drainOnce(threadIdx)
    # 2. Poll I/O — non-blocking when we just ran work, 1ms wait when idle.
    let eventFired = p.poll(if busy: 0.cint else: 1.cint)
    if not eventFired and not busy:
      const timeoutMs = 1
      when defined(windows):
        sleep(timeoutMs.uint32)
      else:
        # `(timeoutMs.float32/1000).Time` truncates to whole seconds and
        # never sets `tv_nsec` — for `timeoutMs=1` that is `tv_sec=0,
        # tv_nsec=0`, so `nanosleep` returned immediately instead of
        # sleeping for ~1ms (busy-spinning the idle worker instead of
        # yielding the core). Use div/mod into whole seconds + nanoseconds.
        var ts = Timespec(tv_sec: Time(timeoutMs div 1000),
                           tv_nsec: clong((timeoutMs mod 1000) * 1_000_000))
        var rem = Timespec()
        discard nanosleep(ts, rem)

proc init*(p: Pool) =
  ## Initialize the I/O poller and start worker threads.
  for i in 0 ..< WorkerCount:
    try:
      # Pass the worker index BY VALUE through the pointer slot. It used to be
      # `addr indexes[i]` into a stack-local array that dangled the moment
      # initPool returned — run()'s sleep frame then reused that memory and the
      # workers read garbage thread indices (valgrind: uninitialised value in
      # tryBulkDequeue/workerLoop, origin the reused frame). No storage, no
      # lifetime, no race.
      var arg = cast[ptr TArg](alloc(sizeof TArg))
      arg[].p = p
      arg[].threadIdx = i
      create p.workers[i], workerLoop, cast[pointer](arg)
    except:
      discard

proc createPool*(): Pool =
  var pool = Pool()
  pool.init()
  return pool

proc stopped*(p: Pool): bool {.inline.} =
  atomicLoad(p.stopFlag, moRelaxed)

proc shutdown*(p: Pool) =
  ## Signal all workers to stop and join threads.
  atomicStore(p.stopFlag, true, moRelaxed)
  for i in 0 ..< WorkerCount:
    p.workers[i].join()

# --- Shared default instance -----------------------------------------------
#
# `Pool` is a first-class, instantiable type (`createPool()` makes an
# independent instance with its own worker threads) so a subsystem that
# needs its own thread/IO profile — e.g. `std/ioring`, which registers its
# epoll/kqueue/io_uring backend via `registerReactor` — can create one. But
# most callers (like `std/parfor`'s `||` loop) just want "the" shared
# pool and don't care to configure it, and every module reinventing its own
# CAS-guarded lazy-init global (as `std/parfor` used to) means N copies of
# the same bootstrap dance and no single source of truth for "is there
# already a shared pool". `defaultPool()` is that single source of truth:
# a lazily-created, process-wide singleton that any zero-config caller can
# share, while still leaving `createPool()` available for anyone who wants
# an independent instance instead.
var defaultPoolState: int
  ## 0 = uninitialised, 1 = initialising, 2 = ready.
var gDefaultPool: Pool

proc defaultPool*(): Pool =
  ## The lazily-initialised, process-wide default pool. Thread-safe and
  ## idempotent: concurrent first-callers race through a CAS, the loser
  ## spins until the winner finishes `createPool()`.
  if atomicLoad(defaultPoolState, moAcquire) == 2: return gDefaultPool
  var expected = 0
  if atomicCompareExchange(defaultPoolState, expected, 1):
    gDefaultPool = createPool()
    atomicStore(defaultPoolState, 2, moRelease)
  else:
    while atomicLoad(defaultPoolState, moAcquire) != 2:
      discard
  gDefaultPool
