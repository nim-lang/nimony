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
else:
  import windows/winlean
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
proc poolPollIo*(timeoutMs: int): bool {.nimcall.} =
  ## Polls one I/O backend once; returns true if it delivered anything.
  ## `std/ioring` registers one of these.
  result = false

var
  stripes: array[StripeCount, Stripe]
  workers: array[WorkerCount, RawThread]
  stopFlag: bool # accessed atomically
  gReactor*: proc(timeoutMs: int): bool {.nimcall.} = poolPollIo

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
  ## the worker loop and `pool.help`.
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
  drainOnce(0)

when defined(useMimalloc):
  proc miCollect(force: bool) {.importc: "mi_collect".}
    ## mimalloc heap collection for the CALLING thread. Continuation-based
    ## scheduling constantly allocates on one worker and frees on another;
    ## those cross-thread frees land on the allocating heap's remote list and
    ## are only reclaimed when its owner thread collects. Without a periodic
    ## collect, that backlog grows without bound (measured: a passive proc
    ## owning an 8MB seq across one park leaks ~the full seq per invocation;
    ## flat when alloc+free stay on one thread — see harness
    ## tests/leak_repro*.nim). An idle-time collect converges it to a small
    ## per-worker steady state.

proc workerLoop(arg: pointer) {.nimcall.} =
  let threadIdx = cast[int](arg)
  var idleTicks = 0
  var sinceCollect = 0
  while not atomicLoad(stopFlag, moRelaxed):
    # 1. Bulk-drain tasks: own stripe first, then steal from others. Trampolines
    #    each continuation, re-submitting any that yield more work.
    let busy = drainOnce(threadIdx)
    # 2. Poll I/O — non-blocking when we just ran work, 1ms wait when idle.
    let eventFired = gReactor(if busy: 0.cint else: 1.cint)
    if not eventFired and not busy:
      const timeoutMs = 1
      when defined(windows):
        sleep(timeoutMs.uint32)
      else:
        var ts = Timespec(tv_sec: Time(timeoutMs div 1000),
                           tv_nsec: clong((timeoutMs mod 1000) * 1_000_000))
        var rem = Timespec()
        discard nanosleep(ts, rem)
    # 3. Reclaim this worker's cross-thread-free backlog: a forced collect
    #    after a brief idle (~8ms of 1ms polls), plus a hard periodic fallback
    #    so a worker that never goes idle still collects. force=true is what
    #    actually drains the remote list; at this cadence its cost is noise.
    #    (mimalloc-only: the default nimNativeAlloc has its own cross-thread
    #    free path — measure before assuming it needs an equivalent.)
    when defined(useMimalloc):
      inc sinceCollect
      if busy: idleTicks = 0 else: inc idleTicks
      if idleTicks >= 8 or sinceCollect >= 8192:
        idleTicks = 0
        sinceCollect = 0
        miCollect(true)

# --- Lifecycle ---

var poolState: int

proc initPool*() =
  if atomicLoad(poolState, moAcquire) == 2: return
  var expected = 0
  if atomicCompareExchange(poolState, expected, 1):
    for i in 0 ..< WorkerCount:
      try:
        create workers[i], workerLoop, cast[pointer](i)
      except:
        discard
    atomicStore(poolState, 2, moRelease)
  else:
    while atomicLoad(poolState, moAcquire) != 2:
      discard

proc stopped*(): bool {.inline.} =
  atomicLoad(stopFlag, moRelaxed)

proc shutdownPool*() =
  atomicStore(stopFlag, true, moRelaxed)
  for i in 0 ..< WorkerCount:
    workers[i].join()
