# (c) 2025 Andreas Rumpf
# Lock-striped thread pool with continuation-based scheduling.
#
# Workers contend on independent stripes to reduce lock pressure.
# Each worker polls I/O every iteration; the timeout doubles as idle sleep.
#
# A Task wraps a Continuation plus metadata. The pool schedules Tasks;
# the worker trampolines the inner continuation.

import std / [atomics, rawthreads, assertions, ticketlocks, private/syslocks, cpuinfo]

when not defined(windows):
  import std/posix/posix
else:
  import windows/winlean
# --- Configuration ---

import std/stripes

const
  StripeSize*  = 2048  ## Tasks per stripe; must be a power of 2.
  BulkSize*    = 32   ## Max tasks drained per bulk dequeue.
  StageSize*   = 256  ## Tasks held in a worker's private staging ring; must be a power of 2.

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

proc poolPollIo*(timeoutMs: int): bool {.nimcall.} =
  ## Polls one I/O backend once; returns true if it delivered anything.
  ## `std/ioring` registers one of these.
  result = false

type
  WorkerMetrics* {.align: 64.} = object
    enqueueMiss*: uint
    dequeueMiss*: uint
    tasksSubmited*: uint
    tasksHandled*: uint
    tasksStealed*: uint

var
  workerMetrics*: seq[WorkerMetrics]
  localQueues: seq[FifoStripe[Task]]
  injectQueue: FifoStripe[Task]
  workers: seq[RawThread]
  stopFlag: bool # accessed atomically
  workerCount* = 0
  gReactor*: proc(timeoutMs: int): bool {.nimcall.} = poolPollIo
var gReactorWaits* = false
  ## Does `gReactor(ms)` actually sleep for up to `ms`? Set from the I/O
  ## backend (see `BackendRelays.waits`). While it is false an idle worker has
  ## to do the sleeping itself, because a reactor that only peeks would
  ## otherwise leave the loop spinning on a core.
var threadIdx* {.threadvar.}: int

# --- Submit / dequeue ---

proc tryEnqueue(s: int; t: Task): bool {.inline.} =
  ## Push `t` onto stripe `s` if it has room; `false` when the stripe is full.
  let i = s mod workerCount
  return localQueues[i].tryEnqueue(t)

proc submit*(t: Task; h = 0) =
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
  workerMetrics[threadIdx].tasksSubmited += 1
  if tryEnqueue(h, t): return
  for off in 1 ..< workerCount:
    workerMetrics[threadIdx].enqueueMiss += 1
    if tryEnqueue(h + off, t): return
  if injectQueue.tryEnqueue(t): return
  # Saturated: run it here. `c.fn` returns the next continuation, or one whose
  # `fn` is nil when the task completes or parks (I/O will resume a parked one).
  var c = t.con
  while true:
    let next = c.fn(c.env)
    if next.fn == nil: break
    if tryEnqueue(h, toTask(next)): break
    c = next

# --- Per-worker staging ring ---

type
  StagedTasks = object
    ## A worker's private submission ring: tasks submitted with the default
    ## hint land here (no locks; the ring is only touched by its owning
    ## thread) and are moved into the shared stripes in bulk at the top of
    ## the next `workerLoop` cycle. Turns bursts of resubmissions from inside
    ## the trampoline into one lock acquisition per `BulkSize` chunk.
    buf: array[StageSize, Task]
    head: int  # oldest element
    count: int

var
  staged {.threadvar.}: StagedTasks
  isWorker {.threadvar.}: bool # only workers may stage; foreign threads must enqueue directly or their batch could never be flushed

proc stageTask(t: Task): bool {.inline.} =
  ## Push onto this worker's private ring; false when it is full.
  if staged.count == StageSize: return false
  staged.buf[(staged.head + staged.count) and (StageSize - 1)] = t
  inc staged.count
  result = true

proc flushStaged(hint: int): bool =
  ## Bulk-enqueue staged tasks into the shared stripes in FIFO order, hinted
  ## stripe first, one lock acquisition per chunk of up to `BulkSize` tasks.
  ## Whatever the stripes cannot absorb goes through `submit`'s caller-runs
  ## path so the batch stays loss-free. Returns true if any task was moved.
  result = false
  var chunk {.noinit.}: array[BulkSize, Task]
  while staged.count > 0:
    result = true
    let n = min(staged.count, BulkSize)
    for i in 0 ..< n:
      chunk[i] = staged.buf[staged.head]
      staged.head = (staged.head + 1) and (StageSize - 1)
    dec staged.count, n
    var remaining = n
    var off = 0
    while remaining > 0 and off < workerCount:
      let s = (hint + off) mod workerCount
      let moved = localQueues[s].tryBulkEnqueue(
        toOpenArray(chunk, n - remaining, n - 1))
      dec remaining, moved
      if moved == 0:
        workerMetrics[threadIdx].enqueueMiss += 1
      inc off
    while remaining > 0:
      let t = chunk[n - remaining]
      dec remaining
      submit(t, hint)

proc submit*(c: Continuation; hint = -1) {.inline.} =
  ## Convenience: submit a bare continuation as a task. On a worker with the
  ## default hint, the task is first staged on the private lock-free ring and
  ## handed to the shared stripes in bulk on the next worker cycle (see
  ## `flushStaged`); everything else enqueues immediately.
  if hint != -1 or not isWorker:
    var hint = hint
    if hint == -1:
      hint = threadIdx
    submit(toTask(c), hint)
  elif not stageTask(toTask(c)):
    # Ring full: drain it into the stripes, then stage again — after a full
    # flush there is always room.
    discard flushStaged(threadIdx)
    discard stageTask(toTask(c))

var dequeTicks {.threadvar.}: int

proc tryBulkDequeue(stripe: int; buf: var array[BulkSize, Task]): int =
  result = 0
  let s = stripe mod workerCount
  inc dequeTicks
  if dequeTicks mod 61 == 0:
    result = injectQueue.tryBulkDequeue(BulkSize, buf)
  if result == 0:
    result = localQueues[s].tryBulkDequeue(BulkSize, buf)

proc drainOnce(startStripe: int): bool =
  ## Dequeue the first non-empty stripe (searching from `startStripe`, for
  ## locality) and trampoline its tasks on the calling thread, re-submitting any
  ## continuation that yields more work. Returns true if a batch ran. Shared by
  ## the worker loop and `pool.help`.
  var buf {.noinit.}: array[BulkSize, Task]
  # 0. Publish staged tasks first so submissions made since the last cycle
  #    become visible to the scan below.
  discard flushStaged(startStripe)
  for attempt in 0 ..< workerCount:
    let n = tryBulkDequeue(startStripe + attempt, buf)
    if n > 0:
      for i in 0 ..< n:
        let c = buf[i].con
        let next = c.fn(c.env)
        if next.fn != nil:
          submit(next, startStripe)
      workerMetrics[threadIdx].tasksHandled += n.uint
      if attempt != 0:
        workerMetrics[threadIdx].tasksStealed += n.uint
      return true
    else:
      workerMetrics[threadIdx].dequeueMiss += 1
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
  threadIdx = cast[int](arg)
  isWorker = true
  var idleTicks = 0
  dequeTicks = 0
  var sinceCollect = 0
  while not atomicLoad(stopFlag, moRelaxed):
    # 1. Bulk-drain tasks: own stripe first, then steal from others. Trampolines
    #    each continuation, re-submitting any that yield more work.
    let busy = drainOnce(threadIdx)
    # 2. Poll I/O — non-blocking when we just ran work, 1ms wait when idle.
    let eventFired = gReactor(if busy: 0.cint else: 1.cint)
    if not eventFired and not busy and not gReactorWaits:
      # Only when the reactor did not wait for us. Behind one that did, this
      # nap is the difference between noticing a completion when it arrives
      # and noticing it up to a millisecond later.
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
  # Only `poolState` gates this; `workerCount` must NOT, because the CAS winner
  # publishes it *before* allocating `workerMetrics`/`localQueues`/`injectQueue`.
  # A second caller that returned on `workerCount > 0` would race ahead and index
  # those still-empty seqs on its next `submit`.
  if atomicLoad(poolState, moAcquire) == 2: return
  var expected = 0
  if atomicCompareExchange(poolState, expected, 1):
    workerCount = max(1, cpuinfo.countProcessors() - 1)
    workerMetrics = newSeq[WorkerMetrics](workerCount)
    localQueues = newSeq[FifoStripe[Task]](workerCount)
    for i in 0..<workerCount:
      localQueues[i].init(StripeSize)
    injectQueue.init(StripeSize*16)
    workers.setLen(workerCount)
    for i in 0 ..< workerCount:
      try:
        # 4th parameter is `stackSize` (0 = OS default); the affinity request
        # is the 5th one.
        create workers[i], workerLoop, cast[pointer](i), 0, i
      except:
        discard
    atomicStore(poolState, 2, moRelease)
  else:
    while atomicLoad(poolState, moAcquire) != 2:
      discard

proc isPoolWorker*(): bool {.inline.} =
  ## True on a thread created by `initPool`. `threadIdx` is only meaningful as
  ## an identity on those: it is a threadvar defaulting to 0, so every foreign
  ## thread (the main thread included) reports 0 — the same value worker 0 uses.
  ## Anything that indexes per-thread state by `threadIdx` must ask this first.
  isWorker

proc stopped*(): bool {.inline.} =
  atomicLoad(stopFlag, moRelaxed)

proc shutdownPool*() =
  atomicStore(stopFlag, true, moRelaxed)
  for i in 0 ..< workerCount:
    workers[i].join()
