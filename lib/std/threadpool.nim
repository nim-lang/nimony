# (c) 2025 Andreas Rumpf
# Lock-striped thread pool with continuation-based scheduling.
#
# Workers contend on independent stripes to reduce lock pressure.
# Each worker polls I/O every iteration; the timeout doubles as idle sleep.
#
# A Task wraps a Continuation plus metadata. The pool schedules Tasks;
# the worker trampolines the inner continuation.

# The pool is a synchronization primitive itself: its globals are guarded by
# its own stripe locks and atomics, which the shared-global check cannot see.
{.feature: "assumeSync".}

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

# --- Work-stealing deque ---

type
  TaskDeque = object
    ## A worker's own task queue: Chase-Lev work stealing, in batches.
    ##
    ## The owner pushes at `bottom` and never touches `top` without a CAS; a
    ## thief takes from `top` and only touches it with a CAS. `bottom` has one
    ## writer (the owner), so publishing an item is a plain store followed by a
    ## release store of `bottom`, and no lock is involved anywhere.
    ##
    ## Both ends take a *batch*, and the batch is what the CAS buys: advancing
    ## `top` to `top + n` in one compare-exchange is the instant the whole batch
    ## stops being visible to everybody else, so one atomic operation covers
    ## `BulkSize` tasks. The owner takes from the same end as the thieves rather
    ## than from `bottom`, which keeps exactly one rule — "whoever moves `top`
    ## CASes it" — instead of the owner/thief protocol a LIFO owner pop needs.
    top: int
      ## Oldest live item; moved only by `atomicCompareExchange`.
    bottom: int
      ## One past the newest item; written only by the owner.
    data: seq[Task]

proc init(d: var TaskDeque; capacity: int) =
  d.data = newSeq[Task](capacity)

proc tryPush(d: var TaskDeque; t: Task): bool =
  ## Owner only. `top` is read relaxed because it only ever grows: a stale
  ## value is a *lower* one, so the check can say "full" when there is room but
  ## never the other way round.
  let b = d.bottom
  if b - atomicLoad(d.top, moRelaxed) >= d.data.len: return false
  d.data[b and (d.data.len - 1)] = t
  atomicStore(d.bottom, b + 1, moRelease)
  result = true

proc tryPushBulk(d: var TaskDeque; items: openArray[Task]): int =
  ## Owner only: publish as many leading items of `items` (in order) as fit.
  let b = d.bottom
  let t = atomicLoad(d.top, moRelaxed)
  result = min(items.len, d.data.len - (b - t))
  if result <= 0: return 0
  for i in 0 ..< result:
    d.data[(b + i) and (d.data.len - 1)] = items[i]
  atomicStore(d.bottom, b + result, moRelease)

proc tryBulkPop(d: var TaskDeque; bulkSize: int; buf: var openArray[Task]): int =
  ## Take up to `bulkSize` of the oldest tasks. Anybody may call it — the owner
  ## draining its own queue and a thief draining somebody else's are the same
  ## operation. A lost CAS means somebody else took that batch, so it retries a
  ## few times before reporting empty; the caller treats that as "no work here"
  ## and moves on.
  for attempt in 0 ..< 4:
    let t = atomicLoad(d.top, moRelaxed)
      ## Only used to size the batch: a stale value makes the CAS below fail
      ## and retry, never over-claim.
    let b = atomicLoad(d.bottom, moAcquire)
      ## This one is the item count, and reading the items depends on it.
    let avail = b - t
    if avail <= 0: return 0
    let n = min(min(avail, bulkSize), buf.len)
    var expected = t
    if atomicCompareExchange(d.top, expected, t + n, moAcquireRelease, moAcquire):
      for i in 0 ..< n:
        buf[i] = d.data[(t + i) and (d.data.len - 1)]
      return n
  result = 0

type
  TaskQueue = object
    ## One stripe: the owner's lock-free deque, plus a locked FIFO for pushes
    ## from threads that are not the owner. Those are rare — a foreign thread
    ## submitting a task, and `flushStaged` spilling into a stripe that is not
    ## the caller's own — and they must not touch `bottom`, which only the owner
    ## may write.
    own: TaskDeque
    shared: FifoStripe[Task]

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
  localQueues: seq[TaskQueue]
  injectQueue: FifoStripe[Task]
  workers: seq[RawThread]
  stopFlag: bool # accessed atomically
  workerCount* = 0
  gReactor*: proc(timeoutMs: int): bool {.nimcall.} = poolPollIo
var gReactorState*: int = 0
  ## 0 until `gReactor`/`gReactorWaits` — and everything they reach — are fully
  ## set up. It has to be PUBLISHED rather than merely assigned: `initPool`
  ## starts the workers, and only then does `ioring.setupRing` allocate the
  ## per-lane arenas the reactor indexes and finally point `gReactor` at itself.
  ## Nothing orders those plain stores against each other, so on a weakly
  ## ordered machine a worker could see the new reactor and an arena seq that is
  ## still empty. `reactorReady` pairs an acquire with `setupRing`'s release.
var gReactorWaits* = false
  ## Does `gReactor(ms)` actually sleep for up to `ms`? Set from the I/O
  ## backend (see `BackendRelays.waits`). While it is false an idle worker has
  ## to do the sleeping itself, because a reactor that only peeks would
  ## otherwise leave the loop spinning on a core.
var threadIdx* {.threadvar.}: int

template countMetric(counter: untyped; n = 1) =
  ## Bump one of this thread's counters — only when the build asked for them.
  ##
  ## The counters sit on the pool's hottest lines: `submit` alone would bump one
  ## per task, and every dequeue attempt bumps another. Nothing in the library
  ## reads them (they are for a profile), so a normal build should not pay for
  ## them at all. `-d:threadpoolMetrics` turns them on and sizes
  ## `workerMetrics`; without it the table stays empty and the argument — an
  ## index into it — is not even looked at.
  when defined(threadpoolMetrics):
    inc counter, uint(n)

# --- Submit / dequeue ---

proc tryEnqueue(s: int; t: Task): bool {.inline.} =
  ## Push `t` onto stripe `s` if it has room; `false` when the stripe is full.
  ## The owner pushes into its own deque; anybody else has to take the locked
  ## FIFO, because `bottom` has exactly one writer.
  let i = s mod workerCount
  if isPoolWorker() and i == threadIdx:
    return localQueues[i].own.tryPush(t)
  return localQueues[i].shared.tryEnqueue(t)

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
  countMetric(workerMetrics[threadIdx].tasksSubmited)
  if tryEnqueue(h, t): return
  for off in 1 ..< workerCount:
    countMetric(workerMetrics[threadIdx].enqueueMiss)
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
      let moved =
        if isPoolWorker() and s == threadIdx:
          localQueues[s].own.tryPushBulk(toOpenArray(chunk, n - remaining, n - 1))
        else:
          localQueues[s].shared.tryBulkEnqueue(
            toOpenArray(chunk, n - remaining, n - 1))
      dec remaining, moved
      if moved == 0:
        countMetric(workerMetrics[threadIdx].enqueueMiss)
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
    result = localQueues[s].own.tryBulkPop(BulkSize, buf)
    if result == 0:
      result = localQueues[s].shared.tryBulkDequeue(BulkSize, buf)

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
      countMetric(workerMetrics[threadIdx].tasksHandled, n)
      if attempt != 0:
        countMetric(workerMetrics[threadIdx].tasksStealed, n)
      return true
    else:
      countMetric(workerMetrics[threadIdx].dequeueMiss)
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

proc reactorReady*(): bool {.inline.} =
  ## Has the I/O ring published its reactor? See `gReactorState`.
  atomicLoad(gReactorState, moAcquire) != 0

proc reactorPoll*(timeoutMs: int): bool {.inline.} =
  ## Drive the I/O reactor if there is one, and the do-nothing default until
  ## there is. Every caller of `gReactor` goes through this.
  if reactorReady(): gReactor(timeoutMs) else: poolPollIo(timeoutMs)

var idleSleepUs {.threadvar.}: int

proc workCycle(stripe: int): bool =
  ## One turn of a worker: run the queued tasks, drive this thread's I/O lane,
  ## and nap when neither had anything. Answers whether tasks ran.
  if idleSleepUs == 0: idleSleepUs = 1
  # 1. Bulk-drain tasks: own stripe first, then steal from others. Trampolines
  #    each continuation, re-submitting any that yield more work.
  result = drainOnce(stripe)
  # 2. Poll I/O — non-blocking when we just ran work, 1ms wait when idle.
  let eventFired = reactorPoll(if result: 0.cint else: 1.cint)
  if not eventFired and not result and not gReactorWaits:
    # Only when the reactor did not wait for us. Behind one that did, this
    # nap is the difference between noticing a completion when it arrives
    # and noticing it up to a millisecond later.
    #
    # Back the nap off from a microsecond to a millisecond. A fork-join
    # producer submits the next batch microseconds after draining the previous
    # one; a flat 1ms sleep makes the worker miss that batch and the merge pays
    # the whole millisecond. The doubling means a genuinely idle pool still
    # reaches the 1ms nap after a handful of short waits.
    when defined(windows):
      sleep(max(1, idleSleepUs div 1000).uint32)
    else:
      var ts = Timespec(tv_sec: Time(idleSleepUs div 1_000_000),
                         tv_nsec: clong((idleSleepUs mod 1_000_000) * 1_000))
      var rem = Timespec()
      discard nanosleep(ts, rem)
    idleSleepUs = min(idleSleepUs * 2, 1000)
  else:
    idleSleepUs = 1

proc passiveWait() {.nimcall.} =
  ## `system.setPassiveWaitHook`: a thread waiting for a parked `.passive` call
  ## to finish does a worker's turn. It must: a non-worker's I/O ops sit on a
  ## lane that only its own poll drains (`ioLane`), so the op its callee parked
  ## on completes nowhere else — and a worker that waited idly could be the one
  ## the pool needed to resume the callee.
  discard workCycle(if isWorker: threadIdx else: 0)

proc workerLoop(arg: pointer) {.nimcall.} =
  threadIdx = cast[int](arg)
  isWorker = true
  var idleTicks = 0
  dequeTicks = 0
  var sinceCollect = 0
  while not atomicLoad(stopFlag, moRelaxed):
    let busy = workCycle(threadIdx)
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

proc initPool*(count = 0) =
  ## Start the worker pool: `count` threads, or one fewer than
  ## `countProcessors()` when `count` is 0. Idempotent: once a pool is up,
  ## later calls return at once and their `count` is ignored. Workers are not
  ## pinned to CPUs; the scheduler places them within the process's affinity.
  # Only `poolState` gates this; `workerCount` must NOT, because the CAS winner
  # publishes it *before* allocating `workerMetrics`/`localQueues`/`injectQueue`.
  # A second caller that returned on `workerCount > 0` would race ahead and index
  # those still-empty seqs on its next `submit`.
  if atomicLoad(poolState, moAcquire) == 2: return
  var expected = 0
  if atomicCompareExchange(poolState, expected, 1):
    workerCount = if count > 0: count else: max(1, cpuinfo.countProcessors() - 1)
    setPassiveWaitHook passiveWait
    when defined(threadpoolMetrics):
      workerMetrics = newSeq[WorkerMetrics](workerCount)
    localQueues = newSeq[TaskQueue](workerCount)
    for i in 0..<workerCount:
      localQueues[i].own.init(StripeSize)
      localQueues[i].shared.init(StripeSize)
    injectQueue.init(StripeSize*16)
    workers.setLen(workerCount)
    for i in 0 ..< workerCount:
      try:
        create workers[i], workerLoop, cast[pointer](i)
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
