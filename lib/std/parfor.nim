# (c) 2026 Andreas Rumpf
## Structured data parallelism: `for i in a || b: x[i] = f(input[i])`.
##
## The `||` iterator is a for-loop *plugin*: at sem time the plugin rewrites
## the loop into a set of chunk runners submitted to the shared
## `std/threadpool` worker pool plus a structured join at the loop's closing.
## The join *is* the loop end -- there is no `spawn`, no `Flowvar`, no handle,
## and (by construction of the static race rules) no data race.
##
## A chunk runner is a `.passive` coroutine, so an iteration that performs I/O
## *parks* its worker instead of pinning it -- the parallel `for` therefore
## covers I/O-bound maps, not just CPU-bound ones.
##
## This module provides the runtime the lowering targets; see `deps/parfor.nim`
## for the plugin that emits calls into it.

{.feature: "lenientnils".}

import std / [atomics, threadpool]

const
  ParDefaultChunks* = WorkerCount
    ## Default number of chunks when the `||` loop does not pass an explicit
    ## `chunkSize`. One chunk per worker keeps scheduling overhead low while
    ## saturating the pool.

  ParMaxChunks* = StripeCount * StripeSize
    ## Soft ceiling on concurrent chunk runners, = the pool queue capacity.
    ## `submit` is non-lossy (it caller-runs once the queue is full), so going
    ## over this no longer deadlocks — but the excess chunks would then run
    ## inline on the submitting thread, serialising them. Capping the chunk
    ## count here keeps every chunk genuinely pool-scheduled; `parGrain`
    ## coarsens a too-fine grain to stay within it.

type
  ParJoin* = object
    ## Completion barrier for a single parallel-for loop. `remaining` counts
    ## chunk runners that have not yet finished; each runner decrements it on
    ## completion and the joining thread waits for it to reach zero.
    remaining*: int   ## accessed atomically

# --- one-time pool bootstrap ----------------------------------------------

var poolState: int
  ## 0 = uninitialised, 1 = initialising, 2 = ready. Accessed atomically so the
  ## first parallel-for from any thread starts the pool exactly once.

proc ensureParPool*() =
  ## Start the worker pool the first time a parallel `for` runs. Idempotent and
  ## thread-safe: concurrent first-callers race through a CAS, the loser spins
  ## until the winner has finished `initPool`.
  if atomicLoad(poolState, moAcquire) == 2: return
  var expected = 0
  if atomicCompareExchange(poolState, expected, 1):
    initPool()
    atomicStore(poolState, 2, moRelease)
  else:
    while atomicLoad(poolState, moAcquire) != 2:
      discard

# --- range chunking --------------------------------------------------------

proc parIterCount*(a, b, step: int): int =
  ## Number of iterations of the inclusive strided range `a, a+step, …, ≤ b`
  ## (Nim's `||` yields `a, a+step, …`). Zero for an empty range or a
  ## non-positive `step`.
  if step <= 0 or b < a: return 0
  result = (b - a) div step + 1

proc parGrain*(iters, chunkSize: int): int =
  ## Iterations per chunk. A positive `chunkSize` is honoured (the programmer
  ## tunes it to the body's cost-per-iteration), but coarsened up if it would
  ## need more than `ParMaxChunks` runners — so a tiny grain over a huge range
  ## stays pool-scheduled instead of spilling onto the submitting thread. `0`
  ## derives a grain that yields about `ParDefaultChunks` chunks (one per
  ## worker), adapting to the machine. Always `>= 1` for a non-empty range, so a
  ## chunk is never empty.
  if iters <= 0: return 0
  # Smallest grain that keeps `ceil(iters/grain) <= ParMaxChunks`.
  let minGrain = (iters + ParMaxChunks - 1) div ParMaxChunks   # ceil
  if chunkSize > 0:
    result = max(chunkSize, minGrain)
  else:
    result = (iters + ParDefaultChunks - 1) div ParDefaultChunks   # ceil

proc parChunkCount*(iters, grain: int): int =
  ## Number of `grain`-sized chunks needed to cover `iters` iterations,
  ## `ceil(iters / grain)`.
  if iters <= 0 or grain <= 0: return 0
  result = (iters + grain - 1) div grain   # ceil

proc parChunkLo*(grain, k: int): int =
  ## First iteration index (inclusive) of chunk `k`: `k * grain`. Chunks are
  ## fixed-size half-open `[lo, hi)` ranges over the *iteration-index* space; the
  ## chunk runner maps each index `j` back to the value `a + j*step`.
  k * grain

proc parChunkHi*(iters, grain, k: int): int =
  ## One past the last iteration index of chunk `k`: `min((k+1)*grain, iters)`
  ## (the final chunk is short when `grain` does not divide `iters`).
  let hi = (k + 1) * grain
  result = if hi < iters: hi else: iters

# --- join lifecycle --------------------------------------------------------

proc parBegin*(j: var ParJoin; chunks: int) =
  ## Arm the join for `chunks` outstanding runners. Call before submitting.
  atomicStore(j.remaining, chunks)

proc parChunkDone*(j: ptr ParJoin) =
  ## Signal that one chunk runner has finished. Called at the tail of every
  ## chunk runner the plugin emits.
  discard atomicFetchSub(j.remaining, 1, moAcquireRelease)

proc parWait*(j: var ParJoin) =
  ## Block until every chunk runner has finished. While waiting the thread
  ## *helps* run pool tasks (`poolHelp`) rather than idle-spinning, so a chunk
  ## body that itself opens a parallel `||` (recursive fork-join, e.g. parallel
  ## fib) keeps making progress instead of every worker deadlocking in a join
  ## with its sub-tasks stuck unrun in the queue. When nothing is queued our
  ## sub-tasks are already in flight, so we just spin for their completion. (A
  ## passive, I/O-parking join that frees the joining worker entirely is still a
  ## planned enhancement.)
  while atomicLoad(j.remaining, moAcquire) > 0:
    if not poolHelp():
      discard

proc parSubmit*(c: Continuation; hint = 0) {.inline.} =
  ## Hand a chunk runner's continuation to the worker pool, spreading chunks
  ## across stripes by index (`hint`, the chunk number) so a many-chunk loop
  ## does not pile into one stripe. `threadpool.submit` is non-lossy (it
  ## caller-runs on a full queue), so this no longer guards against dropped
  ## runners — but the spread still avoids needlessly caller-running chunks on
  ## the submitting thread and balances load. Re-exported so the `||` plugin
  ## only needs symbols visible through `import std/parfor`.
  submit(c, hint)

iterator `||`*(a, b: int; step: Positive = 1; chunkSize = 0): int {.plugin: "deps/parfor".}
  ## Parallel range `for` loop. `for i in a || b: x[i] = f(input[i])` runs the
  ## body for every `i` in the *inclusive* range `a .. b` across the worker pool,
  ## joining at the loop's closing — matching Nim's standard `||`, which yields
  ## `a, a+step, …, ≤ b`. `step` is the iteration stride (default 1).
  ##
  ## `chunkSize` is the grain: how many iterations each parallel runner handles.
  ## Tune it to the body's cost-per-iteration (a property you know) rather than
  ## the worker count (which varies between machines); the number of runners
  ## falls out as `ceil(iters / chunkSize)`. `0` (the default) derives a grain
  ## giving about one chunk per worker. Pass it by name to skip `step`:
  ## `for i in `||`(a, b, chunkSize = 64): …`.
  ##
  ## The body must write only `x[i]`-style outputs at the iteration index and
  ## must not read those outputs back; under that contract iterations are
  ## data-race free by construction. The plugin in `deps/parfor.nim` performs
  ## the rewrite.
