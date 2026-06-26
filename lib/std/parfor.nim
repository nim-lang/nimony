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
    ## count (`a || b` rather than `a || b || n`). One chunk per worker keeps
    ## scheduling overhead low while saturating the pool.

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

proc parChunkCount*(a, b, hint: int): int =
  ## Number of chunks to split `[a, b)` into. `hint <= 0` selects
  ## `ParDefaultChunks`. Never produces empty chunks: at most one element per
  ## chunk, so a 3-element range never spawns 8 runners.
  let n = b - a
  if n <= 0: return 0
  let w = if hint <= 0: ParDefaultChunks else: hint
  result = if w < n: w else: n

proc parChunkLo*(a, b, chunks, k: int): int =
  ## Lower bound (inclusive) of chunk `k` of `chunks` over `[a, b)`.
  a + (b - a) * k div chunks

proc parChunkHi*(a, b, chunks, k: int): int =
  ## Upper bound (exclusive) of chunk `k` of `chunks` over `[a, b)`.
  a + (b - a) * (k + 1) div chunks

# --- join lifecycle --------------------------------------------------------

proc parBegin*(j: var ParJoin; chunks: int) =
  ## Arm the join for `chunks` outstanding runners. Call before submitting.
  atomicStore(j.remaining, chunks)

proc parChunkDone*(j: ptr ParJoin) =
  ## Signal that one chunk runner has finished. Called at the tail of every
  ## chunk runner the plugin emits.
  discard atomicFetchSub(j.remaining, 1, moAcquireRelease)

proc parWait*(j: var ParJoin) =
  ## Block until every chunk runner has finished. v1 is an active spin (the
  ## joining thread also lets the pool's workers make progress); a passive,
  ## I/O-parking join that frees the joining worker is a planned enhancement.
  while atomicLoad(j.remaining, moAcquire) > 0:
    discard

proc parSubmit*(c: Continuation) {.inline.} =
  ## Hand a chunk runner's continuation to the worker pool. A thin re-export of
  ## `threadpool.submit` so the `||` plugin only needs symbols visible through
  ## `import std/parfor`.
  submit(c)

iterator `||`*(a, b: int; n = 0): int {.plugin: "deps/parfor".}
  ## Parallel range `for` loop. `for i in a || b: x[i] = f(input[i])` runs the
  ## body for every `i` in `[a, b)` across the worker pool, joining at the
  ## loop's closing. `n` overrides the chunk count (default: one per worker).
  ##
  ## The body must write only `x[i]`-style outputs at the iteration index and
  ## must not read those outputs back; under that contract iterations are
  ## data-race free by construction. The plugin in `deps/parfor.nim` performs
  ## the rewrite.
