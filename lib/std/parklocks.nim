#
#
#            Nimony's Runtime Library
#        (c) Copyright 2026 Andreas Rumpf
#
#    See the file "copying.txt", included in this
#    distribution, for details about the copyright.
#

## A mutex for `.passive` code: a task that finds the lock taken *parks*
## instead of blocking its thread, and `release` hands the lock to the oldest
## waiter and resumes it on the thread pool (`std/threadpool`).
##
## .. code-block:: nim
##   var L: ParkLock
##   proc bump() {.passive.} =
##     withLock L:
##       inc counter     # may itself call `.passive` procs
##
## Ownership belongs to the task, not to a thread: a task that parks while
## holding the lock may resume on another worker and release it there. Waiters
## are served in arrival order, and an uncontended `acquire`/`release` is one
## compare-and-swap each.
##
## A regular proc may call `acquire` too; like any call of a `.passive` proc
## from regular code it waits until it has the lock, doing pool work meanwhile.
## Contended waiters are resumed via `threadpool.submit`, so the pool must be
## running (`initPool`) whenever the lock can be contended.
##
## The lock is not re-entrant: a task that acquires it twice waits for itself.

# `waiters` is guarded by `spin`, which the shared-global check cannot see.
{.feature: "assumeSync".}

import std / [atomics, ticketlocks, threadpool]

const
  Free = 0
  Held = 1        ## held, and nobody waits: `release` is a CAS back to `Free`
  Contended = 2   ## held, and `waiters` may be non-empty: `release` takes `spin`

type
  ParkLock* = object
    state: int                  ## Free/Held/Contended; accessed atomically
    spin: TicketLock            ## guards `waiters`, `head` and Held->Contended
    waiters: seq[Continuation]  ## parked acquirers, oldest at `head`
    head: int

proc `=copy`(dest: var ParkLock; src: ParkLock) {.error.}
proc `=dup`(x: ParkLock): ParkLock {.error.}

proc tryAcquire*(L: var ParkLock): bool {.sync, inline.} =
  ## Takes the lock if it is free; never parks.
  var expected = Free
  result = atomicCompareExchange(L.state, expected, Held, moAcquire, moRelaxed)

proc parkOrTake(L: var ParkLock) {.passive, sync.} =
  ## The slow path. Under `spin` either the lock turned free (take it) or it is
  ## marked `Contended` and this task joins the queue. A releaser that sees
  ## `Contended` takes `spin` too, so it cannot miss the enqueue.
  acquire L.spin
  var s = atomicLoad(L.state, moRelaxed)
  while true:
    if s == Free:
      if atomicCompareExchange(L.state, s, Held, moAcquire, moRelaxed): break
    elif s == Held:
      if atomicCompareExchange(L.state, s, Contended, moRelaxed, moRelaxed):
        s = Contended
    else:
      L.waiters.add delay()
      release L.spin
      # Nothing may touch this frame from here on: once `spin` is released a
      # `release` on another thread can resume the continuation, finish this
      # call and free the frame before this state function has returned.
      suspend()
      # Resumed by `release`, which handed the lock over without freeing it.
      return
  release L.spin

proc acquire*(L: var ParkLock) {.passive, sync.} =
  ## Takes the lock, parking the task until it is its turn.
  if not tryAcquire(L): parkOrTake(L)

proc release*(L: var ParkLock) {.sync.} =
  ## Releases the lock, or hands it to the oldest waiter and resumes that one.
  var expected = Held
  if atomicCompareExchange(L.state, expected, Free, moRelease, moRelaxed):
    return
  # `Contended`: under `spin` the queue is non-empty, because an acquirer sets
  # `Contended` and enqueues in one hold of `spin`, and the state drops back to
  # `Held` below as soon as the queue runs empty.
  acquire L.spin
  let wake = L.waiters[L.head]   # stays held: the lock is `wake`'s now
  inc L.head
  if L.head == L.waiters.len:
    L.waiters.setLen 0
    L.head = 0
    atomicStore(L.state, Held, moRelaxed)
  release L.spin
  submit(wake)

template withLock*(L: ParkLock; body: untyped) =
  ## Acquires `L`, runs `body` and releases `L`, also when `body` raises.
  acquire(L)
  try:
    body
  finally:
    release(L)
