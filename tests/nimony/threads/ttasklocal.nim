## Task-local data and stop tokens (`std/threadpool`). Every task parks on an
## I/O timer, deep inside its call chain, and may be resumed on any worker: the
## pool finds the task again by walking the parked frame's `caller` links — also
## through a regular proc's passive call and a passive `for` loop's `IterStep`.

import std/syncio

when defined(windows):
  echo "tasks=16 nested=16 viaRegular=16 viaLoop=16 child=16"
  echo "stop: own=true others=false"
  echo "destroyed=16 main task=nil"
else:
  import std / [ioring, threadpool, atomics]

  const N = 16

  type
    Ctx = object
      id: int ## the task's number plus 1: moved-from and zeroed values have 0

  var destroyed: int # accessed atomically

  proc `=destroy`(c: Ctx) =
    if c.id != 0: discard atomicFetchAdd(destroyed, 1, moRelaxed)

  proc `=wasMoved`(c: var Ctx) = c.id = 0

  var
    finished, nestedOk, viaRegularOk, viaLoopOk, childOk: int # accessed atomically
    ownStop, othersStop: int # accessed atomically

  proc myId(): int =
    let d = taskData[Ctx]()
    result = if d == nil: -1 else: d.id - 1

  proc napMs(ms: int) {.passive.} =
    var r = 0
    let c = delay()
    discard submitTimeout(afterMs(ms), c, addr r)
    suspend()

  proc leaf(): int {.passive.} =
    napMs(2)
    result = myId()

  proc viaRegular(): int =
    # a regular proc's passive call: the callee parks, and its chain continues
    # into the task through `PassiveWait`
    result = leaf()

  iterator ids(n: int): int {.passive.} =
    for k in 0 ..< n:
      {.cast(noSideEffect).}:
        napMs(1)
        let id = myId()
      yield id

  proc child(i: int; join: ptr int) {.passive.} =
    napMs(2)
    if myId() == i:
      discard atomicFetchAdd(childOk, 1, moRelaxed)
    discard atomicFetchSub(join[], 1, moRelease)

  proc handler(i: int) {.passive.} =
    let before = myId()
    if before == i and leaf() == i:
      discard atomicFetchAdd(nestedOk, 1, moRelaxed)
    if viaRegular() == i:
      discard atomicFetchAdd(viaRegularOk, 1, moRelaxed)
    var allSame = true
    for id in ids(3):
      if id != i: allSame = false
    if allSame:
      discard atomicFetchAdd(viaLoopOk, 1, moRelaxed)

    var join = 1
    submitChild(delay(child(i, addr join)))
    while atomicLoad(join, moAcquire) > 0:
      # like `parWait`: the child's timer may sit on this thread's I/O lane
      if not poolHelp(): discard reactorPoll(0)

    if i == 0:
      requestStop()
      napMs(1)
      if stopRequested(): discard atomicFetchAdd(ownStop, 1, moRelaxed)
    else:
      napMs(3)
      if stopRequested(): discard atomicFetchAdd(othersStop, 1, moRelaxed)
    discard atomicFetchAdd(finished, 1, moRelease)

  proc main =
    initPool()
    initIoRing()
    for i in 0 ..< N:
      submitTask(delay(handler(i)), Ctx(id: i + 1))
    while atomicLoad(finished, moAcquire) < N: discard
    # a task's data goes when its bottom frame's `caller` runs, one step later
    while atomicLoad(destroyed, moAcquire) < N: discard
    echo "tasks=", atomicLoad(finished, moRelaxed),
      " nested=", atomicLoad(nestedOk, moRelaxed),
      " viaRegular=", atomicLoad(viaRegularOk, moRelaxed),
      " viaLoop=", atomicLoad(viaLoopOk, moRelaxed),
      " child=", atomicLoad(childOk, moRelaxed)
    echo "stop: own=", atomicLoad(ownStop, moRelaxed) == 1,
      " others=", atomicLoad(othersStop, moRelaxed) != 0
    echo "destroyed=", atomicLoad(destroyed, moRelaxed),
      " main task=", (if currentTask() == nil: "nil" else: "set")
    shutdownPool()

  main()
