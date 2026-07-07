## Regression for externally resumed passive coroutines:
## multiple parked continuations can be resumed in scheduler-defined order.
import std / [syncio, assertions]

const MaxPending = 8

type
  Pending = object
    ms: int
    cont: Continuation

var
  pending: array[MaxPending, Pending]
  pendingCount = 0

proc enqueue(ms: int; cont: Continuation) =
  assert pendingCount < MaxPending
  pending[pendingCount] = Pending(ms: ms, cont: cont)
  inc pendingCount

proc dequeueSoonest(): Pending =
  assert pendingCount > 0
  var best = 0
  for i in 1..<pendingCount:
    if pending[i].ms < pending[best].ms:
      best = i
  result = pending[best]
  for i in best+1..<pendingCount:
    pending[i - 1] = pending[i]
  dec pendingCount

proc sleepAsync(ms: int) {.passive.} =
  let cont = delay()
  enqueue(ms, cont)
  suspend()

proc task(name: string; ms: int) {.passive.} =
  echo name, " start"
  sleepAsync(ms)
  echo name, " resumed after ", ms

proc launch() {.passive.} =
  let a = delay task("A", 20)
  let b = delay task("B", 10)
  complete(a)
  complete(b)

  assert pendingCount == 2
  while pendingCount > 0:
    let next = dequeueSoonest()
    complete(next.cont)

launch()
