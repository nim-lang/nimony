# Port of Nim's tests/arc/tdestroy_in_loopcond.nim: a future whose callback
# closure captures the future itself, parked in a heap queue and dropped from
# inside the loop condition. Every future is a cycle through a closure env.
{.feature: "lenientnils".}
import std/syncio

type HeapQueue[T] = object
  data: seq[T]

proc len[T](heap: HeapQueue[T]): int {.inline.} =
  heap.data.len

proc `[]`[T](heap: HeapQueue[T], i: int): T {.inline.} =
  heap.data[i]

proc push[T](heap: var HeapQueue[T], item: sink T) =
  heap.data.add(item)

proc pop[T](heap: var HeapQueue[T]): T =
  result = heap.data.pop

proc clear[T](heap: var HeapQueue[T]) = heap.data = @[]

type
  Future = ref object of RootObj
    s: string
    callme: proc() {.closure.}

var called = 0

proc consume(f: Future) =
  inc called

proc newFuture(s: string): Future =
  var r: Future = nil
  r = Future(s: s, callme: proc() {.closure.} =
    consume r)
  result = r

var q = HeapQueue[tuple[finishAt: int64, fut: Future]](data: @[])

proc sleep(f: int64): Future =
  result = nil
  q.push (finishAt: f, fut: newFuture("async-sleep"))

proc processTimers =
  # Pop the timers in the order in which they will expire (smaller `finishAt`).
  var count = q.len
  let t = high(int64)
  while count > 0 and t >= q[0].finishAt:
    q.pop().fut.callme()
    dec count

var futures: seq[Future] = @[]

proc main =
  for i in 1..200:
    futures.add sleep(56020904056300)
    futures.add sleep(56020804337500)
    processTimers()
    futures = @[]
  q.clear()

let before = getOccupiedMem()
main()
GC_fullCollect()
let leaked = getOccupiedMem() - before
echo called, " ", leaked
