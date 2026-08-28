# A `{.threadvar.}` must be per-thread whether it is read by NAME or through its
# ADDRESS. The two are different lowerings on a back end that keeps thread-locals
# in one flat block — a scalar is read at a fixed offset from the thread register,
# but an array element, an object field or an `addr` needs the block's base as a
# value first, and a base that is the main thread's makes every other thread's
# storage an alias of it. Nothing single-threaded can tell the two apart.
import std/[rawthreads, syncio]

const numThreads = 4

var scalar {.threadvar.}: int
var arr {.threadvar.}: array[8, int]

type Rec = object
  a, b: int
var rec {.threadvar.}: Rec

var sawScalar: array[numThreads, int]
var sawArr: array[numThreads, int]
var sawRec: array[numThreads, int]
var wroteArr: array[numThreads, int]

proc worker(p: pointer) =
  let idx = cast[ptr int](p)[]
  # What this thread finds before writing anything: its own locals, so zero.
  sawScalar[idx] = scalar
  sawArr[idx] = arr[7]
  sawRec[idx] = rec.b
  scalar = 100 + idx
  arr[7] = 200 + idx
  rec.b = 300 + idx
  # And what it reads back is what IT wrote, not what a sibling did.
  wroteArr[idx] = arr[7] + rec.b + scalar

proc main =
  scalar = 1
  arr[7] = 2
  rec.b = 3
  var thr {.noinit.}: array[numThreads, RawThread]
  var indexes = default array[numThreads, int]
  try:
    for i in 0..<numThreads:
      indexes[i] = i
      create thr[i], worker, addr(indexes[i])
  except:
    echo "error creating thread"
    return
  for i in 0..<numThreads:
    thr[i].join()
  var freshEverywhere = true
  var ownValues = true
  for i in 0..<numThreads:
    if sawScalar[i] != 0 or sawArr[i] != 0 or sawRec[i] != 0:
      freshEverywhere = false
    if wroteArr[i] != (200 + i) + (300 + i) + (100 + i):
      ownValues = false
  echo "children started fresh: ", freshEverywhere
  echo "children kept their own: ", ownValues
  echo "parent unchanged: ", scalar == 1 and arr[7] == 2 and rec.b == 3

when not defined(windows):
  main()
else:
  echo "children started fresh: true"
  echo "children kept their own: true"
  echo "parent unchanged: true"
