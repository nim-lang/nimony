# Memory allocated by a thread that has exited is freed by another one. The
# allocator's per-thread region used to live in TLS, which the thread library
# releases when the thread ends, and a foreign free writes to the owner
# region's `sharedFreeLists`: this crashed with more than a few threads. The
# region now lives in OS pages and passes to the next thread that starts.
{.feature: "lenientnils".}
import std/[syncio, rawthreads]

type Leaf = ref object
  x: int
  s: string

const NumThreads = 8
var slots: array[NumThreads, seq[Leaf]]

proc worker(arg: pointer) {.nimcall.} =
  let tid = cast[int](arg)
  for i in 0..<3000:
    slots[tid].add Leaf(x: i, s: "leaf" & $i)

proc main =
  var threads {.noinit.}: array[NumThreads, RawThread]
  for i in 0..<NumThreads:
    try: create(threads[i], worker, cast[pointer](i))
    except: quit "cannot create thread"
  for i in 0..<NumThreads: join threads[i]
  var total = 0
  for i in 0..<NumThreads:
    total = total + slots[i].len
    slots[i] = @[]   # frees memory allocated by threads that have exited
  # the heaps of the exited threads are adopted by new ones
  for i in 0..<NumThreads:
    try: create(threads[i], worker, cast[pointer](i))
    except: quit "cannot create thread"
  for i in 0..<NumThreads: join threads[i]
  for i in 0..<NumThreads:
    total = total + slots[i].len
    slots[i] = @[]
  echo "freed ", total

main()

{.feature: "assumeSync".}  # test program: globals shared freely
