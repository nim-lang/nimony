import std/[rawthreads, atomics, syncio]

# Every thread here holds its OWN counted reference to the same cell and drops
# it concurrently with the others. Exactly one free per object must happen: a
# leak (nobody frees) and a double free (two threads free) are both caught.
#
# This is what constrains `arcDec`'s uniquely-referenced fast path in
# `system/atomicarc`. Skipping the atomic RMW is only allowed when the ACQUIRE
# load proves the count is zero, i.e. that no other thread holds a reference;
# when it is not, the answer must come from the value the RMW itself returned.
# Deciding "who frees" from a separate load and discarding the RMW result is
# the nim-lang/threading#45 bug, where every participant drops the role at once
# and the object leaks. With NumThreads+1 owners per object, this test drives
# the contended path on every drop but the last.

type
  Payload = object
    id: int
  Obj = ref Payload

var freeCount: int = 0

proc `=destroy`(p: Payload) =
  discard atomicFetchAdd(freeCount, 1, moRelease)

const
  NumObjects = 2000
  NumThreads = 6
  Rounds = 3

type
  Arg = object
    refs: seq[Obj]

var
  go: bool = false
  args: array[NumThreads, Arg]

proc worker(p: pointer) =
  let a = cast[ptr Arg](p)
  while not atomicLoad(go, moAcquire):
    cpuRelax()
  a.refs.setLen(0)          # drop them all, as fast as possible

proc main =
  var expected = 0
  for round in 1..Rounds:
    var mine = newSeq[Obj](NumObjects)
    for i in 0 ..< NumObjects:
      mine[i] = Obj(id: i)
    for t in 0 ..< NumThreads:
      args[t].refs = newSeq[Obj](NumObjects)
      for i in 0 ..< NumObjects:
        args[t].refs[i] = mine[i]   # counted copy
    atomicStore(go, false, moRelease)

    var thr {.noinit.}: array[NumThreads, RawThread]
    try:
      for t in 0 ..< NumThreads:
        create thr[t], worker, addr(args[t])
    except:
      echo "error creating thread"
      return
    atomicStore(go, true, moRelease)   # everybody drops at once...
    mine.setLen(0)                     # ...including this thread
    for t in 0 ..< NumThreads:
      thr[t].join()

    expected = expected + NumObjects
    let got = atomicLoad(freeCount, moAcquire)
    if got != expected:
      echo "round ", round, ": got ", got, " frees, expected ", expected,
           (if got < expected: " (leak)" else: " (double free)")
      quit 1
  echo "ok"

when not defined(windows):
  main()
else:
  echo "ok"
