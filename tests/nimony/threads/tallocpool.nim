# Port of Nim's tests/threads/tthreadallocatorpool.nim. A thread's allocator
# region outlives the thread: its chunks are owned by a permanent handle, and
# the next thread that starts checks the region out again.
{.feature: "lenientnils".}
import std/[syncio, assertions, rawthreads]

const concurrentThreads = 4

var
  escaped: pointer
  reused: pointer
  bigEscaped: pointer
  roundAddresses: array[2, array[concurrentThreads, pointer]]
  ready: int
  mayExit: int

proc run(fn: proc (arg: pointer) {.nimcall.}; arg: pointer = nil) =
  var t {.noinit.}: RawThread
  try: create(t, fn, arg)
  except: quit "cannot create thread"
  join t

proc allocateEscaped(arg: pointer) {.nimcall.} =
  escaped = alloc(64)
  cast[ptr int](escaped)[] = 42

proc consumeAfterHandoff(arg: pointer) {.nimcall.} =
  assert cast[ptr int](escaped)[] == 42
  dealloc(escaped)
  reused = alloc(64)
  assert reused == escaped
  dealloc(reused)

# A live allocation can outlast its original thread. The next thread receives
# the same allocator and its stable handle makes the deallocation local again.
run(allocateEscaped)
run(consumeAfterHandoff)

proc allocateBigEscaped(arg: pointer) {.nimcall.} =
  bigEscaped = alloc(8192)
  cast[ptr int](bigEscaped)[] = 91

proc consumeBigAfterHandoff(arg: pointer) {.nimcall.} =
  assert cast[ptr int](bigEscaped)[] == 91
  dealloc(bigEscaped)
  let p = alloc(8192)
  assert p == bigEscaped
  dealloc(p)

# Big chunks use a separate deferred-free queue on the stable handle.
run(allocateBigEscaped)
run(consumeBigAfterHandoff)

proc allocateConcurrently(arg: pointer) {.nimcall.} =
  let a = cast[int](arg)
  let round = a div concurrentThreads
  let index = a mod concurrentThreads
  let p = alloc(80)
  roundAddresses[round][index] = p
  dealloc(p)
  discard atomicAddFetch(addr ready, 1, ATOMIC_RELEASE)
  while atomicLoadN(addr mayExit, ATOMIC_ACQUIRE) == 0: discard

proc runConcurrentRound(round: int) =
  var threads {.noinit.}: array[concurrentThreads, RawThread]
  atomicStoreN(addr ready, 0, ATOMIC_RELAXED)
  atomicStoreN(addr mayExit, 0, ATOMIC_RELAXED)
  for i in 0..<concurrentThreads:
    try: create(threads[i], allocateConcurrently, cast[pointer](round * concurrentThreads + i))
    except: quit "cannot create thread"
  while atomicLoadN(addr ready, ATOMIC_ACQUIRE) != concurrentThreads: discard
  atomicStoreN(addr mayExit, 1, ATOMIC_RELEASE)
  for i in 0..<concurrentThreads:
    join threads[i]

# The first round establishes the peak number of simultaneous allocators. The
# following rounds must reuse those regions instead of reserving one region per
# new thread.
runConcurrentRound(0)
for _ in 0..<32:
  runConcurrentRound(1)
  for p in roundAddresses[1]:
    var found = false
    for old in roundAddresses[0]:
      if p == old:
        found = true
        break
    assert found

echo "ok"

{.feature: "assumeSync".}  # test program: globals shared freely
