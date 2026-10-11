# Port of Nim's tests/threads/tthreadallocatorhandoffrace.nim. The owner
# retires with live small and big allocations; a borrower checks out that
# region while this thread returns the cells. This races foreign queue
# publication against both directions of the region handoff.
{.feature: "lenientnils".}
import std/[syncio, assertions, rawthreads]

const
  pointerCount = 512
  drainCount = 2048
  iterations = 200
  sizes = [16, 64, 4000, 4096, 8192]

var
  pointers: array[pointerCount, pointer]
  mayExit: int

proc owner(arg: pointer) {.nimcall.} =
  for i in 0..<pointerCount:
    let size = sizes[i mod sizes.len]
    pointers[i] = alloc(size)
    cast[ptr byte](pointers[i])[] = byte(i and 255)

proc borrower(arg: pointer) {.nimcall.} =
  while atomicLoadN(addr mayExit, ATOMIC_ACQUIRE) == 0: discard

proc drain(arg: pointer) {.nimcall.} =
  var drained {.noinit.}: array[drainCount, pointer]
  for i in 0..<drainCount:
    drained[i] = alloc(sizes[i mod sizes.len])
  for p in drained:
    dealloc(p)
  let occupied = getOccupiedMem()
  if occupied != 0:
    echo "allocator retained ", occupied, " bytes"

proc spawnJoin(fn: proc (arg: pointer) {.nimcall.}) =
  var t {.noinit.}: RawThread
  try: create(t, fn, nil)
  except: quit "cannot create thread"
  join t

for _ in 0..<iterations:
  spawnJoin(owner)
  atomicStoreN(addr mayExit, 0, ATOMIC_RELAXED)
  var b {.noinit.}: RawThread
  try: create(b, borrower, nil)
  except: quit "cannot create thread"
  for i in 0..<pointerCount:
    if i == pointerCount div 2:
      # Let the borrower tear the allocator down while the second half of the
      # foreign publications are still in flight.
      atomicStoreN(addr mayExit, 1, ATOMIC_RELEASE)
    dealloc(pointers[i])
  join b
  spawnJoin(drain)

echo "ok"

{.feature: "assumeSync".}  # test program: globals shared freely
