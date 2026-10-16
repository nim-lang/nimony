{.feature: "staticContracts".}

import std/[atomics, syncio]
import std/private/syslocks

# --- power-of-2 helpers ---

proc nextPow2(x: int): int =
  if x <= 0: return 1
  result = x - 1
  result = result or (result shr 1)
  result = result or (result shr 2)
  result = result or (result shr 4)
  result = result or (result shr 8)
  result = result or (result shr 16)
  when sizeof(int) > 4:
    # A shift by the full width of `int` is undefined in C, so this step only
    # exists on 64-bit targets; on 32-bit the 16-shift above already saturates.
    result = result or (result shr 32)
  result = result + 1

# --- FifoStripe: lock-based FIFO queue ---

type
  FifoStripe*[T] = object
    ## A bounded FIFO guarded by a `SysLock`. A waiter parks in the kernel
    ## instead of spinning, so a preempted holder or waiter does not stall
    ## every thread queued behind it. `init` must run before first use, and an
    ## initialised stripe must not be moved.
    lock*: SysLock
    head*, tail*, count*: int
    data*: seq[T]

proc init*[T: HasDefault](s: var FifoStripe[T]; capacity: int) =
  let cap = nextPow2(capacity)
  initSysLock(s.lock)
  s.data = newSeq[T](cap)

proc tryEnqueue*[T: HasDefault](s: var FifoStripe[T]; item: T): bool =
  acquireSys(s.lock)
  result = s.count < s.data.len
  if result and s.data.len > 0:
    s.data[s.tail and (s.data.len - 1)] = item
    s.tail = (s.tail + 1) and (s.data.len - 1)
    inc s.count
  releaseSys(s.lock)

proc tryBulkEnqueue*[T: HasDefault](s: var FifoStripe[T]; items: openArray[T]): int =
  ## Enqueue as many leading items of `items` (in order) as fit under one lock
  ## acquisition; returns how many were taken.
  acquireSys(s.lock)
  result = min(items.len, s.data.len - s.count)
  if s.data.len > 0:
    for i in 0 ..< result:
      s.data[s.tail and (s.data.len - 1)] = items[i]
      s.tail = (s.tail + 1) and (s.data.len - 1)
  inc s.count, result
  releaseSys(s.lock)

proc tryBulkDequeue*[T: HasDefault](s: var FifoStripe[T]; bulkSize: int; buf: var openArray[T]): int =
  acquireSys(s.lock)
  result = min(s.count, min(bulkSize, buf.len))
  if s.data.len > 0:
    for i in 0 ..< result:
      buf[i] = s.data[s.head and (s.data.len - 1)]
      s.head = (s.head + 1) and (s.data.len - 1)
  dec s.count, result
  releaseSys(s.lock)

proc grow*[T: HasDefault](s: var FifoStripe[T]; newCapacity: int) =
  acquireSys(s.lock)
  if newCapacity <= s.data.len:
    releaseSys(s.lock)
    return
  let cap = nextPow2(newCapacity)
  var newData = newSeq[T](cap)
  var idx = s.head
  let mask = s.data.len - 1
  for i in 0 ..< s.count:
    newData[i] = s.data[idx]
    idx = (idx + 1) and mask
  s.data = newData
  s.head = 0
  s.tail = s.count
  releaseSys(s.lock)
