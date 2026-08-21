import std/[atomics, ticketlocks, syncio]

# --- power-of-2 helpers ---

proc nextPow2(x: int): int =
  if x <= 0: return 1
  result = x - 1
  result = result or (result shr 1)
  result = result or (result shr 2)
  result = result or (result shr 4)
  result = result or (result shr 8)
  result = result or (result shr 16)
  result = result or (result shr 32)
  result = result + 1

# --- FifoStripe: lock-based FIFO queue ---

type
  FifoStripe*[T] = object
    lock*: TicketLock
    head*, tail*, count*: int
    data*: seq[T]

proc init*[T: HasDefault](s: var FifoStripe[T]; capacity: int) =
  let cap = nextPow2(capacity)
  s.data = newSeq[T](cap)

proc tryEnqueue*[T: HasDefault](s: var FifoStripe[T]; item: T): bool =
  s.lock.acquire()
  result = s.count < s.data.len
  if result:
    s.data[s.tail] = item
    s.tail = (s.tail + 1) and (s.data.len - 1)
    inc s.count
  s.lock.release()

proc tryBulkEnqueue*[T: HasDefault](s: var FifoStripe[T]; items: openArray[T]): int =
  ## Enqueue as many leading items of `items` (in order) as fit under one lock
  ## acquisition; returns how many were taken.
  s.lock.acquire()
  result = min(items.len, s.data.len - s.count)
  for i in 0 ..< result:
    s.data[s.tail] = items[i]
    s.tail = (s.tail + 1) and (s.data.len - 1)
  inc s.count, result
  s.lock.release()

proc tryBulkDequeue*[T: HasDefault](s: var FifoStripe[T]; bulkSize: int; buf: var openArray[T]): int =
  s.lock.acquire()
  result = min(s.count, min(bulkSize, buf.len))
  for i in 0 ..< result:
    buf[i] = s.data[s.head]
    s.head = (s.head + 1) and (s.data.len - 1)
  dec s.count, result
  s.lock.release()

proc grow*[T: HasDefault](s: var FifoStripe[T]; newCapacity: int) =
  s.lock.acquire()
  if newCapacity <= s.data.len:
    s.lock.release()
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
  s.lock.release()
