#
#
#            Nim's Runtime Library
#        (c) Copyright 2024 Nim contributors
#
#    See the file "copying.txt", included in this
#    distribution, for details about the copyright.
#

## A min-heap (priority queue) implemented as a binary heap over a `seq`.
##
## The smallest element (per `<`) is always at index 0, so `pop` returns
## elements in ascending order. The element type must be `Comparable`.

import std/[assertions]

type
  HeapQueue*[T] = object
    ## A priority queue. Use `initHeapQueue` to create one.
    data: seq[T]

func initHeapQueue*[T](): HeapQueue[T] =
  ## Creates a new empty heap.
  HeapQueue[T](data: @[])

func len*[T](heap: HeapQueue[T]): int {.inline.} =
  ## Number of elements in `heap`.
  heap.data.len

func `[]`*[T](heap: HeapQueue[T]; i: Natural): var T {.inline.} =
  ## Accesses the `i`-th element of `heap`'s internal array. `heap[0]` is the
  ## smallest element.
  heap.data[i]

proc siftUp[T: Comparable](heap: var HeapQueue[T]; p: int) =
  ## Bubbles the element at `p` up toward the root until the heap property
  ## holds. Uses `swap` to move elements (Nimony rejects `s[i] = s[j]` on a
  ## `var` seq as an aliasing copy).
  var pos = p
  while pos > 0:
    let parent = (pos - 1) shr 1
    if heap.data[pos] < heap.data[parent]:
      swap(heap.data[pos], heap.data[parent])
      pos = parent
    else:
      break

proc siftDown[T: Comparable](heap: var HeapQueue[T]; p: int) =
  ## Bubbles the element at `p` down toward the leaves, following the smaller
  ## child, until the heap property holds.
  let endpos = heap.data.len
  var pos = p
  while true:
    let left = 2 * pos + 1
    let right = left + 1
    var smallest = pos
    if left < endpos and heap.data[left] < heap.data[smallest]:
      smallest = left
    if right < endpos and heap.data[right] < heap.data[smallest]:
      smallest = right
    if smallest == pos:
      break
    swap(heap.data[pos], heap.data[smallest])
    pos = smallest

proc push*[T: Comparable](heap: var HeapQueue[T]; item: sink T) =
  ## Pushes `item` onto `heap`, keeping the heap invariant.
  runnableExamples:
    var h = initHeapQueue[int]()
    h.push(5)
    h.push(1)
    assert h[0] == 1
  heap.data.add(item)
  siftUp(heap, heap.data.len - 1)

proc pop*[T: Comparable](heap: var HeapQueue[T]): T =
  ## Removes and returns the smallest element of `heap`.
  ## Requires a non-empty heap.
  runnableExamples:
    var h = initHeapQueue[int]()
    h.push(3); h.push(1); h.push(2)
    assert h.pop() == 1
    assert h.pop() == 2
  let n = heap.data.len
  if n == 1:
    result = heap.data.pop()
  else:
    swap(heap.data[0], heap.data[n - 1])
    result = heap.data.pop()
    siftDown(heap, 0)

proc clear*[T](heap: var HeapQueue[T]) =
  ## Removes all elements from `heap`.
  heap.data = @[]

proc toHeapQueue*[T: Comparable](xs: openArray[T]): HeapQueue[T] =
  ## Builds a heap from the elements of `xs`.
  runnableExamples:
    var h = toHeapQueue([3, 1, 2])
    assert h.pop() == 1
  result = initHeapQueue[T]()
  for i in 0 ..< xs.len:
    result.push(xs[i])

iterator items*[T](heap: HeapQueue[T]): var T =
  ## Yields each element of `heap` in its internal (heap-array) order.
  for i in 0 ..< heap.data.len:
    yield heap.data[i]

func find*[T: Equatable](heap: HeapQueue[T]; x: T): int =
  ## Linear scan for `x`; returns its internal index, or -1 if absent.
  result = -1
  for i in 0 ..< heap.data.len:
    if heap.data[i] == x: return i

func contains*[T: Equatable](heap: HeapQueue[T]; x: T): bool =
  ## Whether `x` is in `heap` (shortcut for `find(heap, x) >= 0`).
  find(heap, x) >= 0

proc del*[T: Comparable](heap: var HeapQueue[T]; index: Natural) =
  ## Removes the element at internal `index`, keeping the heap invariant.
  let last = heap.data.len - 1
  if index >= last:
    discard heap.data.pop()
  else:
    swap(heap.data[index], heap.data[last])
    discard heap.data.pop()
    # the moved-in element belongs either up or down; restoring both ways is
    # safe because exactly one direction applies (the rest of the heap is valid).
    siftUp(heap, index)
    siftDown(heap, index)

proc replace*[T: Comparable](heap: var HeapQueue[T]; item: sink T): T =
  ## Pops and returns the smallest element, then pushes `item` — more efficient
  ## than `pop` + `push`. Requires a non-empty heap. The returned value may be
  ## larger than `item`.
  result = heap.data[0]
  heap.data[0] = item
  siftDown(heap, 0)

proc pushpop*[T: Comparable](heap: var HeapQueue[T]; item: sink T): T =
  ## A `push(item)` immediately followed by a `pop()`, but faster.
  result = item
  if heap.data.len > 0 and heap.data[0] < result:
    swap(result, heap.data[0])
    siftDown(heap, 0)
