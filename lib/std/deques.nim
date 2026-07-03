#
#
#            Nim's Runtime Library
#        (c) Copyright 2024 Nim contributors
#
#    See the file "copying.txt", included in this
#    distribution, for details about the copyright.
#

## An implementation of a `deque`:idx: (double-ended queue).
## The underlying implementation uses a `seq`.
##
## This is the Nimony port. The container is a growable, power-of-two ring
## buffer: elements are appended/prepended in amortized O(1) and indexed in
## O(1). Since exceptions are not yet wired up, accessing an element of an
## empty `Deque` (or an out-of-range index) is a broken precondition, expressed
## as a `.requires` contract, rather than raising `IndexDefect`.

import std/assertions

const
  defaultInitialSize = 4

type
  Deque*[T] = object
    ## A double-ended queue backed by a ring buffer whose capacity is always a
    ## power of two. Logical element `i` lives at `data[(head + i) and mask]`.
    data: seq[T]
    head, tail, count, mask: int


func nextPowerOfTwo(x: int): int =
  ## Smallest power of two `>= x` (at least 1).
  result = 1
  while result < x:
    result = result shl 1

func newRingBuf[T](cap: int): seq[T] =
  ## Allocates a ring buffer of `cap` uninitialized slots and marks every slot
  ## moved-out. This is the buffer's invariant: a *dead* slot always stays in
  ## the moved-from state, so it is safe to destroy and safe to overwrite via
  ## `=sink` (which no-ops the destroy of the moved-from value before storing).
  ## A slot only holds a real value while it is live, i.e. in `[head, head+count)`.
  ## Because the elements are never default-initialized, `T` needs no default.
  result = newSeqUninit[T](cap)
  for i in 0 ..< cap:
    `=wasMoved`(result[i])

func initDeque*[T](initialSize: int = defaultInitialSize): Deque[T] =
  ## Creates a new empty `Deque`.
  ##
  ## `initialSize` is rounded up to the next power of two and used as the
  ## initial capacity, avoiding reallocations when the final size is known.
  runnableExamples:
    var a = initDeque[int]()
    assert a.len == 0
  let cap = nextPowerOfTwo(initialSize)
  result = Deque[T](data: newRingBuf[T](cap), head: 0, tail: 0, count: 0, mask: cap - 1)

func len*[T](d: Deque[T]): int {.inline.} =
  ## Returns the number of elements of `d`.
  d.count

func growIfNeeded[T](d: var Deque[T]) =
  ## Doubles the capacity if the ring buffer is full, re-laying the elements
  ## out contiguously starting at index 0.
  if d.count < d.data.len:
    return
  let oldCap = d.data.len
  let newCap = if oldCap == 0: defaultInitialSize else: oldCap * 2
  var newData = newRingBuf[T](newCap)
  var i = 0
  var j = d.head
  while i < d.count:
    newData[i] = move(d.data[j])
    j = (j + 1) and d.mask
    inc i
  d.data = newData
  d.head = 0
  d.tail = d.count
  d.mask = newCap - 1

func addLast*[T](d: var Deque[T]; item: sink T) =
  ## Adds an `item` to the end of `d`.
  runnableExamples:
    var a = initDeque[int]()
    for i in 1 .. 3: a.addLast(i)
    assert $a == "[1, 2, 3]"
  growIfNeeded(d)
  d.data[d.tail] = item
  d.tail = (d.tail + 1) and d.mask
  inc d.count

func addFirst*[T](d: var Deque[T]; item: sink T) =
  ## Adds an `item` to the beginning of `d`.
  runnableExamples:
    var a = initDeque[int]()
    for i in 1 .. 3: a.addFirst(i)
    assert $a == "[3, 2, 1]"
  growIfNeeded(d)
  d.head = (d.head - 1) and d.mask
  d.data[d.head] = item
  inc d.count

func peekFirst*[T](d: Deque[T]): var T {.requires: d.count > 0.} =
  ## Returns the first element of `d`. Requires `d` to be non-empty.
  result = d.data[d.head]

func peekLast*[T](d: Deque[T]): var T {.requires: d.count > 0.} =
  ## Returns the last element of `d`. Requires `d` to be non-empty.
  result = d.data[(d.tail - 1) and d.mask]

func popFirst*[T](d: var Deque[T]): T {.requires: d.count > 0.} =
  ## Removes and returns the first element of `d`. Requires `d` to be non-empty.
  result = move(d.data[d.head])
  d.head = (d.head + 1) and d.mask
  dec d.count

func popLast*[T](d: var Deque[T]): T {.requires: d.count > 0.} =
  ## Removes and returns the last element of `d`. Requires `d` to be non-empty.
  d.tail = (d.tail - 1) and d.mask
  result = move(d.data[d.tail])
  dec d.count

func `[]`*[T](d: Deque[T]; i: int): var T {.requires: i >= 0 and i < d.count.} =
  ## Accesses the `i`-th element of `d` (0-based, counting from the front).
  ## Requires `i` to be in range.
  runnableExamples:
    var a = initDeque[int]()
    for i in 1 .. 3: a.addLast(i)
    assert a[0] == 1
    assert a[2] == 3
  result = d.data[(d.head + i) and d.mask]

func `[]=`*[T](d: var Deque[T]; i: int; val: sink T) {.requires: i >= 0 and i < d.count.} =
  ## Sets the `i`-th element of `d` (0-based, counting from the front).
  ## Requires `i` to be in range.
  d.data[(d.head + i) and d.mask] = val

func clear*[T](d: var Deque[T]) =
  ## Resets `d` to an empty state, destroying its elements.
  d.data.shrink(0)
  d.head = 0
  d.tail = 0
  d.count = 0
  d.mask = 0

func contains*[T: Equatable](d: Deque[T]; item: T): bool =
  ## Returns `true` if `item` is in `d`. Used by the `in` operator.
  runnableExamples:
    var a = initDeque[int]()
    for i in 1 .. 3: a.addLast(i)
    assert 2 in a
    assert 7 notin a
  for i in 0 ..< d.count:
    if d.data[(d.head + i) and d.mask] == item:
      return true
  result = false

iterator items*[T](d: Deque[T]): var T =
  ## Yields every element of `d`, from first to last.
  for i in 0 ..< d.count:
    yield d.data[(d.head + i) and d.mask]

iterator mitems*[T](d: var Deque[T]): var T =
  ## Yields every element of `d` by reference, allowing modification in place.
  for i in 0 ..< d.count:
    yield d.data[(d.head + i) and d.mask]

iterator pairs*[T](d: Deque[T]): (int, var T) =
  ## Yields every `(index, element)` pair of `d`, from first to last.
  for i in 0 ..< d.count:
    yield (i, d.data[(d.head + i) and d.mask])

func `$`*[T: Stringable](d: Deque[T]): string =
  ## Returns the string representation of `d`, e.g. `"[1, 2, 3]"`.
  result = "["
  var first = true
  for i in 0 ..< d.count:
    if not first:
      result.add ", "
    first = false
    result.add $d.data[(d.head + i) and d.mask]
  result.add "]"

func toDeque*[T](xs: openArray[T]): Deque[T] =
  ## Creates a new `Deque` containing the elements of `xs`, in order.
  runnableExamples:
    let a = toDeque([1, 2, 3])
    assert a.len == 3
    assert a.peekFirst == 1
    assert a.peekLast == 3
  result = initDeque[T](xs.len)
  for i in 0 ..< xs.len:
    result.addLast(xs[i])
