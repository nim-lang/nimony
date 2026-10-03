## A routine may touch a mutable global only through a `.sync` routine's
## `var`/`ptr` parameter, unless `{.cast(assumeSync).}` says otherwise.

import std / [atomics, locks]

type
  Pair = object
    a, b: int

var counter: int
var pair: Pair
var lock: Lock
var data: seq[int]
var arr: array[4, int]
var target: int
var p: ptr int = addr target

proc plainRead(): int =
  result = counter

proc plainWrite() =
  counter = 1

proc fieldWrite() =
  pair.a = 3

proc copyLock() =
  let l2 = lock
  discard l2

proc escape(): ptr int =
  result = addr counter

proc derefIsARead() =
  discard atomicLoad(p[])

proc valueArgIsARead() =
  atomicStore(pair.a, counter)

proc fine() =
  discard atomicLoad(counter)
  atomicInc(pair.b)
  discard atomicFetchAdd(arr[2], 1)
  acquire lock
  {.cast(assumeSync).}:
    data.add 1
    pair.a = pair.b
  release lock

counter = 4  # module-level code is exempt
