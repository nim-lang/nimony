# The atomics are `{.intrinsic: "AtomicX".}` rows, not `importc`'d calls: the C
# backend emits the same `__atomic_*` builtin it always did, and a native backend
# emits the instruction sequence directly from the tag alone. Every row in
# `lib/intrinsics` that `std/atomics` exposes is exercised here, on an integer
# cell and on a pointer cell — a pointer is the case that distinguishes an atomic
# from an ordinary bit-counting intrinsic, since the operand type is not a number.

import std/[atomics, assertions]

type Node = object
  val: int

proc main =
  var counter = 0
  atomicStore(counter, 5)
  assert atomicLoad(counter) == 5
  assert atomicFetchAdd(counter, 3) == 5        # returns the OLD value
  assert atomicLoad(counter) == 8
  assert atomicFetchSub(counter, 2) == 8
  assert atomicLoad(counter) == 6
  assert atomicFetchAnd(counter, 3) == 6        # 6 and 3 == 2
  assert atomicLoad(counter) == 2
  assert atomicFetchOr(counter, 8) == 2
  assert atomicLoad(counter) == 10
  assert atomicFetchXor(counter, 3) == 10       # 10 xor 3 == 9
  assert atomicLoad(counter) == 9
  assert atomicExchange(counter, 42) == 9
  assert atomicLoad(counter) == 42

  # A successful compare-exchange leaves `expected` alone and swaps the cell.
  var expected = 42
  assert atomicCompareExchange(counter, expected, 99)
  assert atomicLoad(counter) == 99
  assert expected == 42

  # A failing one must WRITE BACK what it actually saw — that is the protocol the
  # caller retries against, and the one part of a CAS a wrong lowering still
  # "works" without until two threads meet.
  var stale = 7
  assert not atomicCompareExchange(counter, stale, 1)
  assert stale == 99
  assert atomicLoad(counter) == 99

  atomicFence(moSequentiallyConsistent)
  atomicSignalFence(moSequentiallyConsistent)

  # A pointer cell: the same rows, an operand type that is not an integer.
  var a = Node(val: 1)
  var b = Node(val: 2)
  var head: ptr Node = addr a
  assert atomicExchange(head, addr b) == addr a
  assert atomicLoad(head) == addr b
  var exp: ptr Node = addr b
  assert atomicCompareExchange(head, exp, addr a)
  assert atomicLoad(head).val == 1

main()
