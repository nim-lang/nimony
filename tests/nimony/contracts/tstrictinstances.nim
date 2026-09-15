# Generic code in a module compiled with `staticContracts` is only judged where
# it is instantiated. This module instantiates it under the same feature, so
# a generic body that is not provable fails here rather than in whichever user
# module happens to be strict.

{.feature: "staticContracts".}

import std/[assertions, tables, stripes, intsets]

proc tablesRoundTrip(): int {.raises.} =
  var t = initTable[string, int]()
  for i in 0 ..< 20:
    t[$i] = i
  t.del("3")
  result = t.getOrDefault("7") + t.getOrDefault("3", 100) + t.len
  if t.hasKey("5"):
    result = result + t["5"]
  result = result + t.mgetOrPut("x", 1)

proc stripesRoundTrip(): int =
  var s = default(FifoStripe[int])
  s.init(4)
  discard s.tryEnqueue(5)
  discard s.tryBulkEnqueue([6, 7])
  var buf = default(array[3, int])
  result = s.tryBulkDequeue(3, buf) + buf[0] + buf[2]

proc intsetsRoundTrip(): int =
  var s = initIntSet()
  for i in 0 ..< 300:
    s.incl i * 7
  s.excl 14
  result = 0
  if s.contains(21): result = result + 1
  if not s.contains(14): result = result + 1
  if s.containsOrIncl(700): result = result + 1

try:
  assert tablesRoundTrip() == 7 + 100 + 19 + 5 + 1
except:
  assert false
assert stripesRoundTrip() == 3 + 5 + 7
assert intsetsRoundTrip() == 3
