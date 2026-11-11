{.feature: "staticContracts".}

import std/[algorithm, assertions, tables]

func pairCmp(x, y: (int, int)): int =
  if x[0] < y[0]: -1
  elif x[0] > y[0]: 1
  else: 0

block:
  var a = initTable[int, string]()
  a[1] = "one"
  a[2] = "two"
  a[3] = "three"
  assert a.len == 3
  assert a.getOrQuit(2) == "two"
  assert a.getOrDefault(9, "none") == "none"
  var val = ""
  assert a.pop(2, val)
  assert val == "two"
  a.del(3)
  assert not a.hasKey(3)
  assert a.hasKeyOrPut(1, "uno")
  assert not a.hasKeyOrPut(4, "four")
  assert a.take(4, val)
  assert val == "four"

  var nums = initTable[int, int]()
  nums[1] = 1
  assert nums.mgetOrPut(5, 0) == 0
  assert nums.mgetOrPut(5) == 0

  let b = toTable([(1, "one"), (4, "four")])
  let c = toTable([(4, "four"), (1, "one")])
  assert b == c
  assert b != toTable([(1, "uno"), (4, "four")])
  assert initTable[int, string]() == initTable[int, string]()
  assert initTable[int, string]() != b

block:
  var o = initOrderedTable[int, int]()
  o[3] = 3
  o[1] = 1
  o[2] = 2
  assert o.getOrQuit(1) == 1
  var val = 0
  assert o.pop(2, val)
  o.del(3)
  assert o.hasKeyOrPut(1, 9)
  assert not o.hasKeyOrPut(4, 4)
  assert o.mgetOrPut(5, 0) == 0
  assert o.mgetOrPut(5) == 0
  assert o.take(4, val)
  o[5] = 5
  o.sort(pairCmp)
  assert o == toOrderedTable([(1, 1), (5, 5)])
  assert o != toOrderedTable([(5, 5), (1, 1)])

block:
  var ct = initCountTable[string]()
  ct.inc("a")
  ct.inc("b", 2)
  assert ct["a"] == 1
  assert ct.getOrDefault("z", 9) == 9
  assert ct.hasKey("b")
  ct["b"] = 3
  var val = 0
  assert ct.pop("a", val)
  ct.del("b")
  assert ct.len == 0

  let counts = toCountTable(["x", "y", "x"])
  assert counts["x"] == 2
  assert counts.smallest().val == 1
  assert counts.largest().val == 2
  assert counts == toCountTable(["y", "x", "x"])
  assert counts != toCountTable(["x", "y", "y"])
  assert initCountTable[string]() != counts

  var merged = toCountTable(["a", "b"])
  merged.merge(toCountTable(["b", "c"]))
  assert merged["a"] == 1
  assert merged["b"] == 2
  assert merged["c"] == 1
  merged.sort(SortOrder.Ascending)
  merged.clear()
