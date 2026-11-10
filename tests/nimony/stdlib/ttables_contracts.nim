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
  let ra = newTable([(1, "one"), (2, "two")])
  let rb = ra
  rb[3] = "three"
  assert ra == rb
  assert ra.getOrQuit(3) == "three"
  assert ra.hasKeyOrPut(1, "uno")
  assert not ra.hasKeyOrPut(4, "four")
  assert ra.mgetOrPut(5, "five") == "five"
  var val = ""
  assert ra.pop(2, val)
  ra.del(1)
  ra.clear()
  assert ra.len == 0

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
  let ro = newOrderedTable([(1, 1), (2, 2)])
  assert ro == newOrderedTable([(1, 1), (2, 2)])
  assert ro.getOrQuit(1) == 1
  assert ro.hasKeyOrPut(1, 9)
  assert not ro.hasKeyOrPut(3, 3)
  assert ro.mgetOrPut(4, 4) == 4
  var val = 0
  assert ro.pop(2, val)
  ro[1] = 10
  ro.sort(pairCmp)
  ro.clear()
  assert ro.len == 0

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

block:
  let rc = newCountTable(["p", "p", "q"])
  assert rc["p"] == 2
  assert rc == newCountTable(["q", "p", "p"])
  rc.inc("r")
  var val = 0
  assert rc.pop("q", val)
  rc.del("r")
  rc.merge(newCountTable(["p"]))
  assert rc["p"] == 3
  rc.sort(SortOrder.Ascending)
  rc.clear()
