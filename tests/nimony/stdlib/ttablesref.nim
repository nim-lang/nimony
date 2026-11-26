import std/[algorithm, assertions, syncio, tables]

func pairCmp(x, y: (string, int)): int =
  if x[0] < y[0]: -1
  elif x[0] > y[0]: 1
  else: 0

try:
  block:
    var t = newTable[string, int]()
    assert t.len == 0
    t["a"] = 1
    t["b"] = 2
    assert t["a"] == 1
    assert t.hasKey("a")
    assert t.contains("b")
    assert not t.hasKey("missing")
    assert t.getOrDefault("missing") == 0
    assert t.getOrDefault("missing", -1) == -1
    assert t.hasKeyOrPut("a", 9)
    assert t["a"] == 1
    assert not t.hasKeyOrPut("c", 3)
    assert t["c"] == 3
    assert t.mgetOrPut("d", 4) == 4
    t.mgetOrPut("d", 5) = 6
    assert t["d"] == 6
    assert t.mgetOrPut("e") == 0
    t.mgetOrPut("e") = 7
    assert t["e"] == 7

    var keyCount = 0
    for k in t.keys:
      assert t.hasKey(k)
      inc keyCount
    assert keyCount == 5

    var valuesSum = 0
    for v in t.values:
      valuesSum += v
    assert valuesSum == 19

    for k, v in t.mpairs:
      v = v * 2
    assert t["a"] == 2

    for v in t.mvalues:
      v += 1
    assert t["a"] == 3

    var pairSum = 0
    for k, v in t.pairs:
      assert t.hasKey(k)
      pairSum += v
    assert pairSum == 43

    var popped = 0
    assert t.pop("a", popped)
    assert popped == 3
    assert not t.hasKey("a")
    assert t.take("b", popped)
    assert popped == 5
    assert not t.hasKey("b")
    t.del("c")
    assert not t.hasKey("c")
    assert t.len == 2
    t.clear()
    assert t.len == 0

  block:
    let t = newTable([("x", 1), ("y", 2)])
    assert t["x"] == 1
    assert t == newTable([("y", 2), ("x", 1)])

    let indexed = newTableFrom([10, 20], proc (x: int): string = $x)
    assert indexed["10"] == 10
    assert indexed["20"] == 20

  block:
    let t = newTable([("a", 1)])
    assert $t == """{"a": 1}"""
    let alias = t
    alias["b"] = 2
    assert t["b"] == 2

  block:
    var o = newOrderedTable[string, int]()
    o["b"] = 2
    o["a"] = 1
    o["c"] = 3
    assert o.len == 3
    assert o["b"] == 2
    assert o.hasKey("a")
    assert o.contains("c")
    assert o.getOrDefault("missing") == 0
    assert o.getOrDefault("missing", -1) == -1
    assert not o.hasKeyOrPut("d", 4)
    assert o.hasKeyOrPut("d", 5)
    assert o["d"] == 4
    assert o.mgetOrPut("e", 5) == 5
    o.mgetOrPut("e", 6) = 7
    assert o["e"] == 7
    assert o.mgetOrPut("f") == 0
    o.mgetOrPut("f") = 8
    assert o["f"] == 8

    var seen: seq[string] = @[]
    for k in o.keys:
      seen.add k
    assert seen == @["b", "a", "c", "d", "e", "f"]

    var values: seq[int] = @[]
    for v in o.values:
      values.add v
    assert values == @[2, 1, 3, 4, 7, 8]

    for k, v in o.mpairs:
      v += 10
    assert o["b"] == 12

    for v in o.mvalues:
      v += 1
    assert o["b"] == 13

    var sum = 0
    for k, v in o.pairs:
      assert o.hasKey(k)
      sum += v
    assert sum == 91

    var popped = 0
    assert o.pop("b", popped)
    assert popped == 13
    assert not o.hasKey("b")
    o.del("c")
    assert not o.hasKey("c")
    assert o.len == 4
    o.clear()
    assert o.len == 0

  block:
    let o = newOrderedTable([("b", 2), ("a", 1), ("c", 3)])
    assert o != newOrderedTable([("a", 1), ("b", 2), ("c", 3)])
    o.sort(pairCmp)
    assert o == newOrderedTable([("a", 1), ("b", 2), ("c", 3)])

  block:
    let cfg = newOrderedTable[string, OrderedTableRef[string, string]]()
    cfg["main"] = newOrderedTable[string, string]()
    cfg["main"]["key"] = "value"
    assert cfg["main"]["key"] == "value"
    cfg["other"] = newOrderedTable([("x", "y")])
    assert cfg["other"]["x"] == "y"

  block:
    let c = newCountTable(["a", "b", "a"])
    assert c.len == 2
    assert c["a"] == 2
    assert c["b"] == 1
    assert c["missing"] == 0
    assert c.hasKey("a")
    assert c.contains("b")
    assert not c.hasKey("missing")
    assert c.getOrDefault("missing") == 0
    assert c.getOrDefault("missing", 9) == 9
    c.inc("b")
    assert c["b"] == 2
    c.inc("c", 3)
    assert c["c"] == 3
    c["a"] = 5
    assert c["a"] == 5

    var popped = 0
    assert c.pop("b", popped)
    assert popped == 2
    c.del("c")
    assert not c.hasKey("c")
    assert c.len == 1
    c.clear()
    assert c.len == 0

    let empty = newCountTable[string](0)
    assert empty.len == 0

  block:
    var counts = newCountTable(["x", "y", "x"])
    assert counts.smallest().val == 1
    assert counts.largest().val == 2
    assert counts == newCountTable(["y", "x", "x"])
    assert counts != newCountTable(["x", "y", "y"])

    var sum = 0
    for k, v in counts.pairs:
      assert counts.hasKey(k)
      sum += v
    assert sum == 3

    var keyCount = 0
    for k in counts.keys:
      assert counts.hasKey(k)
      inc keyCount
    assert keyCount == 2

    var valuesSum = 0
    for v in counts.values:
      valuesSum += v
    assert valuesSum == 3

    for k, v in counts.mpairs:
      v += 1
    assert counts["x"] == 3

    for v in counts.mvalues:
      v += 1
    assert counts["x"] == 4

  block:
    let merged = newCountTable(["a", "b"])
    merged.merge(newCountTable(["b", "c"]))
    assert merged["a"] == 1
    assert merged["b"] == 2
    assert merged["c"] == 1
    merged.sort(SortOrder.Ascending)
    merged.clear()
    assert merged.len == 0

except:
  assert false

echo "tablesref: OK"
