import std/[hashes, assertions, tables, algorithm]

var hashCalls = 0

type
  ProbeKey = object
    value: int

func hash(k: ProbeKey): Hash =
  {.cast(noSideEffect).}:
    {.cast(assumeSync).}:
      inc hashCalls
  Hash(k.value)

func `==`(a, b: ProbeKey): bool =
  a.value == b.value

func pairCmp(x, y: (string, int)): int =
  if x[0] < y[0]: -1
  elif x[0] > y[0]: 1
  else: 0

block:
  var t = initTable[int, int]()
  assert not t.contains(123)
  assert not t.hasKey(123)
  assert t.getOrDefault(123) == 0
  assert t.len == 0
  t[123] = 321
  try:
    assert t.len == 1
    assert t.contains(123)
    assert t.hasKey(123)
    assert t.getOrDefault(123) == 321
    assert t[123] == 321
    assert t.mgetOrPut(123, -1) == 321
    inc t[123]
    assert t[123] == 322
    assert t.mgetOrPut(456, 654) == 654
    assert t.len == 2
    assert t.contains(456)
    assert t.hasKey(456)
    assert t.getOrDefault(456) == 654
    assert t[456] == 654
  except:
    assert false


  let HC123 = (1 shl 31) + 123  # Hash collision to '123'
  assert not t.contains(HC123)
  assert t.getOrDefault(HC123) == 0

block:
  const Max = 1000

  var t = initTable[int, int]()
  for i in 1 ..< Max:
    t[i] = -i
  assert t.len == (Max - 1)
  for i in 1 ..< Max:
    try:
      assert t[i] == -i
    except:
      assert false

  for k, v in t.pairs:
    assert k == -v
    assert t.contains(k)
    assert not t.contains(-k)
    assert t.getOrDefault(k) == v
  assert t.mgetOrPut(12, 0) == -12

  assert t.len == (Max - 1)
  assert not t.contains(0)

  for k, v in t.mpairs:
    v = -v
    assert k == v

  for k, v in t.pairs:
    assert k == v

block:
  # Deletion closes the dense gap and rebuilds the index when needed.
  var t = initTable[int, int]()
  for i in 0 ..< 12:
    t[i] = i * 10
  t.del(5)
  assert not t.hasKey(5)
  for i in [0, 1, 2, 3, 4, 6, 7, 8, 9, 10, 11]:
    assert t.getOrQuit(i) == i * 10
  var val = 0
  assert t.pop(6, val)
  assert val == 60
  assert not t.hasKey(6)
  assert t.getOrQuit(11) == 110

block:
  # Tiny tables do not call `hash` at all; the fifth insertion builds the index.
  hashCalls = 0
  var t = initTable[ProbeKey, int]()
  for i in 0 ..< 4:
    t[ProbeKey(value: i)] = i
  assert hashCalls == 0
  assert t.getOrQuit(ProbeKey(value: 2)) == 2
  assert hashCalls == 0

  t[ProbeKey(value: 4)] = 4
  assert hashCalls == 5 # four indexed entries plus the new key
  assert t.getOrQuit(ProbeKey(value: 0)) == 0
  assert hashCalls > 5

block:
  # Tables with at most four entries keep their pairs in insertion order and do
  # not build the hash index. Deleting closes the dense gap; the fifth entry
  # switches to the indexed representation.
  var t = initTable[int, string]()
  t[3] = "three"
  t[1] = "one"
  t[2] = "two"
  t[4] = "four"
  var seen: seq[int] = @[]
  for k in t.keys:
    seen.add k
  assert seen == @[3, 1, 2, 4]
  assert $t == """{3: "three", 1: "one", 2: "two", 4: "four"}"""

  t.del(1)
  t[1] = "ONE"
  seen = @[]
  for k in t.keys:
    seen.add k
  assert seen == @[3, 2, 4, 1]

  t[5] = "five"
  assert t.len == 5
  for k in [1, 2, 3, 4, 5]:
    assert t.hasKey(k)
  assert t.getOrQuit(5) == "five"

block:
  # OrderedTable value semantics.
  var t = initOrderedTable[string, int]()
  t["b"] = 2
  t["a"] = 1
  t["c"] = 3
  assert t.len == 3
  assert t.getOrQuit("b") == 2
  assert t.getOrDefault("missing") == 0
  assert t.getOrDefault("missing", -1) == -1

  var seen: seq[string] = @[]
  for k in t.keys:
    seen.add k
  assert seen == @["b", "a", "c"]

  assert not t.hasKeyOrPut("d", 4)
  assert t.hasKeyOrPut("d", 5)
  assert t.getOrQuit("d") == 4
  assert t.mgetOrPut("e", 5) == 5
  t.mgetOrPut("e", 6) = 7
  assert t.getOrQuit("e") == 7

  var val = 0
  assert not t.pop("missing", val)
  assert val == 0
  assert t.pop("b", val)
  assert val == 2
  assert not t.hasKey("b")
  assert t.take("a", val)
  assert val == 1

  seen = @[]
  for k in t.keys:
    seen.add k
  assert seen == @["c", "d", "e"]
  t.del("d")
  assert not t.hasKey("d")
  t.clear()
  assert t.len == 0
  assert t.mgetOrPut("f") == 0
  t.mgetOrPut("f") = 8
  assert t.getOrQuit("f") == 8

block:
  let t = [("a", 1), ("b", 2), ("a", 3)].toOrderedTable
  assert t.len == 2
  assert t.getOrQuit("a") == 3
  assert t.getOrQuit("b") == 2
  assert $t == """{"a": 3, "b": 2}"""

  let same = [("a", 3), ("b", 2)].toOrderedTable
  let reordered = [("b", 2), ("a", 3)].toOrderedTable
  assert t == same
  assert t != reordered

  var sorted = [("c", 3), ("a", 1), ("b", 2)].toOrderedTable
  sorted.sort(pairCmp)
  assert sorted == [("a", 1), ("b", 2), ("c", 3)].toOrderedTable
  sorted.sort(pairCmp, SortOrder.Descending)
  assert sorted == [("c", 3), ("b", 2), ("a", 1)].toOrderedTable
  assert sorted.getOrQuit("b") == 2

  var large = [("f", 6), ("a", 1), ("e", 5), ("b", 2), ("d", 4), ("c", 3)].toOrderedTable
  large.sort(pairCmp)
  assert large == [("a", 1), ("b", 2), ("c", 3), ("d", 4), ("e", 5), ("f", 6)].toOrderedTable
  assert large.getOrQuit("e") == 5
  var largeSeen: seq[string] = @[]
  for k in large.keys:
    largeSeen.add k
  assert largeSeen == @["a", "b", "c", "d", "e", "f"]

block:
  let t = newOrderedTable[string, int](2)
  t["x"] = 1
  t["y"] = 2
  assert t.len == 2
  assert t.getOrDefault("x") == 1
  assert t.hasKey("y")
  assert not t.hasKey("z")
  assert not t.hasKeyOrPut("z", 3)
  assert t.getOrDefault("z") == 3

  var val = 0
  assert t.pop("x", val)
  assert val == 1
  t.del("y")
  assert t.len == 1
  for k, v in t.mpairs:
    v = v + 1
  assert t.getOrDefault("z") == 4
  t.clear()
  assert t.len == 0
  assert t.mgetOrPut("w") == 0
  t["w"] = 5
  assert t.getOrDefault("w") == 5

  let fromPairs = newOrderedTable([("a", 1), ("b", 2)])
  assert fromPairs.getOrDefault("a") == 1
  assert fromPairs.getOrDefault("b") == 2
  assert $fromPairs == """{"a": 1, "b": 2}"""
  assert fromPairs[] == [("a", 1), ("b", 2)].toOrderedTable

  let refSame = newOrderedTable([("a", 1), ("b", 2)])
  let refReordered = newOrderedTable([("b", 2), ("a", 1)])
  let refDifferent = newOrderedTable([("a", 1), ("b", 3)])
  assert fromPairs == refSame
  assert fromPairs != refReordered
  assert fromPairs != refDifferent

  let shared = refSame
  shared["c"] = 3
  assert refSame.getOrDefault("c") == 3

func intPairCmp(x, y: (int, int)): int =
  if x[0] < y[0]: -1
  elif x[0] > y[0]: 1
  else: 0

block:
  # Table equality is content based, not slot/insertion order based.
  var a = initTable[int, string]()
  var b = initTable[int, string]()
  a[1] = "one"
  a[2] = "two"
  a[3] = "three"
  b[3] = "three"
  b[1] = "one"
  b[2] = "two"
  assert a == b

  b[2] = "TWO"
  assert a != b

  let c = [("a", 1), ("b", 2), ("a", 3)].toTable
  assert c.len == 2
  assert c.getOrQuit("a") == 3
  assert c.getOrQuit("b") == 2

  # Equality also works once the open-addressed index is in use.
  var largeA = initTable[int, string]()
  var largeB = initTable[int, string]()
  for i in countdown(19, 0):
    largeA[i] = $i
  for i in 0 ..< 20:
    largeB[i] = $i
  assert largeA == largeB
  largeB[7] = "seven"
  assert largeA != largeB

  var d = initTable[string, int]()
  assert d.len == 0
  assert not d.hasKey("x")
  d["x"] = 1
  assert d.getOrQuit("x") == 1

block:
  # Deletion above the linear-scan threshold must rebuild the index and keep
  # later lookups and reinsertions consistent.
  var t = initTable[int, int]()
  for i in 0 ..< 200:
    t[i] = i * 3
  for i in 0 ..< 200:
    if i mod 2 == 0:
      t.del(i)
  assert t.len == 100
  for i in 0 ..< 200:
    if i mod 2 == 0:
      assert not t.hasKey(i)
    else:
      assert t.getOrDefault(i, -1) == i * 3
  for i in 0 ..< 200:
    if i mod 2 == 0:
      t[i] = -i
  for i in 0 ..< 200:
    assert t.hasKey(i)
    if i mod 2 == 0:
      assert t.getOrQuit(i) == -i
    else:
      assert t.getOrQuit(i) == i * 3

block:
  # A zero hash is normalized to a non-zero slot marker.
  var t = initTable[uint, int]()
  t[0'u] = 42
  assert t.hasKey(0'u)
  assert t.getOrQuit(0'u) == 42
  t.del(0'u)
  assert not t.hasKey(0'u)
  assert t.len == 0

block:
  # TableRef keeps reference semantics and compares contents.
  let a = newTable([(1, "one"), (2, "two")])
  assert a.getOrDefault(1) == "one"
  let b = a
  b[3] = "three"
  assert a.hasKey(3)
  assert a == b

  let c = newTable([(2, "two"), (1, "one")])
  assert a != c          # different contents
  let same = newTable([(3, "three"), (1, "one"), (2, "two")])
  assert a == same       # same contents, different insertion order
  same[1] = "ONE"
  assert a != same

  var val = ""
  assert a.pop(2, val)
  assert val == "two"
  assert not a.hasKey(2)
  assert a.len == 2
  a.clear()
  assert a.len == 0

block:
  # An ordered table keeps insertion order.
  var t = initOrderedTable[string, int]()
  t["b"] = 2
  t["a"] = 1
  var seen: seq[string] = @[]
  for k in t.keys:
    seen.add k
  assert seen == @["b", "a"]

  # Many entries exercise repeated enlarge and multi-pass merge sort.
  var large = initOrderedTable[int, int]()
  for i in countdown(99, 0):
    large[i] = i
  assert large.getOrQuit(50) == 50
  large.sort(intPairCmp)
  var expected = 0
  for k, v in large.pairs:
    assert k == expected
    assert v == expected
    inc expected
  assert expected == 100
  large.sort(intPairCmp, SortOrder.Descending)
  expected = 99
  for k, v in large.pairs:
    assert k == expected
    assert v == expected
    dec expected
  assert expected == -1

  large.del(50)
  assert not large.hasKey(50)
  assert large.getOrQuit(49) == 49
  assert large.getOrQuit(51) == 51
  assert large.len == 99

block:
  # CountTable counts, removes, queries and sorts.
  var t = initCountTable[string]()
  t.inc("a")
  t.inc("a")
  t.inc("b", 3)
  assert t["a"] == 2
  assert t["missing"] == 0
  assert t.len == 2
  assert t.hasKey("b")
  assert not t.hasKey("c")

  let big = t.largest()
  assert big[0] == "b"
  assert big[1] == 3
  let small = t.smallest()
  assert small[0] == "a"
  assert small[1] == 2

  t["b"] = 0
  assert not t.hasKey("b")
  assert t.len == 1
  t.del("a")
  assert t.len == 0

  let c = toCountTable(["x", "y", "x", "z", "x"])
  assert c["x"] == 3
  assert c["y"] == 1
  assert c["z"] == 1
  assert c.len == 3

  let refCounts = newCountTable(["p", "p", "q"])
  assert refCounts["p"] == 2
  assert refCounts["q"] == 1
  refCounts.inc("q")
  assert refCounts["q"] == 2
  let sameCounts = newCountTable(["q", "p", "p", "q"])
  assert refCounts == sameCounts

  var sorted = toCountTable(["low", "mid", "mid", "high", "high", "high"])
  sorted.sort(SortOrder.Ascending)
  var counts: seq[int] = @[]
  for k, v in sorted.pairs:
    counts.add v
  assert counts == @[1, 2, 3]

  var merged = toCountTable(["a", "b"])
  merged.merge(sorted)
  assert merged["a"] == 1
  assert merged["b"] == 1
  assert merged["low"] == 1
  assert merged["high"] == 3
