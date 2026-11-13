import std/[assertions, critbits, sequtils, syncio]

proc main() {.raises.} =
  var s = default(CritBitTree[void])
  assert s.len == 0
  assert $s == "{}"
  assert not s.contains("abc")
  assert not s.hasKey("abc")
  assert "abc" notin s

  s.incl "abc"
  s.incl "xyz"
  s.incl "def"
  s.incl "definition"
  s.incl "prefix"
  s.incl "foo"
  assert s.len == 6
  assert "def" in s
  assert toSeq(s.keys) == @["abc", "def", "definition", "foo", "prefix", "xyz"]
  assert toSeq(s.items) == toSeq(s.keys)
  assert $s == """{"abc", "def", "definition", "foo", "prefix", "xyz"}"""

  s.incl ""
  assert "" in s
  assert s.len == 7
  assert toSeq(s.keysWithPrefix("")) == toSeq(s.keys)

  s.excl "def"
  assert "def" notin s
  assert not s.missingOrExcl("foo")
  assert "foo" notin s
  assert s.missingOrExcl("foo")
  assert toSeq(s.items) == @["", "abc", "definition", "prefix", "xyz"]
  assert toSeq(s.itemsWithPrefix("de")) == @["definition"]
  assert toSeq(s.keysWithPrefix("no such prefix")).len == 0

  assert not s.containsOrIncl("new")
  assert s.contains("new")
  assert s.containsOrIncl("new")
  assert s.containsOrIncl("")
  assert $s == """{"", "abc", "definition", "new", "prefix", "xyz"}"""

  var m = default(CritBitTree[int])
  assert m.len == 0
  assert $m == "{:}"
  m["b"] = 2
  m.incl("a", 1)
  m["c"] = 3
  assert m.len == 3
  assert m["a"] == 1
  assert m["b"] == 2
  assert m["c"] == 3
  assert toSeq(m.keys) == @["a", "b", "c"]
  assert toSeq(m.values) == @[1, 2, 3]

  assert m.containsOrIncl("a", 99)
  assert m["a"] == 1
  assert not m.containsOrIncl("d", 4)
  assert m["d"] == 4

  m.inc("a")
  assert m["a"] == 2
  m.inc("a", 4)
  assert m["a"] == 6
  m.inc("a", -6)
  assert m["a"] == 0
  m.incl("a", 10)
  assert m["a"] == 10
  m["a"] = 11
  assert m["a"] == 11
  m["a"] += 1
  assert m["a"] == 12

  var pairKeys: seq[string] = @[]
  var pairVals: seq[int] = @[]
  for k, v in m.pairs:
    pairKeys.add k
    pairVals.add v
  assert pairKeys == @["a", "b", "c", "d"]
  assert pairVals == @[12, 2, 3, 4]

  for v in m.mvalues:
    v = v * 10
  assert toSeq(m.values) == @[120, 20, 30, 40]
  for k, v in m.mpairs:
    v = v + 1
    assert m[k] == v
  assert toSeq(m.values) == @[121, 21, 31, 41]

  var raised = false
  try:
    discard m["missing"]
  except ErrorCode as e:
    raised = e == KeyError
  assert raised

  m.excl "b"
  assert not m.hasKey("b")
  assert m.len == 3
  m.excl "b"
  assert m.len == 3

  var p = default(CritBitTree[int])
  p["a"] = 1
  p["ab"] = 2
  p["abc"] = 3
  p["b"] = 4
  assert toSeq(p.keysWithPrefix("ab")) == @["ab", "abc"]
  assert toSeq(p.itemsWithPrefix("ab")) == @["ab", "abc"]
  assert toSeq(p.valuesWithPrefix("ab")) == @[2, 3]
  assert toSeq(p.pairsWithPrefix("ab")).len == 2
  for v in p.mvaluesWithPrefix("ab"):
    v = v * 10
  assert p["ab"] == 20
  assert p["abc"] == 30
  for k, v in p.mpairsWithPrefix("ab"):
    v = v + 1
    assert p[k] == v
  assert p["ab"] == 21
  assert p["abc"] == 31
  for k in p.keysWithPrefix("zzz"):
    assert false, k

  var c = default(CritBitTree[void])
  assert c.commonPrefixLen == 0
  c.incl "key1"
  assert c.commonPrefixLen == 4
  c.incl "key2"
  assert c.commonPrefixLen == 3

  let set = ["b", "a", "b"].toCritBitTree
  assert set.len == 2
  assert toSeq(set.keys) == @["a", "b"]

  let map = [("b", 2), ("a", 1)].toCritBitTree
  assert map.len == 2
  assert map["a"] == 1
  assert map["b"] == 2

  let duplicate = [("a", 1), ("a", 2)].toCritBitTree
  assert duplicate.len == 1
  assert duplicate["a"] == 2

  var ti = default(CritBitTree[int])
  ti["a"] = 1
  assert $ti == """{"a": 1}"""
  var ts = default(CritBitTree[string])
  ts["a"] = "1"
  assert $ts == """{"a": "1"}"""
  var tc = default(CritBitTree[char])
  tc["a"] = '1'
  assert $tc == """{"a": '1'}"""

  var big = default(CritBitTree[int])
  for i in 0 ..< 64:
    big["k" & $i] = i
  assert big.len == 64
  for i in 0 ..< 64:
    assert big["k" & $i] == i
  for i in 0 ..< 64:
    big.excl("k" & $i)
  assert big.len == 0
  assert $big == "{:}"

  echo "critbits: OK"

try:
  main()
except ErrorCode:
  assert false
