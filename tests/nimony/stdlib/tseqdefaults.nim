import std/[syncio, assertions]

type
  R = range[3 .. 7]
  E = enum eA = 3, eB = 4
  Trivial = object
    x: array[4, int]
  Mixed = object
    r: R
    xs: seq[int]

block:
  var s = newSeq[R](4)
  assert s.len == 4
  for x in s: assert x == low(R)
  s.setLen(6)
  for x in s: assert x == low(R)
  s.grow(8, R(5))
  for i in 0 ..< 6: assert s[i] == low(R)
  for i in 6 ..< 8: assert s[i] == R(5)
  let t = newSeqOf[R](3, R(6))
  for x in t: assert x == R(6)

block:
  var s = newSeq[E](3)
  for x in s: assert x == eA
  s.setLen(5)
  for x in s: assert x == eA
  let t = newSeqOf[E](3, eB)
  for x in t: assert x == eB

block:
  var ns = newSeq[int](2)
  ns.setLen(4)
  for x in ns: assert x == 0
  var bs = newSeq[bool](2)
  bs.setLen(4)
  for x in bs: assert not x
  var cs = newSeq[char](2)
  cs.setLen(4)
  for x in cs: assert x == '\0'
  var ps = newSeq[nil ptr int](2)
  ps.setLen(4)
  for x in ps: assert x == nil
  var rs = newSeq[nil ref int](2)
  rs.setLen(4)
  for x in rs: assert x == nil
  var cstrs = newSeq[nil cstring](2)
  cstrs.setLen(4)
  for x in cstrs: assert x == nil
  var points = newSeq[nil pointer](2)
  points.setLen(4)
  for x in points: assert x == nil
  var sets = newSeq[set[char]](2)
  sets.setLen(4)
  for x in sets: assert x == {}

block:
  var s = newSeq[Trivial](3)
  for x in s: assert x.x == [0, 0, 0, 0]
  s.setLen(5)
  for x in s: assert x.x == [0, 0, 0, 0]
  let t = newSeqOf[Trivial](2, Trivial(x: [1, 2, 3, 4]))
  for x in t: assert x.x == [1, 2, 3, 4]

block:
  var s = newSeq[Mixed](2)
  for x in s:
    assert x.r == low(R)
    assert x.xs.len == 0
  s.setLen(4)
  for x in s:
    assert x.r == low(R)
    assert x.xs.len == 0
  let a = newSeq[array[2, seq[int]]](2)
  for x in a:
    for y in x: assert y.len == 0
  let r = newSeq[array[2, R]](2)
  for x in r:
    for y in x: assert y == low(R)

block:
  var arr = [1, 2, 3, 4]
  let s = @arr
  assert s == @[1, 2, 3, 4]
  let d = s
  assert d == s
  var c = @[9]
  c = s
  assert c == s

block:
  var s = newSeq[string](2)
  for x in s: assert x == ""
  s.grow(4, "x")
  for i in 0 ..< 2: assert s[i] == ""
  for i in 2 ..< 4: assert s[i] == "x"
  s.setLen(6)
  for i in 4 ..< 6: assert s[i] == ""
  let t = newSeqOf[string](3, "a")
  for x in t: assert x == "a"
  let u = @["a", "b"]
  assert u == @["a", "b"]
  shrink(s, 1)
  assert s.len == 1 and s[0] == ""

block:
  var s = newSeq[seq[int]](2)
  for x in s: assert x.len == 0
  s.grow(4, @[1, 2])
  for i in 0 ..< 2: assert s[i].len == 0
  for i in 2 ..< 4: assert s[i] == @[1, 2]
  s.setLen(6)
  for i in 4 ..< 6: assert s[i].len == 0
  let t = newSeqOf[seq[int]](3, @[3])
  for x in t: assert x == @[3]
  let u = @[@[1], @[2]]
  assert u == @[@[1], @[2]]

block:
  var s = @[1, 2, 3, 4, 5]
  del(s, 1)
  assert s == @[1, 5, 3, 4]
  delete(s, 2)
  assert s == @[1, 5, 4]
  shrink(s, 1)
  assert s == @[1]

echo "ok"
