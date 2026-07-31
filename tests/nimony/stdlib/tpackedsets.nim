import std/[packedsets, assertions]

block: # basics over `int`
  var a = initPackedSet[int]()
  assert a.len == 0
  assert a.card == 0
  a.incl 3
  a.incl 3
  assert a.len == 1
  assert 3 in a
  assert 4 notin a
  assert a.contains(3)

  a.excl 3
  assert a.len == 0
  assert 3 notin a
  a.excl 99 # excluding something absent is a no-op
  assert a.len == 0

block: # sparse: values far apart, and many values in one block
  var a = initPackedSet[int]()
  for i in 5000 ..< 6000: a.incl i
  for i in 0 ..< 500: a.incl i
  for i in 500_000 ..< 500_010: a.incl i
  assert a.len == 1000 + 500 + 10
  for i in 5000 ..< 6000: assert i in a
  for i in 0 ..< 500: assert i in a
  for i in 500_000 ..< 500_010: assert i in a
  for i in 600 ..< 700: assert i notin a
  a.excl 5500
  assert 5500 notin a
  assert a.len == 1000 + 500 + 10 - 1

block: # negative ordinals round-trip
  var a = initPackedSet[int]()
  a.incl(-1)
  a.incl(-5000)
  a.incl 7
  assert -1 in a
  assert -5000 in a
  assert 7 in a
  assert -2 notin a
  assert a.len == 3
  var sum = 0
  for x in items(a): sum = sum + x
  assert sum == -1 + -5000 + 7

block: # containsOrIncl / missingOrExcl
  var a = initPackedSet[int]()
  assert a.containsOrIncl(3) == false
  assert a.containsOrIncl(3) == true
  assert a.containsOrIncl(4) == false
  assert a.len == 2

  assert a.missingOrExcl(3) == false
  assert a.missingOrExcl(3) == true
  assert a.missingOrExcl(99) == true
  assert a.len == 1

block: # toPackedSet, items, clear
  let a = toPackedSet([5, 6, 7, 8, 8])
  assert a.len == 4
  var seen = 0
  for x in items(a): seen = seen + x
  assert seen == 5 + 6 + 7 + 8

  var b = toPackedSet([5, 7])
  clear b
  assert b.len == 0
  assert 5 notin b

block: # value semantics: a copy is independent of its source
  var a = toPackedSet([1, 2, 3])
  var b = a
  b.incl 4
  b.excl 1
  assert a.len == 3
  assert 4 notin a
  assert 1 in a
  assert b.len == 3
  assert 4 in b

block: # set algebra
  let a = toPackedSet([1, 2, 3])
  let b = toPackedSet([3, 4, 5])
  assert union(a, b).len == 5
  assert (a + b) == toPackedSet([1, 2, 3, 4, 5])
  assert intersection(a, b) == toPackedSet([3])
  assert (a * b).len == 1
  assert difference(a, b) == toPackedSet([1, 2])
  assert (a - b).len == 2
  assert symmetricDifference(a, b) == toPackedSet([1, 2, 4, 5])

  var c = toPackedSet([1])
  c.incl toPackedSet([5])
  assert c.len == 2
  c.excl toPackedSet([5])
  assert c == toPackedSet([1])

block: # relations
  let a = toPackedSet([1])
  let b = toPackedSet([1, 2])
  let c = toPackedSet([1, 3])
  assert a <= b
  assert b <= b
  assert not (c <= b)
  assert a < b
  assert not (b < b)
  assert b == toPackedSet([2, 1, 2])
  assert not (b == c)
  assert disjoint(toPackedSet([1, 2]), toPackedSet([3, 4]))
  assert not disjoint(a, b)

block: # `$` needs `Stringable` on top of `Ordinal`
  assert $toPackedSet([1, 2, 3]) == "{1, 2, 3}"
  assert $initPackedSet[int]() == "{}"

block: # char elements
  var a = initPackedSet[char]()
  a.incl 'a'
  a.incl 'z'
  assert 'a' in a
  assert 'b' notin a
  assert a.len == 2
  # note: no `$` here -- `$`(char) lives in strutils, not in `system`, so it is
  # not in scope where the generic body binds it.

block: # enum elements
  type Color = enum
    red, green, blue
  var a = initPackedSet[Color]()
  a.incl green
  a.incl blue
  assert green in a
  assert red notin a
  assert a.len == 2
  assert $a == "{green, blue}"
  assert toPackedSet([red, blue]) == toPackedSet([blue, red])

block: # a distinct ordinal: no `hash`, no `$`, still a usable element type
  type FileId = distinct uint32
  func `==`(a, b: FileId): bool {.borrow.}

  var a = initPackedSet[FileId]()
  a.incl FileId(3'u32)
  a.incl FileId(70'u32)
  a.incl FileId(3'u32)
  assert FileId(3'u32) in a
  assert FileId(4'u32) notin a
  assert a.len == 2
  var total = 0'u32
  for x in items(a): total = total + uint32(x)
  assert total == 73'u32

block: # iteration order is reproducible: ascending within a trunk,
       # trunks in first-touched order
  var a = initPackedSet[int]()
  a.incl 5000
  a.incl 3
  a.incl 5001
  a.incl 1
  var order: seq[int] = @[]
  for x in items(a): order.add x
  assert order == @[5000, 5001, 1, 3]
