import std/[syncio, assertions, sequtils]

proc isEven(x: int): bool = x mod 2 == 0
proc dbl(x: int): int = x * 2

proc getSeq(c: var int): seq[int] =
  inc c
  result = @[1, 2, 3, 4]

proc evalOnce =
  # #2741: the templates evaluate their collection argument once, and before
  # they inject `it`/`a`/`b`, so a caller's own `a` is not captured.
  let a = @[1, 2, 3, 4]
  assert a.foldl(a + b) == 10
  assert a.foldr(a + b) == 10
  let it = @[1, 2, 3]
  assert it.mapIt(it * 2) == @[2, 4, 6]
  assert it.filterIt(it > 1) == @[2, 3]
  assert it.anyIt(it == 3)
  assert it.allIt(it > 0)
  assert it.countIt(it > 1) == 2

  var calls = 0
  assert getSeq(calls).foldl(a + b) == 10
  assert getSeq(calls).foldr(a + b) == 10
  assert getSeq(calls).mapIt(it + 1) == @[2, 3, 4, 5]
  assert getSeq(calls).filterIt(it mod 2 == 0) == @[2, 4]
  assert getSeq(calls).anyIt(it == 4)
  assert getSeq(calls).allIt(it > 0)
  assert getSeq(calls).countIt(it > 2) == 2
  assert calls == 7

proc main =
  assert repeat(7, 3) == @[7, 7, 7]
  assert concat(@[1, 2], @[3, 4]) == @[1, 2, 3, 4]
  assert count(@[1, 2, 2, 3, 2], 2) == 3
  assert find(@[10, 20, 30], 20) == 1
  assert find(@[10, 20, 30], 99) == -1
  assert contains(@[1, 2, 3], 2)
  assert not contains(@[1, 2, 3], 9)
  assert deduplicate(@[1, 2, 2, 3, 1]) == @[1, 2, 3]
  assert deduplicate(@[1, 1, 2, 3, 3], isSorted = true) == @[1, 2, 3]
  assert deduplicate(newSeq[int](0), isSorted = true).len == 0
  assert minIndex(@[3, 1, 2]) == 1
  assert maxIndex(@[3, 1, 2]) == 0
  assert map(@[1, 2, 3], dbl) == @[2, 4, 6]
  assert filter(@[1, 2, 3, 4], isEven) == @[2, 4]
  assert all(@[2, 4, 6], isEven)
  assert not all(@[2, 3, 6], isEven)
  assert any(@[1, 3, 4], isEven)
  assert not any(@[1, 3, 5], isEven)
  var data = @[1, 2, 3, 4, 5]
  keepIf(data, isEven)
  assert data == @[2, 4]
  apply(data, dbl)
  assert data == @[4, 8]
  assert cycle(@[1, 2], 3) == @[1, 2, 1, 2, 1, 2]
  assert zip(@[1, 2, 3], @[10, 20]) == @[(1, 10), (2, 20)]
  let (xs, ys) = unzip(@[(1, 10), (2, 20)])
  assert xs == @[1, 2]
  assert ys == @[10, 20]
  # `it` / fold templates
  assert foldl(@[1, 2, 3, 4], a + b) == 10
  assert foldr(@[1, 2, 3, 4], a + b) == 10
  assert anyIt(@[1, 3, 4], it mod 2 == 0)
  assert not anyIt(@[1, 3, 5], it mod 2 == 0)
  assert allIt(@[2, 4, 6], it mod 2 == 0)
  assert not allIt(@[2, 3], it mod 2 == 0)
  assert countIt(@[1, 2, 3, 4, 6], it mod 2 == 0) == 3
  assert mapIt(@[1, 2, 3], it * 10) == @[10, 20, 30]
  assert mapIt(@[1, 2, 3], $it) == @["1", "2", "3"]
  assert filterIt(@[1, 2, 3, 4], it mod 2 == 0) == @[2, 4]
  var kdata = @[1, 2, 3, 4, 5]
  keepItIf(kdata, it mod 2 == 1)
  assert kdata == @[1, 3, 5]
  # minmax / addUnique / delete / insert / applyIt / newSeqWith
  assert minmax(@[3, 1, 2, 5, 4]) == (1, 5)
  var au = @[1, 2, 3]
  au.addUnique(4)
  au.addUnique(4)
  assert au == @[1, 2, 3, 4]
  var ddata = @[10, 11, 12, 13, 14]
  ddata.delete(1, 2)
  assert ddata == @[10, 13, 14]
  var idata = @[1, 1, 1]
  idata.insert(@[2, 2], 1)
  assert idata == @[1, 2, 2, 1, 1]
  var nums = @[1, 2, 3, 4]
  nums.applyIt(it * 3)
  assert nums == @[3, 6, 9, 12]
  let nsw1: seq[int] = newSeqWith(3, 7)
  assert nsw1 == @[7, 7, 7]
  let nsw2: seq[seq[int]] = newSeqWith(2, newSeq[int](0))
  assert nsw2.len == 2
  # toSeq: collections, ranges, strings
  assert toSeq(@[1, 2, 3]) == @[1, 2, 3]
  assert toSeq([10, 20, 30]) == @[10, 20, 30]
  assert toSeq(1 .. 4) == @[1, 2, 3, 4]
  assert toSeq('a' .. 'c') == @['a', 'b', 'c']
  let chars = toSeq("ab")
  assert chars == @['a', 'b']

main()
evalOnce()
