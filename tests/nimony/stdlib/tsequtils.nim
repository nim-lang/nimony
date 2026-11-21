import std/[syncio, assertions, sequtils]

type
  RefBox = ref object
    value: int
  FixedBox = ref object
    value: int
  Fixed = FixedBox not nil
  Wide = array[32, int]
  HookBox64 = object
    a: array[7, int]
    r: RefBox

proc newFixedBox(value: int): Fixed = FixedBox(value: value)

proc newHookBox64(value: int): HookBox64 =
  var a = default(array[7, int])
  a[0] = value
  a[6] = value + 1
  result = HookBox64(a: a, r: RefBox(value: value))

proc isEven(x: int): bool = x mod 2 == 0
proc dbl(x: int): int = x * 2
proc isShort(x: string): bool = x.len <= 4
proc keepAllText(x: string): bool = x.len >= 0
proc keepNoneText(x: string): bool = x.len < 0
proc textLen(x: string): int = x.len

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

proc keepText =
  var data = @["keep", "drop", "keep2", "drop2"]
  keepIf(data, isShort)
  assert data == @["keep", "drop"]
  var all = @["a", "bb"]
  keepIf(all, keepAllText)
  assert all == @["a", "bb"]
  var none = @["a", "bb"]
  keepIf(none, keepNoneText)
  assert none.len == 0

  var kdata = @["a", "bb", "c", "dd"]
  kdata.keepItIf(it.len == 1)
  assert kdata == @["a", "c"]
  kdata.keepItIf(false)
  assert kdata.len == 0
  kdata = @["a", "bb"]
  kdata.keepItIf(true)
  assert kdata == @["a", "bb"]

proc main =
  assert repeat(7, 3) == @[7, 7, 7]
  assert repeat(7, 7) == @[7, 7, 7, 7, 7, 7, 7]
  assert repeat(7, 0).len == 0
  assert repeat("a", 2) == @["a", "a"]
  assert repeat("a", 0).len == 0
  # `RefBox` has hooks, so this exercises hook-aware insertion.
  let boxes = repeat(RefBox(value: 3), 3)
  assert boxes.len == 3
  assert boxes[0].value == 3
  assert boxes[2].value == 3
  # A not-nil ref has no default value, so the constructors must not require
  # `HasDefault`; they build the result by appending.
  let fixed: Fixed = newFixedBox(4)
  let fixedRepeated: seq[Fixed] = repeat(fixed, 2)
  assert fixedRepeated.len == 2
  assert fixedRepeated[1].value == 4
  let fixedMapped: seq[Fixed] = map(@[1, 2], newFixedBox)
  assert fixedMapped[0].value == 1
  let fixedWith: seq[Fixed] = newSeqWith(2, newFixedBox(5))
  assert fixedWith[1].value == 5
  let fixedConcat: seq[Fixed] = concat(fixedRepeated, fixedWith)
  assert fixedConcat.len == 4
  assert fixedConcat[3].value == 5
  let fixedCycle: seq[Fixed] = cycle(fixedRepeated, 2)
  assert fixedCycle.len == 4
  assert fixedCycle[3].value == 4
  # Hook-bearing elements go through the normal insertion path.
  let hookA = newHookBox64(10)
  let hookB = newHookBox64(20)
  let hookConcat = concat(@[hookA], @[hookB])
  assert hookConcat.len == 2
  assert hookConcat[0].a[0] == 10
  assert hookConcat[1].r.value == 20
  let hookCycle = cycle(@[hookA], 2)
  assert hookCycle.len == 2
  assert hookCycle[1].a[6] == 11
  assert concat(newSeqOfCap[HookBox64](0), @[hookB])[0].r.value == 20
  assert cycle(newSeqOfCap[HookBox64](0), 3).len == 0
  # Large trivial elements need no special path.
  var wide = default(Wide)
  wide[0] = 7
  wide[31] = 9
  let wides = repeat(wide, 3)
  assert wides.len == 3
  assert wides[0][0] == 7
  assert wides[2][31] == 9
  assert concat(@[1, 2], @[3, 4]) == @[1, 2, 3, 4]
  assert concat(newSeq[int](0), @[1, 2]) == @[1, 2]
  assert concat(@[1, 2], newSeq[int](0)) == @[1, 2]
  assert concat(@["a"], @["b"]) == @["a", "b"]
  assert map(@["a", "bb"], textLen) == @[1, 2]
  assert map(newSeq[int](0), dbl).len == 0
  assert cycle(@["a", "b"], 2) == @["a", "b", "a", "b"]
  assert cycle(@[1, 2], 0).len == 0
  assert cycle(newSeq[int](0), 3).len == 0
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
  assert zip(@[1, 2], @["a", "b"]) == @[(1, "a"), (2, "b")]
  assert zip(newSeq[int](0), @[1, 2]).len == 0
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
  assert mapIt(newSeq[int](0), it + 1).len == 0
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
  assert newSeqWith(0, 7).len == 0
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
keepText()
