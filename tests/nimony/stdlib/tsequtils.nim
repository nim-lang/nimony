import std/[syncio, assertions, sequtils]

proc isEven(x: int): bool = x mod 2 == 0
proc dbl(x: int): int = x * 2

proc main =
  assert repeat(7, 3) == @[7, 7, 7]
  assert count(@[1, 2, 2, 3, 2], 2) == 3
  assert find(@[10, 20, 30], 20) == 1
  assert find(@[10, 20, 30], 99) == -1
  assert contains(@[1, 2, 3], 2)
  assert not contains(@[1, 2, 3], 9)
  assert deduplicate(@[1, 2, 2, 3, 1]) == @[1, 2, 3]
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

main()
