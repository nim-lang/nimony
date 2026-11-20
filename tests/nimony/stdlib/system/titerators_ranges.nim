import std/[assertions, sequtils, syncio]

proc main =
  # ascending exclusive: [a, b)
  assert toSeq(3 ..< 7) == @[3, 4, 5, 6]
  assert toSeq(7 ..< 3) == @[]
  assert toSeq(5 ..< 5) == @[]

  # ascending inclusive: [a, b]
  assert toSeq(3 .. 7) == @[3, 4, 5, 6, 7]
  assert toSeq(7 .. 3) == @[]
  assert toSeq(5 .. 5) == @[5]

  # descending inclusive at both ends: [b, a] traversed downward
  assert toSeq(7 >.. 3) == @[7, 6, 5, 4, 3]
  assert toSeq(3 >.. 7) == @[]
  assert toSeq(5 >.. 5) == @[5]

  # stepped variants
  assert toSeq(countdown(9, 2, 3)) == @[9, 6, 3]
  assert toSeq(countup(2, 9, 3)) == @[2, 5, 8]

  # `>..` matches `countdown` for step 1
  assert toSeq(7 >.. 3) == toSeq(countdown(7, 3))

  # unsigned descending inclusive must stop at zero
  assert toSeq(3'u >.. 0'u) == @[3'u, 2'u, 1'u, 0'u]

  # over a `range` type, with and without a step (nimony-bugs-llms#160)
  type R = range[0..9]
  var s: seq[int] = @[]
  for i in countdown(R(9), R(0), 2): s.add i
  for i in countup(R(0), R(8), 2): s.add i
  for i in countdown(R(2), R(0)): s.add i
  for i in countup(R(7), R(9)): s.add i
  assert s == @[9, 7, 5, 3, 1, 0, 2, 4, 6, 8, 2, 1, 0, 7, 8, 9]

  # `items` of an enum whose `high` fills its storage: no step past `high`
  # (nimony-bugs-llms#122)
  type
    E1 = enum e253 = 253, e254, e255
    E2 = enum f40000 = 40000, f40001
    E4 = enum g0 = 2147483646, g1 = 2147483647
  var n = 0
  for v in E1: inc n
  for v in E2: inc n
  for v in E4: inc n
  assert n == 7

main()
