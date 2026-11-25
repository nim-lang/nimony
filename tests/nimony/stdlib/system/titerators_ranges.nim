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

  # `countdown` with a step over `char`, an enum or `uint8`, and over a
  # `range` without 1 and no step
  type
    E = enum ea, eb, ec, ed, ee
    R5 = range[5..9]
  var cs = ""
  for c in countdown('k', 'a', 3): cs.add c
  assert cs == "kheb"
  assert toSeq(countdown(ee, ea, 2)) == @[ee, ec, ea]
  assert toSeq(countdown(10'u8, 0'u8, 3)) == @[10'u8, 7'u8, 4'u8, 1'u8]
  s = @[]
  for i in countdown(R5(9), R5(5)): s.add i
  assert s == @[9, 8, 7, 6, 5]

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
