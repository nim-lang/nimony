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

  # descending inclusive: [b, a] traversed downward
  assert toSeq(7 >.. 3) == @[7, 6, 5, 4, 3]
  assert toSeq(3 >.. 7) == @[]
  assert toSeq(5 >.. 5) == @[5]

  # descending exclusive: (b, a] traversed downward
  assert toSeq(7 >..< 3) == @[7, 6, 5, 4]
  assert toSeq(3 >..< 7) == @[]
  assert toSeq(5 >..< 5) == @[]
  assert toSeq(4 >..< 3) == @[4]

  # stepped variants
  assert toSeq(countdown(9, 2, 3)) == @[9, 6, 3]
  assert toSeq(countup(2, 9, 3)) == @[2, 5, 8]

  # `>..` matches `countdown` for step 1
  assert toSeq(7 >.. 3) == toSeq(countdown(7, 3))

  # unsigned descending inclusive must stop at zero
  assert toSeq(3'u >.. 0'u) == @[3'u, 2'u, 1'u, 0'u]

main()
