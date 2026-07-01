import std/[assertions]

# `system` no longer defines a greedy 0-parameter `concat` template, so user
# code (e.g. `sequtils.concat`) is free to define its own `concat` overload
# without it being shadowed and failing overload resolution.

func concat[T](a, b: openArray[T]): seq[T] =
  result = @[]
  for i in 0 ..< a.len: result.add a[i]
  for i in 0 ..< b.len: result.add b[i]

proc main =
  assert concat(@[1, 2], @[3, 4]) == @[1, 2, 3, 4]
  assert concat(@["x"], @["y", "z"]) == @["x", "y", "z"]

main()
