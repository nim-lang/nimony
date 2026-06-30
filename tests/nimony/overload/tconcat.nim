import std/[assertions]

# `system.concat` must accept string arguments without shadowing a user-defined
# overload for other argument types. Previously `concat` was a bare 0-parameter
# `{.varargs.}` template that matched *every* call and then failed in its body.

func concat[T](a, b: openArray[T]): seq[T] =
  result = @[]
  for i in 0 ..< a.len: result.add a[i]
  for i in 0 ..< b.len: result.add b[i]

proc main =
  # system overload (strings)
  assert concat("foo", "bar") == "foobar"
  assert concat("a", "b", "c") == "abc"
  assert concat() == ""
  # user overload (seqs) coexists
  assert concat(@[1, 2], @[3, 4]) == @[1, 2, 3, 4]
  assert concat(@["x"], @["y", "z"]) == @["x", "y", "z"]

main()
