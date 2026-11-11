import std/syncio

# The program that evaluates a `const` (`exprexec`) is checked beside the
# declarations the evaluated code uses. The names that program introduces
# itself must not be ones the user's code can declare too, and the routines
# it calls (`std/writenif`'s, `system.len`, `system.[]`) must not be taken
# over by the user's routines of the same name.

type E = enum dest, other   # `dest` was the serializer's parameter

proc pair(): array[2, E] = [other, dest]

proc setup(p: string) = discard
proc teardown() = discard
proc writeNifInt(x: int) = discard
proc writeNifParLe(tag: string) = discard
proc len(s: seq[int]): int = 1      # shadows `system.len` for `seq[int]`

proc nums(): (int, int) =
  setup("x")
  teardown()
  writeNifInt(3)
  writeNifParLe("x")
  (1, 2)

proc items3(): seq[int] =
  result = @[5, 6, 7]
  discard len(result)

const
  ce = pair()
  cn = nums()
  cs = items3()

echo ce[0], " ", ce[1]
echo cn[0], " ", cn[1]
echo system.len(cs), " ", cs[0], cs[1], cs[2]
