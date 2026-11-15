import std/syncio

# The program that evaluates a `const` (`exprexec`) is checked beside the
# declarations the evaluated code uses. The names that program introduces
# itself must not be ones the user's code can declare too, and `std/writenif`'s
# routines, which it calls, must not be taken over by the user's routines of
# the same name. (A container's `len` and `[]` are resolved where the
# program is checked, on purpose: any collection that has them works.)

type E = enum dest, other   # `dest` was the serializer's parameter

proc pair(): array[2, E] = [other, dest]

proc setup(p: string) = discard
proc teardown() = discard
proc writeNifInt(x: int) = discard
proc writeNifParLe(tag: string) = discard

proc nums(): (int, int) =
  setup("x")
  teardown()
  writeNifInt(3)
  writeNifParLe("x")
  (1, 2)

const
  ce = pair()
  cn = nums()

echo ce[0], " ", ce[1]
echo cn[0], " ", cn[1]
