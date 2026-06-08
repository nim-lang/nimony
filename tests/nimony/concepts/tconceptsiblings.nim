import std/assertions

# Two independent parents (no shared ancestor), each requiring the same proc.
type
  Addable = concept
    proc `+`(a, b: Self): Self

  Summable = concept
    proc `+`(a, b: Self): Self

  Both = concept of Addable, Summable

proc sumAddable[T: Addable](x, y: T): T =
  x + y

proc sumSummable[T: Summable](x, y: T): T =
  x + y

proc sumBoth[T: Both](x, y: T): T =
  x + y

proc nested[T: Both](x, y, z: T): T =
  sumAddable(sumSummable(x, y), z)

assert sumAddable(1, 2) == 3
assert sumSummable(4, 5) == 9
assert sumBoth(10, 7) == 17
assert nested(1, 2, 3) == 6
