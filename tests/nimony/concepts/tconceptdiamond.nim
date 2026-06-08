import std/assertions

# Diamond: Base is reachable through both Left and Right.
type
  Base = concept
    proc `+`(a, b: Self): Self

  Left = concept of Base
    proc `-`(a, b: Self): Self

  Right = concept of Base
    proc `*`(a, b: Self): Self

  Diamond = concept of Left, Right

proc sumBase[T: Base](x, y: T): T =
  x + y

proc diffLeft[T: Left](x, y: T): T =
  x - y

proc prodRight[T: Right](x, y: T): T =
  x * y

proc allOps[T: Diamond](x, y: T): T =
  (x + y) * (x - y)

proc viaBase[T: Base](x, y: T): T =
  sumBase(x, y)

proc viaDiamond[T: Diamond](x, y: T): T =
  viaBase(x, y)

assert sumBase(2, 3) == 5
assert diffLeft(10, 4) == 6
assert prodRight(3, 4) == 12
assert allOps(5, 3) == 16
assert viaDiamond(1, 2) == 3
