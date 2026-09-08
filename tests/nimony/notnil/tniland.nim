
# Issue #1946-adapted: nil-tracking narrows through an `and` short-circuit too.
import std / syncio

type Box = ref object
  v: int

proc getOpt(x: int): nil Box =
  result = nil
  if x > 0: result = Box(v: x)

proc use(a, b: Box) =
  echo a.v + b.v

proc bug(x, y: int) =
  let a = getOpt(x)
  let b = getOpt(y)
  if a != nil and b != nil:
    use(a, b)

bug(1, 2)
bug(0, 5)
bug(7, 4)
