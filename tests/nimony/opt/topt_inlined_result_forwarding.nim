# The inliner substitutes single-use arithmetic arguments at their use
# (`isPureValueArg`), and sem no longer appends an unreachable `return result`
# behind `return expr`, so a callee's result is built in the destination.
# Each case below must stay correct when an argument mentions the destination.
import std/syncio

type
  V = object
    x, y, z: float64

proc vec(x, y, z: float64): V {.inline.} = V(x: x, y: y, z: z)
proc mul(a, b: V): V {.inline.} = vec(a.x * b.x, a.y * b.y, a.z * b.z)
proc add(a, b: V): V {.inline.} = vec(a.x + b.x, a.y + b.y, a.z + b.z)
proc swapXY(a: V): V {.inline.} = vec(a.y, a.x, a.z)

proc clampedNeg(a: V): V {.inline.} =
  # An early return: the result is not written last on every path.
  if a.x < 0.0:
    return a
  result = vec(-a.x, -a.y, -a.z)

proc twice(x: float64): float64 {.inline.} =
  # The argument is used inside a loop: it must stay bound, not substituted.
  result = 0.0
  for i in 0 ..< 2:
    result = result + x

proc main =
  var c = vec(1.0, 2.0, 3.0)
  let d = vec(2.0, 3.0, 4.0)
  c = mul(c, d)                       # an argument mentions the destination
  echo c.x, " ", c.y, " ", c.z
  c = add(add(c, d), c)               # nested, both mention it
  echo c.x, " ", c.y, " ", c.z
  c = swapXY(c)                       # reads the field the other one writes
  echo c.x, " ", c.y, " ", c.z
  c = clampedNeg(c)
  echo c.x, " ", c.y, " ", c.z
  c = clampedNeg(c)
  echo c.x, " ", c.y, " ", c.z
  var s = 1.5
  s = twice(s - 0.5)
  echo s

main()
