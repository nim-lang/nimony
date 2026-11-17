# The inliner substitutes single-use arithmetic arguments at their use
# (`isPureValueArg`), and sem no longer appends an unreachable `return result`
# behind `return expr`, so a callee's result is built in the destination.
# Each case below must stay correct when an argument mentions the destination.
# An argument re-evaluated at its uses (`i + j`, `p[]`) must keep its value
# when the callee writes what it reads.
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

proc bumpThenAdd(z: int; p: ptr int): int {.inline.} =
  p[] = 100
  result = z + p[]

var ga = 5
var gb = 7
var gp: ptr int = addr ga

proc repointGlobal(z: int): int {.inline.} =
  {.cast(assumeSync).}:
    gp = addr gb
  result = z

proc repointLocal(z: int; pp: ptr ptr int; b: ptr int): int {.inline.} =
  pp[] = b
  result = z

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
  var i = 5
  var j = 1
  echo bumpThenAdd(i + j, addr i)     # `i + j` is 6 at the call
  {.cast(assumeSync).}:
    echo repointGlobal(gp[])          # `gp[]` is 5 at the call
  var a = 5
  var b = 7
  var p = addr a
  echo repointLocal(p[], addr p, addr b)

main()
