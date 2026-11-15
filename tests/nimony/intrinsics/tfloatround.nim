# `Floor`/`Ceil`/`Trunc`/`Round`: one instruction on AArch64 (`frint*`) and on
# native x86-64 (`roundsd`); `round` there is built from `Trunc`. Every value is
# a runtime operand (`noinline` callers), and the sign of a zero result is
# printed explicitly because `$` does not show it.
import std/[math, syncio]

proc sgnBit(x: float64): int = int(cast[uint64](x) shr 63)
proc sgnBit32(x: float32): int = int(cast[uint32](x) shr 31)

proc all4(x: float64): string {.noinline.} =
  let a = floor(x)
  let b = ceil(x)
  let c = trunc(x)
  let d = round(x)
  result = $a & " " & $b & " " & $c & " " & $d & " | " &
           $sgnBit(a) & $sgnBit(b) & $sgnBit(c) & $sgnBit(d)

proc all4f(x: float32): string {.noinline.} =
  let a = floor(x)
  let b = ceil(x)
  let c = trunc(x)
  let d = round(x)
  result = $a & " " & $b & " " & $c & " " & $d & " | " &
           $sgnBit32(a) & $sgnBit32(b) & $sgnBit32(c) & $sgnBit32(d)

proc checker(x, z: float64): int {.noinline.} =
  # the raytracer's checkerboard: two floors feeding an int conversion
  int(floor(z) + floor(x))

proc toByte(v: float64): int {.noinline.} =
  int(round(v * 255.0)) mod 256

proc sumFloors(n: int): float64 {.noinline.} =
  # several rounding results live at once, across a loop
  result = 0.0
  var x = -3.75
  for i in 0 ..< n:
    result = result + floor(x) * 2.0 + ceil(x) - trunc(x) + round(x)
    x = x + 0.5

for x in [0.0, -0.0, 0.5, -0.5, 1.5, -1.5, 2.5, -2.5, 2.4999999999999996,
          0.49999999999999994, 7.25, -7.25, 1e300, -1e300, 4503599627370497.0]:
  echo all4(x)
echo all4(Inf), " ", all4(-Inf)
echo floor(NaN).isNaN, " ", ceil(NaN).isNaN, " ", trunc(NaN).isNaN, " ", round(NaN).isNaN
for x in [0.5'f32, -0.5'f32, 2.5'f32, -2.5'f32, 3.75'f32, -3.75'f32, 16777215.0'f32]:
  echo all4f(x)
echo checker(-0.5, 2.5), " ", checker(3.9, -1.1), " ", toByte(0.5), " ", toByte(1.0)
echo sumFloors(16)
