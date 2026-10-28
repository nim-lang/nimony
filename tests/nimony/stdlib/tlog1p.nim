# The native log1p (#2585) dropped musl's `f = x` in the k = 0 branch (1+x in
# [sqrt(2)/2, sqrt(2))), so log1p returned 0 for about -0.29 < x < 0.41, and
# arctanh / arcsinh / arccosh, which call it, returned 0 for small arguments.
# Expected values: glibc's log1p (within 1 ulp of the correctly rounded result).
import std/[syncio, math]

proc near(got, want: float64): bool =
  abs(got - want) <= 2.0 * abs(want) * 2.220446049250313e-16

const Cases = [
  (0.01, 0.009950330853168083), (-0.1, -0.10536051565782631), (0.4, 0.33647223662121295),
  (-0.28, -0.3285040669720361), (0.05, 0.04879016416943201), (0.42, 0.35065687161316933),
  (-0.3, -0.35667494393873234), (2.0, 1.0986122886681096), (1e-10, 9.999999999500001e-11)]

var bad = 0
for c in Cases:
  if not near(log1p(c[0]), c[1]):
    inc bad
    echo "FAIL log1p(", c[0], ") = ", log1p(c[0]), ", want ", c[1]
  let g32 = float64(log1p(float32(c[0])))
  if abs(g32 - c[1]) > 4.0 * abs(c[1]) * 1.1920928955078125e-07:
    inc bad
    echo "FAIL log1p(float32 ", c[0], ") = ", g32
if not near(arctanh(0.05), 0.05004172927849127):
  inc bad
  echo "FAIL arctanh(0.05) = ", arctanh(0.05)
if not near(arcsinh(0.3), 0.29567304756342244):
  inc bad
  echo "FAIL arcsinh(0.3) = ", arcsinh(0.3)
echo "log1p: ", Cases.len, " cases, ", bad, " failures"
