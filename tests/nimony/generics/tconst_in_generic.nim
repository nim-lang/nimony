import std/syncio

proc almostEqual[T: float](a, b: T): bool {.untyped.} =
  const eps = 0.0001
  abs(a - b) < eps

echo almostEqual(1.0, 1.00005)
echo almostEqual(1.0, 2.0)
