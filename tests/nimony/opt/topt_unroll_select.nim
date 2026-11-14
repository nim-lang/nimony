# Counted loops whose counter selects a `const` table entry are unrolled by the
# inliner, each iteration's literal counter folding `table[i].field` to a
# constant and deciding the branches on it; a call on a literal selector is
# specialized the same way. These are the shapes that must keep their meaning
# when that happens: `break` and `continue` out of an unrolled iteration, a
# table entry copied into a local that is then written, a counter read after
# the loop, and loops that are not to be touched at all. Every result is printed.

import std / syncio

type
  Shape = object
    kind: int
    a, b: float64

const
  shapes = [
    Shape(kind: 0, a: 1.0, b: 2.0),
    Shape(kind: 1, a: 3.0, b: 4.0),
    Shape(kind: 1, a: 5.0, b: 6.0)
  ]
  weights = [3, 1, 4, 1]

proc area(i: int; scale: float64): float64 {.requires: i >= 0 and i < 3.} =
  # `i` selects `shapes[i]`: a literal argument decides the `kind` test
  let s = shapes[i]
  if s.kind == 1:
    result = s.a * s.b * scale
  else:
    result = (s.a + s.b) * scale

proc total(scale: float64): float64 =
  result = 0.0
  for i in 0 ..< 3:
    result = result + area(i, scale)

proc firstWide(scale: float64): int =
  # `break` out of the second iteration
  result = -1
  for i in 0 ..< 3:
    if area(i, scale) > 10.0:
      result = i
      break

proc skipPlanes(): float64 =
  # `continue` out of an iteration
  result = 0.0
  for i in 0 ..< 3:
    if shapes[i].kind == 0: continue
    result = result + shapes[i].a

proc mutatedCopy(): float64 =
  # the copy of a table entry is written: it is no constant any more
  result = 0.0
  for i in 0 ..< 3:
    var s = shapes[i]
    if s.kind == 1: s.a = s.a * 10.0
    result = result + s.a

proc weighted(x: int): int =
  result = 0
  for i in 0 ..< 4:
    result = result + weights[i] * x

proc counterAfter(): int =
  # the counter is read after the loop: not unrolled, same result either way
  var i = 0
  while i < 3:
    if shapes[i].kind == 1: break
    inc i
  result = i

proc notSelecting(n: int): int =
  # the counter selects nothing: an ordinary loop
  result = 0
  for i in 0 ..< 3:
    result = result + i * n

proc zeroed(): int =
  # a fill loop: the memory-idiom pass takes it before the inliner could unroll
  var a: array[3, int] = [7, 8, 9]
  for i in 0 ..< 3: a[i] = 0
  result = a[0] + a[1] + a[2]

echo total(2.0)
echo firstWide(1.0)
echo skipPlanes()
echo mutatedCopy()
echo weighted(2)
echo counterAfter()
echo notSelecting(5)
echo zeroed()
