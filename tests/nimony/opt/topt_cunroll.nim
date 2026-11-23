# Complete loop unrolling (`cunroll`) regression test.
# Covers:
# - vec4-style constant-index loop (unrolled, bounds check folded away),
# - trip-9 loop (above MaxTrips = 8, kept as a loop),
# - loop with `break` (escaping jump, kept as a loop),
# - loop whose counter is read after the loop (final value preserved),
# - loop whose unrolled form grows beyond token slack (rejected by trial-and-keep).

import std / syncio

proc vec4Sum(a: array[4, float32]): float32 {.noinline.} =
  var s = 0.0'f32
  for i in 0 ..< 4:
    s = s + a[i]
  result = s

proc vec4Checked(a: array[4, float32]): float32 {.noinline.} =
  var s = 0.0'f32
  for i in 0 ..< 4:
    if i >= 4: quit(1)
    s = s + a[i]
  result = s

proc trip9Sum(a: array[9, int]): int {.noinline.} =
  var s = 0
  for i in 0 ..< 9:
    s = s + a[i]
  result = s

proc findWithBreak(a: array[4, int]; target: int): int {.noinline.} =
  result = -1
  for i in 0 ..< 4:
    if a[i] == target:
      result = i
      break

proc sumCounterAfter(a: array[4, int]): int {.noinline.} =
  var i = 0
  var s = 0
  while i < 4:
    s = s + a[i]
    inc i
  result = s * 10 + i

proc loopThatGrows(dst: var array[8, int]; a, b: array[8, int]) {.noinline.} =
  for i in 0 ..< 8:
    dst[i] = (dst[i] + a[i]) * (b[i] - i) + (a[i] xor b[i])

let v = [1.0'f32, 2.0'f32, 3.0'f32, 4.0'f32]
echo vec4Sum(v)
echo vec4Checked(v)

let a9 = [1, 2, 3, 4, 5, 6, 7, 8, 9]
echo trip9Sum(a9)

let a4 = [10, 20, 30, 40]
echo findWithBreak(a4, 30), " ", findWithBreak(a4, 99)
echo sumCounterAfter(a4)

var g = [1, 2, 3, 4, 5, 6, 7, 8]
let ga = [2, 3, 4, 5, 6, 7, 8, 9]
let gb = [3, 4, 5, 6, 7, 8, 9, 10]
loopThatGrows(g, ga, gb)
echo g[0], " ", g[3], " ", g[7]
