import std/[syncio]

template testCountdown(a, b, step: untyped) =
  echo a, ", ", b, ", ", step
  for i in countdown(a, b, step):
    echo i

for i in 1 .. 3:
  for j in -2 .. 3:
    testCountdown(j, 0, i)
    testCountdown(j, 1, i)

for i in 1 .. 3:
  for j in 0 .. 4:
    testCountdown(low(int) + j, low(int), i)
    testCountdown(low(int) + j, low(int) + 1, i)

for i in 1'u .. 3'u:
  for j in 0'u .. 3'u:
    testCountdown(j, 0'u, i)
    testCountdown(j, 1'u, i)
