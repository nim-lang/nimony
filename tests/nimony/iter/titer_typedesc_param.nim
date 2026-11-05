import std/syncio

type
  Color = enum
    red, green, blue
  Dir = enum
    north, east, south

for c in Color:
  echo c

for c in items(Color):
  echo c

proc countDirs(): int =
  result = 0
  for d in Dir:
    inc result

echo countDirs()

iterator countAs[T](t: typedesc[T]; n: int): T =
  for i in 0 ..< n:
    yield T(i)

for x in countAs(int8, 2):
  echo x

iterator upTo(n: static int; start: int): int =
  for i in start .. n:
    yield i

for i in upTo(3, 1):
  echo i

for c in Color:
  for d in Dir:
    echo c, " ", d
