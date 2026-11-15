# Loop variables that unpack, in a template or a generic body, where the
# iterator's type is known only at expansion or instantiation.
import std/syncio

template sumPairs(s: untyped): int =
  var total = 0
  for (a, b) in s:
    total += a * b
  total

template sumIndexed(s: untyped): int =
  var total = 0
  for i, (a, b) in s:
    total += i + a + b
  total

iterator twice[T](x: T): T =
  yield x
  yield x

proc sumTwice[T](x: T): int {.untyped.} =
  result = 0
  for (a, b) in twice(x):
    result += a + b
  for a, b in twice(x):
    result += a * b

proc firsts[T](s: seq[(T, T)]): seq[T] =
  result = @[]
  for (a, b) in s:
    result.add a

echo sumPairs([(1, 2), (3, 4)])
echo sumIndexed(@[(10, 20), (30, 40)])
echo sumTwice((2, 5))
let f = firsts(@[(7, 8), (9, 10)])
echo f[0], " ", f[1]
