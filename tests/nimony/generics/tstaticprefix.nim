import std/syncio

# The bracket-less `static T` (nifler: `(cmd static T)`) is the same
# declaration syntax as `static[T]`, for constant parameters and for value
# generic parameters alike.

proc pick(backward: static bool): int =
  result = if backward: 1 else: 0
echo pick(true)
echo pick(false)

proc len2[N: static int](a: array[N, int]): int = N
var a: array[3, int]
echo len2(a)
