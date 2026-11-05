# `inc`/`dec`/`succ`/`pred` on a `range`-typed location owe the range like the
# assignment they stand for (nim-lang/nimony#2855). Before, the step was typed
# as the range itself and taken as its own proof, so the location silently
# left its range and an index from it lost its bounds check.

type D = object
  a: array[3, int]
  n: range[0..2]

proc incField(d: var D; k: int) =
  for i in 0 ..< k: inc d.n

proc decLocal(n: Natural): int =
  var i: Natural = n
  dec i
  result = i

proc predLocal(n: Natural): int =
  var i: Natural = n
  i = pred(i)
  result = i

proc incByUnknown(n: Natural; w: int): int =
  var i: Natural = n
  if i < 10: inc i, w
  result = i

var d = default(D)
incField(d, 3)
discard decLocal(0)
discard predLocal(0)
discard incByUnknown(0, -1)
