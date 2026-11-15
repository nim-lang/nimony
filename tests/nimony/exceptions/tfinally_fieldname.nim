# A `finally` body is copied once per exit with its locals renamed; a field
# named like one of those locals keeps its own name.

import std/syncio

type
  O = object
    b: int
  R = ref object
    b: int

proc g(fail: bool): int {.raises.} =
  if fail: raise Failure
  result = 5

proc f(o: O; r: R; fail: bool) {.raises.} =
  try:
    discard g(fail)
  finally:
    let b = o.b + r.b
    let c = O(b: b)
    echo b, " ", c.b + 1

try:
  f(O(b: 41), R(b: 1), false)
  f(O(b: 1), R(b: 2), true)
except:
  echo "raised"
