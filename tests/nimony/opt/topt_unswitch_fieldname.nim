# Loop unswitching copies the loop with its locals renamed; a field named like
# one of those locals keeps its own name.

import std/syncio

type O = object
  b: int

proc f(o: O; flag: bool; n: int): int =
  result = 0
  var i = 0
  while i < n:
    var b = o.b + i
    if flag:
      b = b + o.b
    result = result + b + O(b: b).b
    if flag:
      result = result + o.b
    i = i + 1

echo f(O(b: 3), true, 4)
echo f(O(b: 3), false, 4)

# The hoisted condition is canonicalized by replacing the loop's temps with
# their definitions; `o.b` must not be read as the temp `b`.
type P = object
  a, b: int

proc g(o: P; n: int): int =
  result = 0
  var i = 0
  while true:
    let b = o.a * 3
    if o.b > 0:
      result = result + b
    else:
      result = result - 1
    i = i + 1
    if i >= n: return result

echo g(P(a: 2, b: 5), 4)
echo g(P(a: 2, b: -5), 4)
