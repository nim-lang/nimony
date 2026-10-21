# A global may be left without an initializer when zero is a value of its
# type; it is zeroed then. Otherwise it needs one (errmsgs/tglobalneedsinit).

import std / syncio

type
  R = range[1..3]
  E = enum eA = 1, eB = 2
  Neg = enum nA = -1, nB = 0, nC = 1
  O = object
    r: R
    i: int = 7

proc pick(x: R): int =
  let a = [10, 20, 30]
  result = a[int(x) - 1]

var z: range[-5..5]
var n: Neg
var x: int
var g: R = default(R)
var e = eB
var o = O(r: 2)
var s {.noinit.}: R   # explicitly not initialized: allowed, and not read here

echo int(z)
echo ord(n)
echo x
echo pick(g)
echo ord(e)
echo int(o.r), " ", o.i
