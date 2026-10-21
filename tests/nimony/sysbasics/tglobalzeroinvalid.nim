# A global without an initializer is not zeroed when zero is not a value of
# its type: it starts as `default(T)` then. Zero is kept where it is a value.

import std / syncio

type
  R = range[1..3]
  E = enum eA = 1, eB = 2
  Holes = enum hA = 1, hB = 5
  Neg = enum nA = -1, nB = 0, nC = 1
  D = distinct R
  O = object
    r: R
    i: int = 7
  Foo[T] = object
    x: T
  Base = object of RootObj
    r: R
  Derived = object of Base
    n: int

proc pick(x: R): int =
  let a = [10, 20, 30]
  result = a[int(x) - 1]

var g: R
var p: Positive
var e: E
var h: Holes
var d: D
var o: O
var arr: array[3, R]
var t: (int, R)
var foo: Foo[R]
var der: Derived
var z: range[-5..5]
var n: Neg

echo pick(g)
echo int(p)
echo ord(e)
echo ord(h)
echo int(d)
echo int(o.r)
echo int(arr[0]), int(arr[1]), int(arr[2])
echo t[0], " ", int(t[1])
echo int(foo.x)
echo int(der.r), " ", der.n
echo int(z)
echo ord(n)
