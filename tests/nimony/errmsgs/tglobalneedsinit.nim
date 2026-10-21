# A global whose type does not have zero as a value needs an initializer:
# its storage would be zeroed, and the contracts engine trusts the type.
# A field's declared default does not count (`o`): the storage is zeroed.

type
  R = range[1..3]
  E = enum eA = 1, eB = 2
  Holes = enum hA = 1, hB = 5
  D = distinct R
  O = object
    r: R = 2
    i: int = 7
  Foo[T] = object
    x: T
  Base = object of RootObj
    r: R
  Derived = object of Base
    n: int

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
