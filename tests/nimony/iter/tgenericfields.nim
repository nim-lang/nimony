import std/syncio

proc eq[T: object](a, b: T): bool =
  result = true
  for f1, f2 in fields(a, b):
    if f1 != f2:
      return false

type Obj = object
  a, b: int
  c: cstring

var o = Obj(a: 1, b: 2, c: "xyz")
var o2 = Obj(a: 3, b: 4, c: "uvw")
assert eq(o, o)
assert not eq(o, o2)
assert not eq(o2, o)
assert eq(o2, o2)
