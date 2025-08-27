import std/assertions

proc eq[T: object](a, b: T): bool =
  result = true
  for f1, f2 in fields(a, b):
    if f1 != f2:
      return false

# split for now since `T: tuple|object` does not match `T: tuple|object`:
proc eq[T: tuple](a, b: T): bool =
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

var tup = (x: "abc", y: 123, z: true)
var tup2 = ("def", 456, false)
assert eq(tup, tup)
assert not eq(tup, tup2)
assert not eq(tup2, tup)
assert eq(tup2, tup2)
