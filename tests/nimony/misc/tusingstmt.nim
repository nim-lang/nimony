type
  Foo = object
    discard

using
  x: Foo
  y: array[2, int]

proc test(x; y) =
  discard

proc test2(x; y: array[2, int]) =
  discard

proc test3(x = Foo(); y) =
  discard

var f = Foo()
test f, [1, 2]
test2 f, [1, 2]
test3 f, [1, 2]
