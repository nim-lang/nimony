import std/assertions

func test[T](x: T): T {.untyped.} =
  var v: T
  v = x
  result = v

assert test(1234) == 1234

type
  Foo = object
    x: int

proc test2[T](x: T): T {.untyped.} =
  var v: Foo
  v = x
  v

var f = Foo(x: 5678)
var g = test2(f)
assert g.x == 5678
