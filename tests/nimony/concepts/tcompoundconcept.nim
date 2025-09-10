import std/assertions

proc foo[T: SomeFloat and Comparable](x, y: T): bool = x == y

assert foo(1.0, 1.0)
assert not foo(1.0, 2.0)

proc bar[T: Comparable and HasDefault](x: T): bool =
  x == default(T)

assert bar(0.0)
assert bar("")
assert not bar(1.0)
assert not bar("abc")
