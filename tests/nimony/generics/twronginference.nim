type Foo[T] = object

proc foo[T](x: Foo[T], y: T) = discard

proc main() =
  var x = Foo[int]()
  foo(x, x)
