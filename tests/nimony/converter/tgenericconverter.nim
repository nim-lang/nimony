type Foo[T] = object
  val: T

converter toFoo[T](x: T): Foo[T] = Foo[T](val: x)

proc bar[T](x: Foo[T], y: T) = discard

bar(123, 456)
let foo: Foo[int] = 123
