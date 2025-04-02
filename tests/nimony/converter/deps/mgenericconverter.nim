type Foo*[T] = object
  val: T

converter toFoo*[T](x: T): Foo[T] = Foo[T](val: x)

