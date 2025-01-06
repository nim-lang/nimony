type
  Foo[T] = object
    x: ref Foo[T]

var foo: Foo[int]
