type
  Foo[T] = object
  Bar[T] = object
    foo: Foo[T]

var bar: Bar[int]
