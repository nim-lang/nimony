type
  Foo[T] = object
  Bar* = object
    y: Foo[int]

proc foo[T](): Foo[T] = discard

proc initBar*[T]() =
  discard Bar(y: foo[int]())
