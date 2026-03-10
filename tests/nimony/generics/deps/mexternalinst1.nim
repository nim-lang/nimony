type
  Foo[T] = object
  Bar* = object
    y: Foo[int]

proc foo[T](): Foo[T] = default(Foo[T])

proc initBar*[T]() =
  discard Bar(y: foo[int]())
