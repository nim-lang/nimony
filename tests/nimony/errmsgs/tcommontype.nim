type Foo[T] = object
  x: ptr UncheckedArray[T]

proc foo[T]() =
  var y: int
  discard Foo[T](x: addr y)
