template foo*[T: proc](x: typedesc[T]) =
  discard

foo(proc (s: int))