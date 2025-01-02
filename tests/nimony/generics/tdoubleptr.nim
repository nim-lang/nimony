proc generic[T](a, b: ptr T) =
  discard a == b

proc generic2[T](a, b: ptr T) =
  generic(a, b)
