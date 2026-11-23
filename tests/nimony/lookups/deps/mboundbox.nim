type
  Box*[T] = object
    v*: T

proc describe*[T](x: Box[T]): string = "box"
