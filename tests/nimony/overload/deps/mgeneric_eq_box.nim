type
  Box*[A, B] = object
    x*: int

func make*[A, B](): Box[A, B] =
  Box[A, B](x: 1)

func `==`*[A, B](a, b: Box[A, B]): bool =
  false
