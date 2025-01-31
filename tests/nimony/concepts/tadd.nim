type Addable = concept
  proc `+`(a, b: Self): Self

template incr[T: Addable](x: var T) =
  x = x + T(1)

var a = 0
incr(a)
