# Regression for overload resolution between a structural formal
# (array[R, array[C, T]]) and a catch-all typevar formal (fill: T).
# The catch-all must not win just because it binds fewer type variables.

type
  Matrix*[R, C: static[int], T] = object
    x: int

proc matrix*[R, C: static[int], T](elems: array[R, array[C, T]]): Matrix[R, C, T] {.noinit.} =
  discard

proc matrix*[R, C: static[int], T](fill: T): Matrix[R, C, T] {.noinit.} =
  discard

let data: array[2, array[2, float64]] = [
  [1.0'f64, 2.0'f64],
  [3.0'f64, 4.0'f64],
]

let m = matrix(data)
discard m.x
