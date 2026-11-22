{.feature: "assumeSync".}
# A matrix type that opts into `std/deps/avoidtemps`. The counters let the
# test observe which operators actually run.

type
  Matrix* {.plugin: "std/deps/avoidtemps".} = object
    a*: array[2, array[2, float]]

var binaryCalls*, inplaceCalls*: int

proc mat*(a, b, c, d: float): Matrix =
  result = Matrix(a: [[a, b], [c, d]])

proc `*=`*(x: var Matrix; y: Matrix) =
  inc inplaceCalls
  let l = x
  for i in 0..1:
    for j in 0..1:
      x.a[i][j] = l.a[i][0] * y.a[0][j] + l.a[i][1] * y.a[1][j]

proc `*=`*(x: var Matrix; s: float) =
  inc inplaceCalls
  for i in 0..1:
    for j in 0..1: x.a[i][j] = x.a[i][j] * s

proc `+=`*(x: var Matrix; y: Matrix) =
  inc inplaceCalls
  for i in 0..1:
    for j in 0..1: x.a[i][j] = x.a[i][j] + y.a[i][j]

proc `-=`*(x: var Matrix; y: Matrix) =
  inc inplaceCalls
  for i in 0..1:
    for j in 0..1: x.a[i][j] = x.a[i][j] - y.a[i][j]

proc subFrom*(x: var Matrix; y: Matrix) =
  ## `x = y - x`. There is deliberately no `mulFrom`, so `a * (b + c)` keeps
  ## its temporary.
  inc inplaceCalls
  for i in 0..1:
    for j in 0..1: x.a[i][j] = y.a[i][j] - x.a[i][j]

proc `*`*(x, y: Matrix): Matrix =
  inc binaryCalls
  result = x; result *= y
proc `*`*(x: Matrix; s: float): Matrix =
  inc binaryCalls
  result = x; result *= s
proc `*`*(s: float; x: Matrix): Matrix =
  inc binaryCalls
  result = x; result *= s
proc `+`*(x, y: Matrix): Matrix =
  inc binaryCalls
  result = x; result += y
proc `-`*(x, y: Matrix): Matrix =
  inc binaryCalls
  result = x; result -= y

proc `$`*(m: Matrix): string =
  result = "[" & $m.a[0][0] & ", " & $m.a[0][1] & "; " & $m.a[1][0] & ", " & $m.a[1][1] & "]"
