import std/syncio
import std/math

# Value generic parameters ("static params", issue #2089): the parameter is
# an ordinary value whose type is the plain element type spelled in the
# declaration; there is no `static[T]` type in the checked type system.

type
  Vec[N: static[int]] = object
    data: array[N, int]

var v: Vec[3]
v.data[0] = 42
v.data[2] = 7
echo v.data[0] + v.data[2]

# the value parameter is an ordinary `int` value inside the body:
proc len2[N: static[int]](x: Vec[N]): int = N

echo len2(v)

# binding by unification from an argument's type:
proc first[N: static[int]](x: Vec[N]): int = x.data[0]

echo first(v)

# explicit generic arguments:
echo first[3](v)

type
  Matrix[M, N: static[int]; T] = object
    data: array[M * N, T]   # symbolic at declaration time, folded on instantiation

var m: Matrix[2, 3, int]
m.data[5] = 11
echo m.data[5]
echo sizeof(m)

# concrete operands fold: Matrix[2 + 3, 1, int] is Matrix[5, 1, int]
proc takesFive(x: Matrix[5, 1, int]): int = 5

var m5: Matrix[2 + 3, 1, int]
echo takesFive(m5)

# element access helpers over a value-generic matrix:
proc `[]`[M, N: static[int]; T](m: Matrix[M, N, T]; i, j: int): T =
  m.data[i * N + j]

proc `[]=`[M, N: static[int]; T](m: var Matrix[M, N, T]; i, j: int; v: T) =
  m.data[i * N + j] = v

# dimension-checked matrix multiply: the shared dimension K must unify across
# both operands, and the result dimensions M, N are carried into the result
# type. `T: Arithmetic` supplies `+`/`*`; `{.noinit.}` because the loops fill
# every element (the init analyzer does not reason about loop coverage).
proc `*`[M, K, N: static[int]; T: Arithmetic](a: Matrix[M, K, T];
                                              b: Matrix[K, N, T]): Matrix[M, N, T] {.noinit.} =
  for i in 0 ..< M:
    for j in 0 ..< N:
      var acc = T(0)
      for k in 0 ..< K:
        acc = acc + a[i, k] * b[k, j]
      result[i, j] = acc

var a: Matrix[2, 3, int]
var b: Matrix[3, 2, int]
var t = 1
for i in 0 ..< 2:
  for j in 0 ..< 3:
    a[i, j] = t
    inc t
t = 1
for i in 0 ..< 3:
  for j in 0 ..< 2:
    b[i, j] = t
    inc t

let prod = a * b   # 2x2 = [[22, 28], [49, 64]]
echo prod[0, 0]
echo prod[0, 1]
echo prod[1, 0]
echo prod[1, 1]

# type-level arithmetic in a result type: the column count is the SUM of the
# inputs' column counts (symbolic N1 + N2), folded on instantiation.
proc concatCols[M, N1, N2: static[int]; T](a: Matrix[M, N1, T];
                                           b: Matrix[M, N2, T]): Matrix[M, N1 + N2, T] {.noinit.} =
  for i in 0 ..< M:
    for j in 0 ..< N1:
      result.data[i * (N1 + N2) + j] = a.data[i * N1 + j]
    for j in 0 ..< N2:
      result.data[i * (N1 + N2) + N1 + j] = b.data[i * N2 + j]

var col: Matrix[2, 1, int]
var pair: Matrix[2, 2, int]
col.data[0] = 1; col.data[1] = 2
pair.data[0] = 3; pair.data[1] = 4; pair.data[2] = 5; pair.data[3] = 6
let wide = concatCols(col, pair)   # Matrix[2, 3, int] = [[1, 3, 4], [2, 5, 6]]
echo wide.data[0]
echo wide.data[3]
echo sizeof(wide)                  # 2 * 3 * 8 = 48
