import std/[assertions, syncio]

# Orphan static type parameter on a generic overload must not compete with a
# non-generic overload at an un-instantiated call site.
func foo[Z: static[int]](x: float): int = Z
func foo(x: float): int = foo[1](x)

assert foo[1](1.0) == 1
assert foo(1.0) == 1

# Same rule for orphan regular type parameters.
proc bar[T](x: int): string = "generic"
proc bar(x: int): string = "concrete"

assert bar(1) == "concrete"

# Static type parameters that appear in the callable surface are still inferred.
proc len[N: static[int]](x: array[N, int]): int = N

var a: array[3, int]
assert len(a) == 3
assert len[3](a) == 3

# The rule holds even when the orphans appear in the RETURN type: `fill: T`
# binds `T` to the whole array and orphans `R` and `C`, so it is not a match
# and the structural overload wins (nim-lang/nimony#2442).
type Matrix[R, C: static[int], T] = object
  rows, cols: int

proc matrix[R, C: static[int], T](elems: array[R, array[C, T]]): Matrix[R, C, T] =
  result = Matrix[R, C, T](rows: R, cols: C)

proc matrix[R, C: static[int], T](fill: T): Matrix[R, C, T] =
  result = Matrix[R, C, T](rows: R, cols: C)

let data: array[2, array[3, float64]] = [
  [1.0'f64, 2.0'f64, 3.0'f64],
  [4.0'f64, 5.0'f64, 6.0'f64]]
let m = matrix(data)
assert m.rows == 2
assert m.cols == 3

# ... and the orphaning overload is still callable once nothing is orphaned.
let f = matrix[4, 5, float64](1.0'f64)
assert f.rows == 4
assert f.cols == 5

# The other side of the rule: a typevar the arguments leave unbound is NOT an
# orphan when the call site's expected type still binds it.
type Res[T, E] = object
  v: T

proc ok[T, E](x: T): Res[T, E] = Res[T, E](v: x)

let r: Res[int, string] = ok(5)
assert r.v == 5

echo "ok"
