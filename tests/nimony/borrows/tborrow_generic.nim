import std/assertions

type
  Slice*[T] = distinct seq[T]
  Meters*[T] = distinct T

# generic distinct over a generic base type
proc len*[T](x: Slice[T]): int {.borrow.}

# distinct return type: the result is converted back
proc `+`*[T](x, y: Meters[T]): Meters[T] {.borrow.}
proc `==`*[T](x, y: Meters[T]): bool {.borrow.}

# generic routine over a concrete distinct type
proc twice*[T](x: Meters[int]; y: T): Meters[int] {.borrow.}
proc twice*[T](x: int; y: T): int = x * 2 + int(y)

var s = Slice[int](@[1, 2, 3])
assert s.len == 3

var s2 = Slice[string](@["a", "b"])
assert s2.len == 2

let a = Meters[int](3)
let b = Meters[int](4)
assert int(a + b) == 7
assert a + b == Meters[int](7)

assert int(twice(a, 1)) == 7

let c = Meters[float](1.5)
let d = Meters[float](2.5)
assert float(c + d) == 4.0
