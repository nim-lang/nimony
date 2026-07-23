import std/assertions

## Concept bounds must propagate into nested procs, lambdas, and do-closures
## inside a generic body (classic Nim semantics).

type
  Iterable* = concept
    iterator items(self: Self): Self

  Magnitude* = concept
    func abs(x: Self): Self
    func `<`(a, b: Self): bool

func abs*(x: float64): float64 =
  if x < 0.0: -x else: x

proc reduce*[C: Iterable; T](source: C, combine: proc(acc, value: T): T {.closure.}): T {.noinit.} =
  var started = false
  for value in source:
    if not started:
      result = value
      started = true
    else:
      result = combine(result, value)

proc testClosure*[T: Magnitude](data: openArray[T]): T =
  reduce(data, proc(acc, value: T): T =
    if acc > abs(value): acc else: abs(value))

proc testDo*[T: Magnitude](data: openArray[T]): T =
  reduce(data) do (acc, value: T) -> T:
    if acc > abs(value): acc else: abs(value)

func testNested*[T: Magnitude](x: T): T =
  proc inner(v: T): T {.noSideEffect.} =
    abs(v)
  inner(x)

assert testClosure([1.0'f64, -3.0'f64, 2.0'f64]) == 3.0'f64
assert testDo([1.0'f64, -3.0'f64, 2.0'f64]) == 3.0'f64
assert testNested(1.0'f64) == 1.0'f64
