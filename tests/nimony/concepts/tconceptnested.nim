import std/assertions

## Concept bounds must propagate into nested procs, lambdas, and do-closures
## inside a generic body (classic Nim semantics).

type
  Magnitude* = concept
    func abs(x: Self): Self
    func `<`(a, b: Self): bool

proc abs*(x: float64): float64 =
  if x < 0.0: -x else: x

proc reduce*[T](data: openArray[T], combine: proc(acc, value: T): T {.closure.}): T =
  var started = false
  for value in data:
    if not started:
      result = value
      started = true
    else:
      result = combine(result, value)

func testDirect*[T: Magnitude](x: T): T =
  abs(x)

func testNested*[T: Magnitude](x: T): T =
  proc inner(v: T): T {.noSideEffect.} =
    abs(v)
  inner(x)

func testDo*[T: Magnitude](x: T): T =
  block:
    (proc (v: T): T =
      abs(v))(x)

func testReduce*[T: Magnitude](data: openArray[T]): T =
  reduce(data) do (acc, value: T) -> T:
    let v = abs(value)
    if acc > v: acc else: v

assert testDirect(1.0'f64) == 1.0'f64
assert testReduce([1.0'f64, -3.0'f64, 2.0'f64]) == 3.0'f64
