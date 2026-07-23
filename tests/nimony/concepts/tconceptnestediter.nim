import std/assertions

type
  Iterable* = concept
    iterator items(self: Self): auto

  Mag* = concept
    func abs(x: Self): Self
  RealS* = concept of Mag
    func `<`(a, b: Self): bool

proc abs*(x: float64): float64 =
  if x < 0.0: -x else: x

iterator items*(a: openArray[float64]): float64 =
  for x in a:
    yield x

proc reduce*[C: Iterable; T](source: C, combine: proc(acc, value: T): T {.closure.}): T =
  var started = false
  for value in items(source):
    if not started:
      result = value
      started = true
    else:
      result = combine(result, value)

func maxNorm*[T: RealS](data: openArray[T]): T =
  reduce(data) do (acc, value: T) -> T:
    if acc > abs(value): acc else: abs(value)

assert maxNorm([1.0'f64, -3.0'f64, 2.0'f64]) == 3.0'f64
