import std/assertions

## Concept-bound generic `do` blocks that call concept requirements like
## `max` must survive semcheck (derefs pass). Regression test for a crash in
## `derefs.trCall` when the callee type was `(auto)`.

type
  MagnitudeOps* = concept
    func max(a, b: Self): Self

  RealScalar* = concept of MagnitudeOps
    func `+`(a, b: Self): Self
    func zero(_: typedesc[Self]): Self

func `+`*(a, b: float64): float64 =
  a + b

func zero*(_: typedesc[float64]): float64 =
  0.0

func max*(a, b: float64): float64 =
  if a > b: a else: b

proc fold*[N: static[int], T, A](
  values: array[N, T],
  initial: A,
  combine: proc(acc: A, value: T): A {.closure.}
): A =
  result = initial
  for value in values:
    result = combine(result, value)

proc maxElem*[N: static[int], T: RealScalar](values: array[N, T]): T =
  fold(values, T.zero) do (acc, value: T) -> T:
    max(acc, value)

assert maxElem([1.0'f64, 3.0'f64, 2.0'f64]) == 3.0'f64
