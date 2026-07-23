import std/assertions

## Regression: hexer must not crash when hook lookup reaches unconstrained type
## variable symbols produced by concept dispatch inside generic `do` blocks.
##
## Before the fix, lowering modules that combine concept-bound folds with generic
## array algorithms hit `AssertionDefect` in `tryLoadHook` while scanning
## `TypevarY` pragmas.

type
  Field* = concept
    func max(a, b: Self): Self
    func abs(x: Self): Self
    func `/`(a, b: Self): Self
    func `-`(a, b: Self): Self

func max*(a, b: float64): float64 =
  if a > b: a else: b

func abs*(x: float64): float64 =
  if x < 0.0: -x else: x

func `/`*(a, b: float64): float64 =
  a / b

func `-`*(a, b: float64): float64 =
  a - b

proc fold*[N: static[int], T, A](
  data: array[N, T],
  seed: A,
  combine: proc(acc: A, value: T): A {.closure.}
): A =
  result = seed
  for value in data:
    result = combine(result, value)

proc peak*[N: static[int], T: Field](data: array[N, T]): T =
  fold(data, data[0]) do (acc, item: T) -> T:
    max(acc, item)

proc rank*[N: static[int], T: Field](data: array[N, T]): int {.noinit.} =
  var values = data
  values[0] = values[0] - abs(values[0]) / values[0]
  1

assert peak([1.0'f64, 3.0'f64, 2.0'f64]) == 3.0'f64
