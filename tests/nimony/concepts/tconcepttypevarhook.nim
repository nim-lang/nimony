## Regression: hexer must not crash when hook lookup reaches unconstrained type
## variable symbols produced by concept dispatch inside generic `do` blocks.
##
## Before the fix, lowering modules that combine concept-bound folds with generic
## array algorithms hit `AssertionDefect` in `tryLoadHook` while scanning
## `TypevarY` pragmas.

type
  Field* = concept
    proc max(a, b: Self): Self
    proc abs(x: Self): Self
    proc `/`(a, b: Self): Self
    proc `-`(a, b: Self): Self

proc fold*[N: static[int], T, A](
  data: array[N, T],
  seed: A,
  combine: proc(acc: A, value: T): A {.closure.}
): A =
  result = seed
  for value in data:
    result = combine(result, value)

func peak*[N: static[int], T: Field](data: array[N, T]): T =
  fold(data, data[0]) do (acc, item: T) -> T:
    max(acc, item)

func rank*[N: static[int], T: Field](data: array[N, T]): int {.noinit.} =
  var values = data
  values[0] = values[0] - abs(values[0]) / values[0]
  1
