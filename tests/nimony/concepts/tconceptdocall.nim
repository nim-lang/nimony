## Concept-bound generic `do` blocks that call concept requirements like
## `max` must survive semcheck (derefs pass). Regression test for a crash in
## `derefs.trCall` when the callee type was `(auto)`.

type
  MagnitudeOps* = concept
    proc max(a, b: Self): Self

  RealScalar* = concept of MagnitudeOps
    proc `+`(a, b: Self): Self
    proc zero(_: typedesc[Self]): Self

proc fold*[N: static[int], T, A](
  values: array[N, T],
  initial: A,
  combine: proc(acc: A, value: T): A {.closure.}
): A =
  result = initial
  for value in values:
    result = combine(result, value)

func maxElem*[N: static[int], T: RealScalar](values: array[N, T]): T =
  fold(values, T.zero) do (acc, value: T) -> T:
    max(acc, value)
