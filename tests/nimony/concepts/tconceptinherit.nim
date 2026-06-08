import std/assertions

type
  A = concept
    proc `+`(a, b: Self): Self

  B = concept of A
    proc `-`(a, b: Self): Self

  C = concept of A, B
    ## multiple inheritance
    proc `<=`(a, b: Self): bool

  AB = concept of A, B
    # empty body (constraints come only from parents)

  AOnly = concept of A

proc sumA[T: A](x, y: T): T =
  x + y

proc diffB[T: B](x, y: T): T =
  x - y

proc sumB[T: B](x, y: T): T =
  x + y

proc cmpC[T: C](x, y: T): bool =
  x <= y

proc fromB[T: B](x, y: T): T =
  sumA(x, y)

proc sumAB[T: AB](x, y: T): T =
  x + y

proc diffAB[T: AB](x, y: T): T =
  x - y

proc sumAOnly[T: AOnly](x, y: T): T =
  x + y

assert sumA(1, 2) == 3
assert diffB(5, 2) == 3
assert sumB(10, 5) == 15
assert cmpC(1, 2)
assert fromB(2, 3) == 5
assert sumAB(10, 5) == 15
assert diffAB(10, 3) == 7
assert sumAOnly(4, 6) == 10
