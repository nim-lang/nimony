
type
  int* {.magic: Int.}
  bool* {.magic: Bool.}

proc `+`*(x, y: int): int {.magic: "AddI".}
proc `-`*(x, y: int): int {.magic: "SubI".}

proc `<=`*(x, y: int): bool {.magic: "LeI".}

type
  Fibable = concept
    proc `<=`(a, b: Self): bool
    proc `+`(x, y: Self): Self
    proc `-`(x, y: Self): Self

proc fib[T: Fibable](a: T): T =
  if a <= T(2):
    result = T(1)
  else:
    result = fib(a-T(1)) + fib(a-T(2))

discard fib(8)
