
type
  int* {.magic: Int.}
  bool* {.magic: Bool.}
  float* {.magic: Float.}

proc `+`*(x, y: int): int {.magic: "AddI".}
proc `-`*(x, y: int): int {.magic: "SubI".}

proc `<=`*(x, y: int): bool {.magic: "LeI".}

proc `+`*(x, y: float): float {.magic: "AddF64".}
proc `-`*(x, y: float): float {.magic: "SubF64".}

proc `<=`*(x, y: float): bool {.magic: "LeF64".}


type
  Fibable = concept
    proc `<=`(a, b: Self): bool
    proc `+`(x, y: Self): Self
    proc `-`(x, y: Self): Self

proc fib[T: Fibable](a: T): T =
  if a <= 2:
    result = 1
  else:
    result = fib(a-1) + fib(a-2)

discard fib(8)
discard fib[int](8)

#discard fib(8.0)
