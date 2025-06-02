type
  DInt* = distinct int32

proc `==`(x, y: DInt): bool {.borrow.}

func isFail*(x: DInt): bool {.inline.} =
  x == DInt 0