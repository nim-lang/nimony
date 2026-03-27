type
  DInt* = distinct int32

func `==`(x, y: DInt): bool {.borrow.}

func isFail*(x: DInt): bool {.inline.} =
  x == DInt 0