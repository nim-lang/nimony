
type
  int* {.magic: Int.}
  bool* {.magic: Bool.}

proc `+`*(x, y: int): int {.magic: "AddI".}
proc `-`*(x, y: int): int {.magic: "SubI".}

proc `<=`*(x, y: int): bool {.magic: "LeI".}

iterator countup(a, b: int): int =
  var x = a
  while x <= b:
    yield x
    x = x + 1

for mycounter in countup(0, 44):
  discard mycounter
