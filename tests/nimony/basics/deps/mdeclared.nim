type
  bool* {.magic: Bool.} = enum
    false = 0, true = 1
  untyped* {.magic: Expr.}
proc declared*(x: untyped): bool {.magic: Declared.}
