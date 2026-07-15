proc getClosure*(): proc (): int {.closure.} =
  var x = 7
  proc (): int {.closure.} = x
