import mgetclosure

proc passClosure*(): proc (): int {.closure.} =
  var c = getClosure()
  result = c
