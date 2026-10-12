proc value*(x: int): int {.raises.} =
  if x < 0: raise ValueError
  x * 2
