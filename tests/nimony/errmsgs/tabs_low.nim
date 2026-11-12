# Signed arithmetic wraps, so `abs(low(T)) == low(T)`: `abs` promises no
# `0 <= result`, and a claim built on one is not proven, for any width.

proc wide(x: int) =
  let a = abs(x)
  {.assert: a >= 0.}

proc narrow(x: int8) =
  let a = abs(x)
  {.assert: not (a < 0'i8).}

wide(low(int))
narrow(low(int8))
