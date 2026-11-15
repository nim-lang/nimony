# A join keeps a constant bound that one arm states and the other only derives,
# at the weaker of the two constants: `result = 0` states `result <= 0`, and
# `result = x` under `x < 3` derives `result <= 2`, so `result <= 2` survives.

{.feature: "staticContracts".}

import std/syncio

proc clamp3(x: int): int {.ensures: result >= 0 and result < 3.} =
  if x < 0: result = 0
  elif x >= 3: result = 2
  else: result = x

proc pickRange(x: int; neg: bool): int {.requires: x >= 0 and x < 10,
    ensures: result >= -1 and result < 10.} =
  var r = -1
  if not neg:
    r = x
  result = r

const arr = [10, 20, 30]
echo arr[clamp3(7)], " ", arr[clamp3(-4)], " ", arr[clamp3(1)]
echo pickRange(4, false), " ", pickRange(4, true)
