# A delayed call runs later, often on a worker, after the expression that
# built it is gone: a result has nowhere to go, so `delay` rejects a callee
# that returns one.

proc calc(x: int): int {.passive.} =
  result = x * 2

proc driver() =
  let c = delay(calc(3))
  complete(c)

driver()
