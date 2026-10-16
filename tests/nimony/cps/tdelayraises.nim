# Like a result, an error of a delayed call surfaces where nothing can catch
# it, so `delay` rejects a `.raises` callee. (A `try` around `delay` would
# catch nothing: the call runs when the continuation does.)

proc job(x: int) {.passive, raises.} =
  if x > 1: raise ValueError

proc driver() =
  let c = delay(job(1))
  complete(c)

driver()
