import std/syncio
# A generic .passive proc that suspends: instantiation re-sem typed suspend()
# as Continuation and the discard check rejected it.
var log = ""
proc worker[T](x: T) {.passive.} =
  log.add "w"
  suspend()
  log.add "W"
proc driver() {.passive.} =
  let c = delay(worker(3))
  complete(c)
  complete(c)
driver()
echo log
