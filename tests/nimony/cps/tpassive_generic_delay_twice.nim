import std/syncio
# Two instantiations of a generic .passive proc that delay-spawns: semDelay
# must be idempotent across re-sems.
var log = ""
proc leaf(x: int) {.passive.} =
  log.add "L"
proc mid[T](x: T) {.passive.} =
  let c = delay(leaf(1))
  complete(c)
  log.add "M"
proc driver() {.passive.} =
  let a = delay(mid(5))
  complete(a)
  let b = delay(mid("s"))
  complete(b)
driver()
echo log
