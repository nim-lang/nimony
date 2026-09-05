import std/syncio
# A generic .passive proc that delay-spawns a generic callee: re-sem met the
# flattened (delay fn args) and rejected it; the callee stayed uninstantiated.
var log = ""
proc leaf[T](x: T) {.passive.} =
  log.add "L"
proc mid[T](x: T) {.passive.} =
  let c = delay(leaf(x))
  complete(c)
  log.add "M"
proc driver() {.passive.} =
  let c = delay(mid(5))
  complete(c)
driver()
echo log
