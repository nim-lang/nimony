# The same rule through the flattened `(delay fn args)` shape: a generic body
# is copied verbatim on the first pass and re-sem'd on instantiation, and that
# is a second place the callee is resolved. The escape has to be caught there
# too, or it slips through whenever the spawner is generic.

proc gleaf[T](x: var T) {.passive.} =
  x = x

proc gmid[T](v: T) {.passive.} =
  var local = v
  let c = delay(gleaf(local))
  complete(c)

proc gdriver() {.passive.} =
  let c = delay(gmid(5))
  complete(c)

gdriver()
