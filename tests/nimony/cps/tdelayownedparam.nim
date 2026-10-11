import std/syncio
# The other side of `tdelayvarparam`: what a `.passive` proc may still do with
# a parameter that aliases its caller. Only `delay` is the escape — a passive
# call made in the ordinary way suspends the caller, whose frame is therefore
# alive for the whole of the callee, so `var` stays legal there.

type Box = object
  data: string

proc bump(b: var Box) {.passive.} =      # aliases the caller: fine when called
  b.data.add "!"

proc consume(b: sink Box) {.passive.} =  # owns it: fine when delayed
  echo b.data

proc driver() {.passive.} =
  var x = Box(data: "hi")
  bump(x)                                # an ordinary passive call
  let c = delay(consume(x))
  complete(c)

driver()
