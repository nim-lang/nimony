# `delay(f(x))` builds a continuation this chain does not run: a scheduler owns
# it, and in the spawn shape it outlives the frame that built it. So `f` must
# not take a parameter that is an alias of that frame — the coroutine frame
# stores one as a bare pointer, and nothing at the use site says it dangles.

type Box = object
  data: string

proc worker(b: var Box) {.passive.} =
  b.data.add "!"

proc driver() {.passive.} =
  var x = Box(data: "hi")
  let c = delay(worker(x))
  complete(c)

driver()
