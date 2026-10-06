import std/syncio
# `delay` hands the callee's frame to a scheduler, so the frame has to own
# what its borrowed parameters point to: the caller's reference may be gone
# before the continuation runs. The destructor shows who frees the box.

type
  Tracked = object
    name: string
  Box = ref object
    t: Tracked

proc `=destroy`(t: Tracked) =
  if t.name.len > 0: echo "destroy ", t.name

proc show(b: Box; tag: string) {.passive.} =
  echo tag, " ", b.t.name

proc driver() =
  var c: Continuation
  block:
    let b = Box(t: Tracked(name: "box"))
    c = delay(show(b, "x" & "y"))
  echo "caller's ref is gone"
  complete(c)
  echo "done"

driver()
