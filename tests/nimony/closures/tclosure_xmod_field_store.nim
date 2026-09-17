# A closure stored into the `proc {.closure.}` field of a FOREIGN object.
# The importing module sees the field's semchecked type — `(proctype …
# closure)`, never lowered to `(closureTuple fn env)` here — and the lifter
# called that trivial: the store was a bitcopy with no `=wasMoved` of the
# local, whose `=destroy` at scope end then freed the env the field still
# pointed at.
#
# Observable without a memory checker: the freed env block is the next
# same-sized allocation, so the SECOND window's closure env lands on top of
# the first's, and firing the first window reports the second's tag (or
# crashes) instead of its own.
{.feature: "lenientnils".}
import std/syncio
import deps/mclosurefield

proc attach(w: var Window; tag: string) =
  var name = tag # captured: lives in the closure's env
  let enqueue = proc() {.closure.} = report("configured ", name)
  w.onConfigured = enqueue
  # `enqueue` dies here; the field must have taken over its reference

proc readBack(w: var Window) =
  # the same field read from THIS module: a copy out, a clear (destroy of
  # the old value), the call, then the copy's own destroy
  let cb = w.onConfigured
  w.onConfigured = nil
  if cb != nil: cb()

proc main() =
  var a = newWindow("a")
  var b = newWindow("b")
  attach(a, "alpha")
  attach(b, "beta")
  fire(a)
  fire(b)
  fire(a) # cleared: nothing to run
  attach(a, "gamma")
  attach(b, "delta")
  readBack(a)
  readBack(b)
  echo "done"

main()
echo "ok"
