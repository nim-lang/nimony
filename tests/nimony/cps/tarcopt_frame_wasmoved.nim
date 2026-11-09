# arcopt elides a `=wasMoved(x); =destroy(x)` pair only for a whole local. In a
# `.passive` iterator a local that lives across a `yield` is a frame field,
# `(dot (deref frame) x)`: its uses in between are paths, not mentions of a
# symbol, so such a pair must stay.

import std/syncio

type Tracked = object
  name: string

proc `=wasMoved`(x: var Tracked) {.nodestroy, inline.} = zeroMem(addr x, sizeof(x))
proc `=destroy`(t: Tracked) {.noSideEffect.} =
  if t.name.len > 0:
    {.cast(noSideEffect).}: echo "destroy ", t.name

proc consume(t: sink Tracked) {.noSideEffect.} =
  {.cast(noSideEffect).}: echo "consume ", t.name

iterator walk(tag: string): int {.passive.} =
  var x = Tracked(name: tag & ".first")
  yield 1
  consume x                        # `=wasMoved(x)`
  x.name = tag & ".patched"        # writes through the frame slot
  x = Tracked(name: tag & ".second") # `=destroy(x)` first: "destroy w.patched"
  {.cast(noSideEffect).}: echo "end ", x.name  # `=destroy(x)` follows

proc main() =
  for i in walk("w"):
    echo "got ", i

main()
