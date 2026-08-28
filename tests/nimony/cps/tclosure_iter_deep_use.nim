## A CAPTURING `.closure` iter used two proc levels below its owner:
## `deep` mentions `it` (joins `closureProcs` via `propagateEnvNeed`),
## `mid` mentions `deep` (joins transitively via the fixpoint) — so the
## environment is threaded from `root` down to where `emitIterValue`
## binds it into the iter's frame.

import std/syncio

proc root(k: int) =
  var acc = 0
  iterator it(): int {.closure.} =
    var i = 0
    while i < k:
      yield i + acc
      inc i
  proc mid() =
    proc deep() =
      for v in it():
        echo v
    deep()
  acc = 10
  mid()

root(3)
