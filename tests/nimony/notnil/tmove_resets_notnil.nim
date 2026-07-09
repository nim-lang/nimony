# issue: reading a ref after `move` must be rejected. `move(a)` passes `a` by
# `haddr` and resets it to a moved-from (nil) state, so the `a != nil` proof
# established by the guard no longer holds and `a.x` must be re-proven.

import std/syncio

type
  TT = ref object
    x: int

proc main() =
  var a: nil TT = TT()
  if a != nil:
    a.x += 1
    var b = move(a)
    echo a.x       # INVALID: `a` was moved-from (reset to nil)
    discard b

main()
