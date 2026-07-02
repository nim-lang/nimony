## Regression: `=wasMoved` (and other location-taking builtins) applied to an
## lvalue *call* — e.g. a `seq[T]` element access `s[i]`, which lowers to a
## call returning `var T` — used to crash njvl with
## `[Bug] call must have been bound to a location`. The lvalue-call argument
## is now bound to a location like any other call operand.

import std/syncio

proc test(cap: int) =
  var s = newSeqUninit[int](cap)
  for i in 0 ..< cap:
    `=wasMoved`(s[i])
  echo s.len

test(4)
