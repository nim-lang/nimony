# A `while` condition that needs a TEMPORARY was evaluated only once.
#
# `build()` returns a `Box`, which owns a `string`, so the duplifier binds the
# call's result to an owning temp before the field read can happen. That temp
# is a statement, and a statement in front of a condition still sitting in the
# `while`'s condition slot lands *outside* the loop — evaluated once instead of
# once per iteration. `counter` then came out as 1.
#
# The fix is the Final-IR loop shape (`doc/final_ir.md`): a `while` whose
# condition is impure is rewritten by `xelim` into `while true:` with the
# condition as a leading guard in the body, so there IS a per-iteration
# statement position to hoist into (`xelim.loopCondNeedsGuard`).

import std / [syncio, assertions]

type Box = object
  s: string

var counter = 0

proc build(): Box =
  inc counter
  result = Box(s: "abc")

proc main =
  var i = 0
  while build().s.len == 3:
    inc i
    if i >= 3: break
  echo "iterations: ", i
  echo "condition evaluations: ", counter
  assert counter == 3

main()
