## Shared-state semantics for `.closure` iter VALUES (Nim-compat).
##
## When an iterator is bound to a value (`let g = countup(1, 10)`),
## the resulting iter value owns a heap-allocated coroutine frame.
## Subsequent for-loops over the value resume that single frame
## instead of restarting — calling `g()` twice from two separate
## for-loops continues the iteration where the previous one left off.
##
## Without eager iter-value frame allocation (step 4 of
## project_closure_iter_resume_slot.md) this would print 1..10 twice
## (each for-loop allocated a fresh frame inside the wrapper). With
## step 4 the second loop picks up at the post-break position.

import std / syncio

iterator countup(a, b: int): int {.closure.} =
  var i = a
  while i <= b:
    yield i
    inc i

proc main() =
  # Bind iter SYM to value (Nim syntax; `let g = countup(...)` would
  # be a non-sensical iterator-call-as-expression). The iter value's
  # env slot holds a freshly-allocated `ref CoroType` frame.
  let g = countup
  for x in g(1, 10):
    echo x
    if x == 3:
      break
  echo "---"
  # Same iter value `g`, second for-loop: should RESUME at 4, NOT
  # restart at 1. State (`i`) is preserved in the env-slot ref's
  # frame; the wrapper's `caller.env != nil` branch refreshes args
  # but dispatches via the stashed resume slot in `this.caller.fn`.
  for x in g(1, 10):
    echo x

main()
