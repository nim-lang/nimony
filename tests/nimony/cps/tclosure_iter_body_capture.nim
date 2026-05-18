## For-loop body over a `.closure` iterator contains a closure-proc
## call that captures from the enclosing proc's scope. Lambdalifting
## owns `.closure` iter corofor expansion (cps handles only
## `.passive`). The for-loop body is walked via lambdalifting's own
## `tre`, so capture rewriting — turning `accumulate(x)` from a bare
## call into its tuple-form with the env-arg, and rewriting `sum`
## inside `accumulate` to an env-field access — happens correctly
## inside the loop body.
##
## Also exercises the non-escaping-closure path: `accumulate` never
## leaves `outer`, so `outer`'s env stays on the stack
## (`needsHeap=false`). The captured-var rewrite in `tre`'s Symbol
## path must skip the `(deref ...)` wrap in this case — the env is
## the object itself, not a `ref` to it.

import std / syncio

iterator countup(a, b: int): int {.closure.} =
  var i = a
  while i <= b:
    yield i
    inc i

proc outer() =
  var sum = 0
  proc accumulate(x: int) {.closure.} =
    sum += x
  for x in countup(1, 5):
    accumulate(x)
  echo sum

outer()
