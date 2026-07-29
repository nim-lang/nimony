## A `.closure` iterator whose body contains an inline `for` loop.
##
## `elimForLoops` must descend into the closure-iter body so the inner
## `for i in a..b` is inlined before cps sees it. Without that descent the
## `(for ...)` survives into later passes and `duplifier` blows up with
## `[Bug] could not find symbol: i.0` on the for-var that was never
## declared.

import std / syncio

iterator countup(a, b: int): int {.closure.} =
  for i in a..b:
    yield i

iterator pairsUp(n: int): int {.closure.} =
  for i in 0..<n:
    for j in 0..<n:
      yield i * 10 + j

proc main() =
  for x in countup(1, 3):
    echo x
  for y in pairsUp(2):
    echo y

main()
