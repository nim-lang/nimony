## First-class closure-iterator values: `iterator(args): T` as a value type.
##
## Tests:
##   1. type-level: `iterator(a, b: int): int {.closure.}` is a valid type
##      (`ItertypeT`), assignable/comparable as a value.
##   2. nil: `let g: MyIter = nil` round-trips.
##   3. capture: `let g: MyIter = countup` captures the iter sym as the value.
##   4. pass-through: iter values flow through proc parameters.
##   5. for-loop over the value: `for v in g(args):` lowers via the same
##      corofor trampoline used for direct iter calls — cps's trCoroFor
##      distinguishes iter-sym vs iter-value via its typeCache and calls
##      through the local function-pointer when appropriate.

import std / syncio

type
  MyIter = iterator(a, b: int): int {.closure.}

iterator countup(a, b: int): int {.closure.} =
  var i = a
  while i <= b:
    yield i
    inc i

iterator doubled(a, b: int): int {.closure.} =
  var i = a
  while i <= b:
    yield i * 2
    inc i

proc consume(g: MyIter; lo, hi: int) =
  for v in g(lo, hi):
    echo v

proc main() =
  let g: MyIter = countup
  if g != nil:
    echo "g != nil"
  let h: MyIter = nil
  if h == nil:
    echo "h == nil"
  for v in g(1, 5):
    echo v
  consume(countup, 1, 3)
  consume(doubled, 4, 6)

main()
