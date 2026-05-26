## First-class `.passive` iterator values: `iterator(args): T {.passive.}` as
## a value type.
##
## Unlike `.closure` iter values (which carry a captured environment and lower
## to a `(tuple <wrapper-proctype> (ref RootObj))`), a `.passive` iter has no
## environment, so it lowers to a bare wrapper proctype — a plain function
## pointer. Tests:
##   1. type-level: `iterator(a, b: int): int {.passive.}` is a valid value type.
##   2. nil: `let h: MyIter = nil` round-trips and compares to nil.
##   3. capture: `let g: MyIter = countup` stores the iter as a value.
##   4. pass-through: iter values flow through proc parameters.
##   5. for-loop over the value: `for v in g(args):` lowers via the same
##      corofor trampoline as direct iter calls, calling through the value's
##      function pointer.

import std / syncio

type
  MyIter = iterator(a, b: int): int {.passive.}

iterator countup(a, b: int): int {.passive.} =
  var i = a
  while i <= b:
    yield i
    inc i

iterator doubled(a, b: int): int {.passive.} =
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
