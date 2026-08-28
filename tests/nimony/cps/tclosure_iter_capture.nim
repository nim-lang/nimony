## Captures from the ENCLOSING proc inside a `.closure` iter body
## (issue #2340). A closure proc carries its environment in the value's
## second tuple slot; an iter value has no such slot free — its env slot
## already holds the coroutine frame. So the env pointer is stored IN the
## frame, in one extra `(ref RootObj)` field:
##
##   - `emitIterValue` (lambdalifting) is the only place that writes it:
##     the env is bound where the iter VALUE is created, never where it is
##     called — `it()` is typically written in a different proc entirely.
##   - `preLowerIter` runs pass 2's ordinary closure lowering over the iter
##     decl before the coroutine transform sees it: a body-prologue loads
##     the frame's env slot into an `ep.0 local and the body then lowers
##     exactly like a closure proc body (`EnvIsParam`); the coroutine
##     transform hoists that local into the frame when a capture is read
##     after a `yield`, like any other local live across a suspension.
##   - a capturing iter called DIRECTLY (`for x in nested()`) is routed
##     through an iter value too: the wrapper's fresh-frame branch
##     allocates the frame itself and could not reach our environment.
##
## What is exercised here: a captured param, a captured local mutated
## after the iter was created (the env is shared, not copied), a captured
## `string` that must outlive the proc that created the iter, a captured
## closure that is CALLED from the body, and the direct-call shape.

import std / syncio

proc countTo(a: int): iterator(): int {.closure.} =
  return iterator(): int {.closure.} =
    var i = 0
    while i <= a:
      yield i
      inc i

proc greeter(name: string): iterator(): string {.closure.} =
  return iterator(): string {.closure.} =
    yield "hello " & name
    yield "bye " & name

## Two iters over ONE captured local: `b` observes what `a` left behind.
proc counterPair(): (iterator(): int {.closure.}, iterator(): int {.closure.}) =
  var n = 0
  let a: iterator(): int {.closure.} = iterator(): int {.closure.} =
    while n < 3:
      yield n
      inc n
  let b: iterator(): int {.closure.} = iterator(): int {.closure.} =
    yield n * 100
  result = (a, b)

## The issue's own use case: a lazy `map`, capturing both the source and
## the mapping proc. (`.noSideEffect` on the callback works around an
## unrelated bug: an iterator body is treated as a no-side-effect context.)
proc map[T, A](source: seq[T]; f: proc (value: T): A {.closure, noSideEffect.}):
    iterator(): A {.closure.} =
  return iterator(): A {.closure.} =
    for value in source:
      yield f(value)

## Direct call of a nested capturing iter — no iter value in the source.
## `inner` uses it from one proc further in, so it has to be handed the
## environment as well even though it captures nothing itself.
proc directLoop(limit: int) =
  var total = 0
  iterator upto(): int {.closure.} =
    var i = 0
    while i < limit:
      yield i
      inc i
  proc inner() =
    for x in upto():
      echo "inner=", x
  inner()
  for x in upto():
    total += x
  echo "total=", total

## Two levels of nesting: a closure proc returning a capturing iter, so
## the env reaches the iter value through the proc's own `ep.0` param
## rather than through an env LOCAL.
proc twoLevels(a: int) =
  proc mid(): iterator(): int {.closure.} =
    return iterator(): int {.closure.} =
      yield a
      yield a * 2
  let it = mid()
  for x in it():
    echo "m=", x

## Inside a capturing iter body: a nested closure PROC over the same
## capture, an iterator with parameters of its own, and a `for` loop over
## a sibling capturing iter.
proc nestedClosure(k: int): iterator(): int {.closure.} =
  return iterator(): int {.closure.} =
    proc scale(v: int): int {.closure, noSideEffect.} = v * k
    yield scale(1)
    yield scale(2)

proc withParam(base: int): iterator(a: int): int {.closure.} =
  return iterator(a: int): int {.closure.} =
    yield base + a
    yield base - a

proc loopOverIter(k: int): iterator(): int {.closure.} =
  iterator inner(): int {.closure.} =
    yield k
    yield k + 1
  return iterator(): int {.closure.} =
    for v in inner():
      yield v * 10

proc main() =
  let it = countTo(4)
  for v in it():
    echo v

  let g = greeter("world")
  for s in g():
    echo s

  let p = counterPair()
  let a = p[0]
  let b = p[1]
  for x in a():
    echo "a=", x
  for x in b():
    echo "b=", x

  let doubled = map[int, int](@[1, 2, 3], proc (v: int): int {.closure, noSideEffect.} = v * 2)
  for v in doubled():
    echo "d=", v

  directLoop(5)
  twoLevels(7)

  let nc = nestedClosure(5)
  for x in nc():
    echo "n=", x
  let wp = withParam(100)
  for x in wp(3):
    echo "w=", x
  let lo = loopOverIter(2)
  for x in lo():
    echo "l=", x

main()
