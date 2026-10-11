## Composable iterators (issue #2487): a `proc` that TAKES an
## `iterator(): T` and RETURNS one, so lazy stages chain.
##
## Spelled the default way — every iterator type and iterator literal carries
## `.closure.`, because a closure is never inferred (doc/language.md,
## "Closures"). `titer_compose_autoclosures.nim` is the same program under
## `.feature: "autoclosures".`, where both annotations are implied.
##
## Four things had to be true for the composition itself to work:
##
##   1. Calling through an itertype VALUE is `noSideEffect` by default, like
##      calling the iterator it holds — otherwise a `for` over an iter value
##      is rejected inside any iterator or `func` body.
##   2. A `for` over an iter value inside a closure-iterator body lowers to a
##      `corofor` that has to be expanded BEFORE the body is cut into state
##      procs, or the trampoline's `while` is a plain loop and the `yield`
##      inside it returns to nobody. Capture-free iters used to skip that
##      pre-lowering entirely (`lambdalifting.transformClosureIter`), and a
##      captured iter value's `(envp …)` target was not recognized as an
##      iter-value call at all.
##   3. `for x in take(it, 2)():` — the chained form — needs a `for` head whose
##      callee is a CALL, not only a symbol, to count as an iterator call.
##   4. A module-level `let g: iterator(): T {.closure.}` driven by a top-level
##      `for`: `iterinliner` read the `(let …)` decl behind the symbol as a
##      routine and walked off its end.

import std / syncio

func iter*[T](s: seq[T]): iterator(): T {.closure.} =
  return iterator(): T {.closure.} =
    for x in s:
      yield x

proc map*[T, U](it: iterator(): T {.closure.}; f: proc (x: T): U {.noSideEffect.}): iterator(): U {.closure.} =
  return iterator(): U {.closure.} =
    for v in it():
      yield f(v)

proc filter*[T](it: iterator(): T {.closure.}; p: proc (x: T): bool {.noSideEffect.}): iterator(): T {.closure.} =
  return iterator(): T {.closure.} =
    for v in it():
      if p(v):
        yield v

proc take*[T](it: iterator(): T {.closure.}; n: int): iterator(): T {.closure.} =
  return iterator(): T {.closure.} =
    var k = 0
    for v in it():
      if k >= n: break
      yield v
      inc k

## Two stages over one source, so the second `for` sees an EXHAUSTED iter
## value and must simply yield nothing rather than restart or crash.
proc twice*[T](it: iterator(): T {.closure.}): iterator(): T {.closure.} =
  return iterator(): T {.closure.} =
    for v in it():
      yield v
    for v in it():
      yield v

## The issue's own `cycle`, and the shape that keeps a `for` over an iter value
## inside a `while` — the trampoline has to be part of the state machine, not a
## plain loop around it. `iter(s)()` is rebuilt each round because an exhausted
## iterator stays exhausted; `iter` is a `func` because an iterator body is a
## `noSideEffect` context.
func cycle*[T](s: seq[T]): iterator(): T {.closure.} =
  return iterator(): T {.closure.} =
    while true:
      for value in iter(s)():
        yield value

## Capture-free: `src` is a module-level iterator, so the anonymous iter
## below captures nothing — the case that used to skip `preLowerIter`.
iterator src(): int {.closure.} =
  yield 1
  yield 2

proc relay*(): iterator(): int {.closure.} =
  return iterator(): int {.closure.} =
    for v in src():
      yield v * 10

## An iter VALUE stored in a module-level `let`, driven by a top-level `for`.
let g: iterator(): int {.closure.} = src

proc main() =
  let numbers = @[1, 2, 3, 4, 5, 6]

  echo "-- pipeline"
  let evens = filter(iter(numbers), func (v: int): bool = v mod 2 == 0)
  let labeled = map(evens, func (v: int): string = "v=" & $v)
  for x in take(labeled, 2)():
    echo x

  echo "-- twice"
  for x in twice(iter(@[7, 8]))():
    echo x

  echo "-- relay"
  for x in relay()():
    echo x

  echo "-- cycle"
  var n = 0
  for x in cycle(@[1, 2, 3])():
    echo x
    inc n
    if n >= 7: break

main()

echo "-- module-level iter value"
for x in g():
  echo x
