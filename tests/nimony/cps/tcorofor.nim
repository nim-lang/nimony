# `for` loops over `.passive` iterators.
#
# Three things that used to be wrong:
#  1. a loop in a `.passive` routine did not compile at all — the iterator call
#     was taken for a suspension point of the routine, which put a state proc
#     inside the loop; and the loop variable was not redirected into the frame;
#  2. a passive call inside the iterator returns into the iterator's frame, and
#     the trampoline's frame-identity test read that as a yield, running the
#     body again with the previous value;
#  3. a `{.cast(...)}:` block anywhere in a coroutine crashed hexer.
#
# The loop is a trampoline inside ONE state proc, so its body cannot suspend:
# no passive call, no `yield`, no park in the iterator. Each of those is
# refused — the first two at compile time, the park at run time.

import std / syncio

proc step() {.passive.} = discard

iterator count(n: int): int {.passive.} =
  var i = 1
  while i <= n:
    yield i
    {.cast(noSideEffect).}:
      step()               # a passive call BETWEEN two yields
    inc i

proc fromRegular() =
  for x in count(3):
    echo "regular ", x

proc fromPassive() {.passive.} =
  var sum = 0
  for x in count(3):       # `x` lives in this coroutine's frame
    sum += x
    echo "passive ", x
  echo "passive sum ", sum

proc breaks() {.passive.} =
  for x in count(10):
    if x == 3: break
    echo "break ", x

proc returns(): int {.passive.} =
  for x in count(10):
    if x == 4: return x
  result = -1

proc nested() {.passive.} =
  for a in count(2):
    for b in count(2):
      echo "nested ", a, " ", b

proc pragmaBlock() {.passive.} =
  echo "before"
  {.cast(noSideEffect).}:
    step()
  echo "after"

fromRegular()
fromPassive()
breaks()
echo "returns ", returns()
nested()
pragmaBlock()
