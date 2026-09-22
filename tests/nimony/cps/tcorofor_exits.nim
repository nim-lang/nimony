# `for` over a `.passive` iterator: the ways out of the loop, and a loop body
# that suspends.
#
# In a `.passive` routine the loop spans several state procs, so the
# `try`/`finally` of a regular routine's trampoline is not available: every
# exit that skips the loop's end closes the iterator itself (`iterClose`).
# The iterator's `cancel` is not observable from here, so the tests check what
# is: the loop stops where it should, and a second loop starts from scratch.

import std / syncio

proc step() {.passive.} = discard

iterator count(n: int): int {.passive.} =
  var i = 1
  while i <= n:
    yield i
    {.cast(noSideEffect).}:
      step()               # a passive call between two yields
    inc i

iterator evens(n: int): int {.passive.} =
  # an iterator consuming an iterator
  for x in count(n):
    if x mod 2 == 0: yield x

proc regularBreak() =
  for x in count(10):
    if x == 3: break
    echo "regularBreak ", x

proc regularReturn(): int =
  for x in count(10):
    if x == 4: return x
  result = -1

proc passiveBreak() {.passive.} =
  for x in count(10):
    if x == 3: break
    echo "passiveBreak ", x

proc passiveReturn(): int {.passive.} =
  for x in count(10):
    if x == 4: return x
  result = -1

proc passiveBody() {.passive.} =
  var sum = 0
  for x in count(3):
    step()                 # the body suspends too
    sum += x
    echo "passiveBody ", x
  echo "passiveBody sum ", sum

proc nested() {.passive.} =
  for a in count(2):
    for b in count(2):
      echo "nested ", a, " ", b

proc consumeEvens() {.passive.} =
  for e in evens(6):
    echo "evens ", e

regularBreak()
echo "regularReturn ", regularReturn()
passiveBreak()
echo "passiveReturn ", passiveReturn()
passiveBody()
nested()
consumeEvens()
