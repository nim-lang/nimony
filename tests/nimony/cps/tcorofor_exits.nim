# `for` over a `.passive` iterator: the ways out of the loop, and a loop body
# that suspends.
#
# The loop's `Join` is an ordinary local whose `=destroy` closes the
# iterator, so the destroyer covers every exit — even though in a `.passive`
# routine the loop spans several state procs.
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

# The move analysis (`mover`) runs while `corofor` is still a statement. It
# used to `bug` on one ("statement not eliminated: corofor") once a walk
# reached it — a destructor-carrying tuple pattern followed by another loop —
# and never took the loop's back-edge, so a value consumed in the body was
# moved on the first pass and the later passes saw it empty.

iterator pairsOf(xs: seq[string]): (int, string) {.passive.} =
  var i = 0
  while i < xs.len:
    yield (i, xs[i])
    inc i

proc sinkIt(s: sink string) = echo "sunk '", s, "'"

proc twoLoops() {.passive.} =
  for (i, s) in pairsOf(@["a", "b"]):
    echo "pair ", i, " ", s
  for x in count(2):
    echo "then ", x

proc backEdge() {.passive.} =
  var z = "z"
  for x in count(3):
    sinkIt(z)              # read again on the next pass: must not move
  var t = "t"
  for x in count(3):
    if x == 1: continue
    sinkIt(t)              # likewise after a `continue`
  var u = "u"
  for x in count(2):
    discard
  sinkIt(u)                # the last read: may move

regularBreak()
echo "regularReturn ", regularReturn()
passiveBreak()
echo "passiveReturn ", passiveReturn()
passiveBody()
nested()
consumeEvens()
twoLoops()
backEdge()
