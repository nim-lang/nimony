# A `.passive` iterator that PARKS on real I/O between its yields, driven by a
# `for` loop.
#
# The corofor trampoline exits on `stopping(it)`, which a park satisfies just
# like a finish. The loop must not end early on a park, and its `finally`
# (`finalizeCoroutine`) must not cancel a frame the ring still holds.
#
# Two ways to park: inside a passive proc the iterator calls (`nap`), and in
# the iterator's own body.

import std / [syncio, ioring]

proc nap(ms: int) {.passive.} =
  var res = 0
  let c = delay()
  discard submitTimeout(afterMs(ms), c, addr res)
  suspend()

iterator slowCount(): int {.passive.} =
  # `cast(noSideEffect)`: an iterator body is checked as `.noSideEffect`, which
  # would reject the I/O this test is about.
  yield 1
  {.cast(noSideEffect).}:
    nap(10)                # parks in a callee
  yield 2
  var res = 0
  {.cast(noSideEffect).}:
    let c = delay()
    discard submitTimeout(afterMs(10), c, addr res)
    suspend()              # parks in the iterator itself
  yield 3

proc fromRegular() =
  var sum = 0
  for x in slowCount():
    echo "regular: ", x
    sum += x
  echo "regular sum: ", sum

proc fromPassive() {.passive.} =
  var sum = 0
  for x in slowCount():
    echo "passive: ", x
    sum += x
  echo "passive sum: ", sum

fromRegular()
fromPassive()
echo "done"
