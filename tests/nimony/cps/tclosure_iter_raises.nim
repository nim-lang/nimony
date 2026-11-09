# A `for` loop over a `.closure, raises` iterator. Lambdalifting lowers the
# iterator before the eraiser runs, so the success-tuple slot is its business:
# the iterator yields into `(ErrorCode, T)`, the loop variable is that tuple,
# and the loop checks the code once the iterator is over.

import std / syncio

func mayFail(x: int): int {.raises.} =
  if x < 0:
    raise SyntaxError
  result = x * 10

iterator countTo(n, failAt: int): int {.closure, raises.} =
  var i = 0
  while i < n:
    if i == failAt:
      raise SyntaxError
    yield i
    inc i

iterator words(failAfter: int): string {.closure, raises.} =
  var i = 0
  while i < 3:
    if i == failAfter:
      raise IOError
    yield "word" & $i
    inc i

iterator viaCall(x: int): int {.closure, raises.} =
  yield 1
  yield mayFail(x)   # a raising call inside the iterator
  yield 2

iterator guarded(x: int): int {.closure, raises.} =
  # a `try` inside the iterator, across a `yield`
  try:
    yield 1
    yield mayFail(x)
  except:
    yield -1
  yield 3

proc loopCapture(limit: int) {.raises.} =
  # a capturing closure iterator
  iterator upTo(): int {.closure, raises.} =
    var i = 0
    while true:
      if i > limit:
        raise RangeError
      yield i
      inc i
  for x in upTo():
    if x == 5: break
    echo "cap ", x
  echo "capture done"

type
  IntIter = iterator(n, failAt: int): int {.closure, raises.}

proc loopValue(g: IntIter) {.raises.} =
  # through an iterator value
  for x in g(5, 2):
    echo "val ", x

proc loopInts(n, failAt: int) {.raises.} =
  for x in countTo(n, failAt):
    echo "int ", x
  echo "loop done"

proc loopWords(failAfter: int) {.raises.} =
  for w in words(failAfter):
    echo w
  echo "words done"

proc loopBreak() {.raises.} =
  for x in countTo(10, 5):
    if x == 2: break
    echo "brk ", x
  echo "after break"

proc loopViaCall(x: int): int {.raises.} =
  result = 0
  for v in viaCall(x):
    result += v

proc loopCatch() =
  # caught by a `try` inside a routine that does not raise itself
  try:
    for x in countTo(3, 1):
      echo "catch ", x
    echo "unreachable"
  except:
    echo "loopCatch: caught"

try:
  loopInts(3, -1)
  loopInts(3, 1)
  echo "unreachable"
except ErrorCode as e:
  echo "caught ", e == SyntaxError

try:
  loopWords(-1)
  loopWords(2)
  echo "unreachable"
except ErrorCode as e:
  echo "caught ", e == IOError

try:
  loopBreak()
except ErrorCode:
  echo "unreachable"

try:
  let a = loopViaCall(1)
  echo "sum ", a
  let b = loopViaCall(-1)
  echo "sum ", b
  echo "unreachable"
except ErrorCode as e:
  echo "caught ", e == SyntaxError

loopCatch()

try:
  for x in guarded(1): echo "guarded ", x
  for x in guarded(-1): echo "guarded ", x
except:
  echo "unreachable"

try:
  loopCapture(10)
  loopCapture(1)
  echo "unreachable"
except ErrorCode as e:
  echo "caught ", e == RangeError

try:
  loopValue(countTo)
  echo "unreachable"
except ErrorCode as e:
  echo "caught ", e == SyntaxError
