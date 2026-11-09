# A `for` loop over a `.passive, raises` iterator. The iterator yields into its
# loop variable's slot, which for a raising iterator is the success tuple: a
# raise writes its code into the slot's first half and ends the iterator, and
# the loop checks that code once it is over.

import std / syncio

proc step() {.passive.} = discard

proc mayFail(x: int) {.passive, raises.} =
  step()
  if x < 0:
    raise SyntaxError

iterator countTo(n, failAt: int): int {.passive, sideEffect, raises.} =
  var i = 0
  while i < n:
    if i == failAt:
      raise SyntaxError
    yield i
    inc i

iterator words(failAfter: int): string {.passive, sideEffect, raises.} =
  var i = 0
  while i < 3:
    if i == failAfter:
      raise IOError
    yield "word" & $i
    inc i

iterator viaCall(x: int): int {.passive, sideEffect, raises.} =
  yield 1
  mayFail(x)     # a raising passive call inside the iterator
  yield 2

# --- the shape of `filesystem.walk`: a raising proc, the loop under `try` ---

iterator walkish(): int {.passive, sideEffect, raises.} =
  yield 1
  yield 2

proc driver() {.raises.} =
  try:
    for x in walkish():
      echo "walkish ", x
  except ErrorCode:
    echo "unreachable"

proc loopInts(n, failAt: int) {.raises.} =
  for x in countTo(n, failAt):
    echo "int ", x
  echo "loop done"

proc loopWords(failAfter: int) {.raises.} =
  for w in words(failAfter):
    echo w
  echo "words done"

proc loopBreak() {.raises.} =
  # leaving early never reaches the error the iterator would have raised
  for x in countTo(10, 5):
    if x == 2: break
    echo "brk ", x
  echo "after break"

proc loopViaCall(x: int): int {.raises.} =
  result = 0
  for v in viaCall(x):
    result += v

proc passiveLoop(failAt: int) {.passive, raises.} =
  # a loop in a `.passive` routine is laid out by `cps` itself
  for x in countTo(4, failAt):
    step()
    echo "passive ", x

proc catchInPassive() {.passive.} =
  try:
    passiveLoop(-1)
    passiveLoop(2)
    echo "unreachable"
  except:
    echo "catchInPassive: caught"

try:
  driver()
except ErrorCode:
  echo "unreachable"

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

catchInPassive()
