import std/syncio

# Issue #2371: a loop that stays a plain `(loop ...)` construct — it has no
# suspension point of its own — used to lower its back-edge against the
# ENCLOSING suspending loop, because `trGoto` translated every `(continue .)`
# it met into a jump to `loopHeads[^1]`. The inner loop then ran exactly one
# iteration per outer round and jumped back to the outer loop's head state.

proc ping() {.passive.} = discard

# the issue's own repro
var got = 0

proc task() {.passive.} =
  var round = 0
  while round < 2:
    ping()
    round = round + 1
    let xs = @[10, 20, 30]
    var i = 0
    while i < xs.len:
      got = got + xs[i]
      i = i + 1

task()
echo got

# a source-level `continue` in the inner loop keeps working next to the
# back-edge the inner loop owns
proc withContinue(): int {.passive.} =
  result = 0
  var round = 0
  while round < 2:
    ping()
    round = round + 1
    var i = 0
    while i < 5:
      i = i + 1
      if i mod 2 == 0: continue
      result = result + i

echo withContinue()

# ... and so does a `break` out of it
proc withBreak(): int {.passive.} =
  result = 0
  var round = 0
  while round < 3:
    ping()
    round = round + 1
    var i = 0
    while i < 10:
      i = i + 1
      if i == 4: break
      result = result + i

echo withBreak()

# two inner loops in sequence: each needs a back-edge of its own
proc twoLoops(): int {.passive.} =
  result = 0
  var round = 0
  while round < 2:
    ping()
    round = round + 1
    var i = 0
    while i < 3:
      result = result + 1
      i = i + 1
    var j = 0
    while j < 4:
      result = result + 10
      j = j + 1

echo twoLoops()

# nesting: the innermost `continue` belongs to the innermost loop
proc nested(): int {.passive.} =
  result = 0
  var round = 0
  while round < 2:
    ping()
    round = round + 1
    var i = 0
    while i < 3:
      var j = 0
      while j < 2:
        result = result + 1
        j = j + 1
      i = i + 1

echo nested()

# the inner loop suspends too, so it is a state machine of its own: this
# shape already worked and must stay working
proc bothSuspend(): int {.passive.} =
  result = 0
  var round = 0
  while round < 2:
    ping()
    round = round + 1
    var i = 0
    while i < 3:
      ping()
      result = result + 1
      i = i + 1

echo bothSuspend()

# the loop construct is irrelevant to the bug — `for` lowers to the same
# `(loop ...)` construct, so it needs a back-edge of its own just the same
proc forRange(): int {.passive.} =
  result = 0
  var round = 0
  while round < 2:
    ping()
    round = round + 1
    for i in 0 ..< 3:
      result = result + 1

echo forRange()

proc forSeq(): int {.passive.} =
  result = 0
  var round = 0
  while round < 2:
    ping()
    round = round + 1
    let xs = @[10, 20, 30]
    for x in xs:
      result = result + x

echo forSeq()

proc forWithJumps(): int {.passive.} =
  result = 0
  var round = 0
  while round < 2:
    ping()
    round = round + 1
    for i in 0 ..< 10:
      if i == 1: continue
      if i == 4: break
      result = result + i

echo forWithJumps()

# a `for` over a closure iterator is a `corofor` trampoline, a loop the
# state machine builds itself
iterator counter(n: int): int {.closure.} =
  var i = 0
  while i < n:
    yield i
    i = i + 1

proc forCoro(): int {.passive.} =
  result = 0
  var round = 0
  while round < 2:
    ping()
    round = round + 1
    for v in counter(4):
      result = result + v

echo forCoro()
