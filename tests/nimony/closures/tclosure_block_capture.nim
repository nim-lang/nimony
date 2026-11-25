## A closure capturing a local of an enclosing `block`. The Final IR turns the
## block into a `(scope …)`, and typenav's capture count used to stop at the
## first scope that declared the local, reporting no capture at all.
import std/syncio

proc main =
  block:
    var a = 5
    proc inner(x: int): int {.closure.} =
      result = a + x
    echo inner(3)
    a = 10
    echo inner(3)

main()

# the same at module level, where the block is no proc's body (#2555)
block:
  var nums = @[1, 2, 3]
  let p = proc () {.closure.} =
    nums.add 4
  p()
  echo nums.len
  iterator counted(): int {.closure.} =
    yield nums[3]
  for x in counted(): echo x

proc dummy(): int = 0
var escaped: proc (): int {.closure.} = dummy
block:
  var x = 10
  escaped = proc (): int {.closure.} =
    inc x
    result = x
echo escaped()
echo escaped()

# A module level `while` is a `(loop)` after lowering. The whole loop owns the
# environment, as a proc owns the environment of its loops: the closures share
# `w` (2 2 2, the same as inside a proc), and `continue` stays the loop body's
# last statement (it used to end up inside the environment's scope).
var wcbs: seq[proc () {.closure.}] = @[]
var wk = 0
while wk < 3:
  var w = wk
  wcbs.add proc () {.closure.} = echo w
  inc wk
for cb in wcbs: cb()

wk = 0
while true:
  inc wk
  var w = wk
  let p = proc () {.closure.} = echo w
  if wk == 2: continue
  p()
  if wk >= 3: break

var wi = 0
while wi < 2:
  var wj = 0
  while wj < 2:
    var w = wi * 10 + wj
    let p = proc () {.closure.} = echo w
    p()
    inc wj
  inc wi
