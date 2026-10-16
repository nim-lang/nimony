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
