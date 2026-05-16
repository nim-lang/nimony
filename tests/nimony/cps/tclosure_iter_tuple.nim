## Closure iterators that yield tuples and are consumed by `for k, v in ...`
## loops. iterinliner's emitCoroFor synthesises a hidden tuple-typed slot for
## the multi-var case and re-binds each user for-var via `(tupat hidden i)`
## per iteration. Also covers `var T` components inside the tuple — mutation
## through the borrow propagates back to the source array.

import std / syncio

iterator pairs(s: array[3, string]): (int, string) {.closure.} =
  yield (0, s[0])
  yield (1, s[1])
  yield (2, s[2])

iterator mpairs(s: var array[3, int]): (int, var int) {.closure.} =
  yield (0, s[0])
  yield (1, s[1])
  yield (2, s[2])

proc main() =
  let arr = ["alpha", "beta", "gamma"]
  for i, v in pairs(arr):
    echo i
    echo v

  var nums = [10, 20, 30]
  for k, vref in mpairs(nums):
    vref = vref + k * 100
  echo nums[0]
  echo nums[1]
  echo nums[2]

main()
