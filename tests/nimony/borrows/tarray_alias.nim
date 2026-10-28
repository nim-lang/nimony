# Test: array element aliasing within a call should not be flagged.
# `result[i] = result[last-i]` accesses potentially different elements.

proc reverseString(s: string): string =
  result = s
  let last = result.len - 1
  var i = 0
  let b = result.len div 2
  while i < b:
    let ch = result[i]
    result[i] = result[last - i]
    result[last - i] = ch
    inc i

import std/assertions
assert reverseString("hello") == "olleh"

# Distinct constant indices name distinct elements: no aliasing.
proc swapInts(a, b: var int) =
  let t = a
  a = b
  b = t

proc swapEnds(): array[3, int] =
  result = [1, 2, 3]
  swapInts(result[0], result[2])

assert swapEnds() == [3, 2, 1]
