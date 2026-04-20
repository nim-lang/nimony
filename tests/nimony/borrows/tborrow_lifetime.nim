# Test: borrow lifetime - borrowing from var param, then using borrowed path.
# No mutation during borrow should be fine.

import std/assertions

proc readFirst(s: var seq[int]): int =
  result = s[0]

proc testBorrowNoConflict =
  var s = @[1, 2, 3]
  let x = readFirst(s)
  assert x == 1

testBorrowNoConflict()

# Passing the same seq to two different by-value params is fine:
proc addBoth(a, b: int): int = a + b

proc testValueParams =
  var s = @[10, 20]
  let r = addBoth(s[0], s[1])
  assert r == 30

testValueParams()
