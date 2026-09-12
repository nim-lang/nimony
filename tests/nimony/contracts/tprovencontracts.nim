# The *positive* side of `.requires` checking: every contract these call sites
# carry must be proven, which is the default. It is the regression test for the
# machinery that makes
# the ordinary shapes provable — derived locations (`s.len` is a location, not
# an opaque call), transparent-accessor look-through (`len(s)` and the `s.len`
# written inside `seqimpl` are one location), and the range a `for` loop
# variable takes, which comes from the iterator's own `.ensures` because an
# inline iterator is not inlined until long after this analysis runs.

{.feature: "staticContracts".}

import std/assertions

proc needsPositive(x: int) {.requires: x > 0.} =
  discard x

proc literals =
  needsPositive(2)
  needsPositive(high(int))

proc fromAGuard(y: int) =
  if y > 0:
    needsPositive(y)

proc fromAnEqualityGuard(y: int) =
  if y == 7:
    needsPositive(y)

proc fromALocal =
  let y = 5
  needsPositive(y)

proc seqIndexUnderAGuard(s: seq[int]; i: int): int =
  result = 0
  if i >= 0 and i < s.len:
    result = s[i]

proc seqIndexInAForLoop(s: seq[int]): int =
  result = 0
  for i in 0 ..< s.len:
    result = result + s[i]

proc stringIndexInAForLoop(s: string): int =
  result = 0
  for i in 0 ..< s.len:
    result = result + ord(s[i])

proc openArrayIndexInAForLoop(s: openArray[int]): int =
  result = 0
  for i in 0 ..< s.len:
    result = result + s[i]

proc inclusiveRange(s: seq[int]): int =
  result = 0
  for i in 0 .. s.len - 1:
    result = result + s[i]

proc contractIsAssumedInTheBody(x: int) {.requires: x > 0.} =
  # A body may assume its own precondition: every call site had to discharge it.
  needsPositive(x)

literals()
fromAGuard(1)
fromAnEqualityGuard(7)
fromALocal()
assert seqIndexUnderAGuard(@[1, 2, 3], 1) == 2
assert seqIndexInAForLoop(@[1, 2, 3]) == 6
assert stringIndexInAForLoop("") == 0
assert openArrayIndexInAForLoop([4, 5]) == 9
assert inclusiveRange(@[1, 2]) == 3
contractIsAssumedInTheBody(1)
