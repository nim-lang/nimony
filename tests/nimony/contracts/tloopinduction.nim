# Loop induction. A loop body is walked once but runs many times, so a fact
# established ahead of the loop holds only on the first iteration — dropping all
# of them is sound but throws away the one thing every counting loop rests on:
#
#   var i = 0
#   while i < s.len:      # `i < s.len` is the guard
#     use(s[i])           # `0 <= i` has to survive the loop
#     inc i
#
# `0 <= i` *is* an invariant, because every write to `i` in the body moves it
# up. `scanLoopWrites` classifies each written location as increasing /
# decreasing / unknown and keeps exactly the facts no such movement can break:
# in `a <= b + c`, `a` may only go down and `b` may only go up.
#
# `staticContracts` makes every undecided contract an error, so each index below
# must be *proven* for this file to compile.

{.feature: "staticContracts".}

from std/syncio import quit
import std/assertions

proc viaInc(s: seq[int]): int =
  result = 0
  var i = 0
  while i < s.len:
    result = result + s[i]
    inc i

proc viaExplicitStep(s: seq[int]): int =
  result = 0
  var i = 0
  while i < s.len:
    result = result + s[i]
    i = i + 1

proc viaStepOfTwo(s: seq[int]): int =
  result = 0
  var i = 0
  while i < s.len:
    result = result + s[i]
    inc(i, 2)

proc downwards(s: string): int =
  # `dec` is the mirror image: an upper bound survives where a lower one does
  # not, and the guard supplies the other half.
  result = 0
  var i = s.len - 1
  while i >= 0:
    result = result + ord(s[i])
    dec i

proc guardIsAConjunction(s: seq[int]): int =
  result = 0
  var i = 0
  while i < s.len and s[i] != 0:
    result = result + s[i]
    inc i

proc nested(s: seq[int]): int =
  result = 0
  var i = 0
  while i < s.len:
    var j = i
    while j < s.len:
      result = result + s[j]
      inc j
    inc i

proc withABreak(s: seq[int]): int =
  result = 0
  var i = 0
  while i < s.len:
    if s[i] == 0: break
    result = result + s[i]
    inc i

proc anAssertEstablishesIt(s: seq[int]; i: int): int =
  # The `and` is materialized into one boolean and the `assert` tests its
  # *negation*, so the knowledge sits on the false side of the `not`.
  assert i >= 0 and i < s.len
  result = s[i]

proc aDisjunctiveGuardClause(s: seq[int]; i: int): int =
  # `t` false means neither disjunct held, which is where both bounds come from.
  if i < 0 or i >= s.len: quit "out of range"
  result = s[i]

proc separateGuardClauses(s: seq[int]; i: int): int =
  result = 0
  if i < 0: return
  if i >= s.len: return
  result = s[i]

assert viaInc(@[1, 2, 3]) == 6
assert viaExplicitStep(@[1, 2, 3]) == 6
assert viaStepOfTwo(@[1, 2, 3]) == 4
assert downwards("") == 0
assert guardIsAConjunction(@[1, 2, 0, 4]) == 3
assert nested(@[1, 2]) == 5
assert withABreak(@[1, 2, 0, 9]) == 3
assert anAssertEstablishesIt(@[5], 0) == 5
assert aDisjunctiveGuardClause(@[5], 0) == 5
assert separateGuardClauses(@[5], 0) == 5
