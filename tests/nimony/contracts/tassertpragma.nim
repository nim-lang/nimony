# The proven side of `{.assert: cond.}`: a claim the prover discharges costs
# nothing at run time and is a fact afterwards, so it can carry an obligation
# further. `{.assume: cond.}` states what cannot be proven. The rejected side
# lives in `tests/nimony/errmsgs/tassertpragma.nim`.

{.feature: "staticContracts".}

import std/assertions

proc guarded(s: seq[int]; i: int): int =
  result = -1
  if i >= 0 and i < s.len:
    {.assert: i + 1 <= s.len.}
    result = s[i]

proc chained(x: int) =
  if x > 10:
    {.assert: x > 5.}
    {.assert: x >= 6 and 0 < x.}

proc assumedLength(s: seq[int]): int =
  # The caller's word that `s` is non-empty; nothing checks it.
  {.assume: s.len > 0.}
  result = s[0]

proc assumptionFeedsAnAssertion(i: int) =
  {.assume: i >= 0 and i < 4.}
  {.assert: i <= 3.}

assert guarded(@[7, 8], 1) == 8
assert guarded(@[7, 8], 2) == -1
chained(11)
assert assumedLength(@[9]) == 9
assumptionFeedsAnAssertion(2)
