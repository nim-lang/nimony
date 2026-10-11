# `{.assert: cond.}` is judged at compile time and only there: nothing is
# emitted for it at run time, so a claim the prover does not prove is an error —
# an undecided one exactly like a violated one, and without any contract
# feature. `{.assume: cond.}` is the override. The proven side lives in
# `tests/nimony/contracts/tassertpragma.nim`.

proc violated(x: int) =
  if x > 5:
    {.assert: x < 3.}

proc undecided(s: seq[int]; i: int): int =
  {.assert: i >= 0 and i < s.len.}
  result = 0

proc assumedIsNotAsserted(i: int) =
  {.assume: i > 0.}
  {.assert: i > 1.}

violated(7)
discard undecided(@[1], 0)
assumedIsNotAsserted(2)
