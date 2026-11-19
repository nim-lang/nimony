# `nimony check` reports what the prover rejects, exactly as `nimony c` does:
# a `{.assert.}` it can disprove and one it cannot prove are both errors.
# Where the prover runs is not part of this file's contract; that `check`
# keeps reporting these is.

proc violated(x: int) =
  if x > 5:
    {.assert: x < 3.}

proc undecided(s: seq[int]; i: int): int =
  {.assert: i >= 0 and i < s.len.}
  result = 0

violated(7)
discard undecided(@[1], 0)
