# Signed arithmetic wraps (doc/language.md "Signed overflow"), so the prover
# reads it mathematically only where the facts keep it inside its type. Each
# claim below is false for some input: `127'i8 + 1'i8` is `-128'i8` (#2840),
# and `i + 1 < n` holds for `i == high(int)`. The proven side is
# `tests/nimony/contracts/tsignedwrap.nim`.

proc sum8(a, b: int8): int =
  result = 0
  if a >= 0 and b >= 0:
    {.assert: a + b >= 0.}
    result = int(a + b)

proc ltGuard(i, n: int): int =
  result = 0
  if i + 1 < n:
    {.assert: i < n - 1.}
    result = 1

proc subGuard(i, n: int16): int =
  result = 0
  if i - 1 > n:
    {.assert: n < i.}
    result = 1

discard sum8(1, 2)
discard ltGuard(1, 5)
discard subGuard(3, 1)
