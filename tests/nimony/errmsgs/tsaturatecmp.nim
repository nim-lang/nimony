# Signed arithmetic saturates (#2840). A comparison that admits the clamped
# value says nothing about the mathematical one: `i + 1 <= n` holds for
# `i == n == high(int)`, where `i + 1 > n`. Each claim below is false for some
# input; the proven side is `tests/nimony/contracts/tsaturate.nim`.

proc leGuard(i, n: int): int =
  result = 0
  if i + 1 <= n:
    {.assert: i < n.}
    result = 1

proc eqGuard(i, n: int8): int =
  result = 0
  if n == i + 1:
    {.assert: i < n.}
    result = 1

proc subGuard(i, n: int16): int =
  result = 0
  if i - 1 >= n:
    {.assert: n < i.}
    result = 1

discard leGuard(1, 2)
discard eqGuard(1, 2)
discard subGuard(3, 1)
