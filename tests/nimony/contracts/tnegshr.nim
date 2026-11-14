# The prover folds a constant `shr` of a negative value the way the program
# computes it: toward negative infinity (planetis-m/nimony-bugs-fuzzers#16).
# The false claims are in `tests/nimony/errmsgs/tnegshr.nim`.
{.feature: "staticContracts".}
import std/syncio

proc facts(): int =
  {.assert: (-1) shr 1 == -1.}
  {.assert: ((-1) shr 1) <= (-1).}
  {.assert: (-7) shr 1 == -4.}
  {.assert: (-9'i16) shr 2 == -3'i16.}
  {.assert: ashr(-8, 2) == -2.}
  var arr = [10, 20, 30, 40]
  result = arr[(-1) shr 1 + 1] + arr[7 shr 1]

echo facts()
