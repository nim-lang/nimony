# `(-2) shr 2` is `-1`: a signed `shr` rounds toward negative infinity
# (planetis-m/nimony-bugs-fuzzers#16). The prover must not read it as `0`.
# The true claims are in `tests/nimony/contracts/tnegshr.nim`.

{.feature: "staticContracts".}
proc f(): bool =
  let k = (-2) shr 2
  {.assert: k >= 0 and k <= 3.}
  result = true

proc h(): int =
  var arr = [10, 20, 30, 40]
  result = arr[(-2) shr 2]

proc takesRT(x: range[0..8]): int = x

proc g(): int = takesRT((-2) shr 2)

discard f()
discard h()
discard g()
