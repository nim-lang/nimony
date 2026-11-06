# `len(s) + MaxContainerLen + 1` can be `high(int)`: `len`'s `.ensures`
# bounds it by `MaxContainerLen`, no tighter, so the loop up to it may not end.
# The provable side is `tests/nimony/contracts/tstringhigh.nim`.

{.feature: "staticContracts".}

func past(s: string): int = len(s) + (MaxContainerLen + 1)

proc loop(s: string) =
  for i in 0 .. past(s): discard
