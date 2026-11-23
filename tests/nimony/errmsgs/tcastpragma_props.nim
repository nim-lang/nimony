## Verifies that `{.cast(uncheckedAssign).}` does NOT suppress the
## side-effect check (only `{.cast(noSideEffect).}` is allowed to do that).
## Also verifies that `{.cast(noSideEffect).}` itself works.
var counter = 0

proc bumps() =
  counter += 1

func ok() =
  {.cast(noSideEffect).}:
    bumps()

func bad() =
  {.cast(uncheckedAccess).}:
    bumps()

{.feature: "assumeSync".}  # test program: globals shared freely

proc noisy(): bool =
  bumps()
  result = true

func discards(): int =
  discard noisy() # a discarded call is checked like any other (#341)
  result = 1

iterator walk(): int =
  discard noisy()
  yield 1
