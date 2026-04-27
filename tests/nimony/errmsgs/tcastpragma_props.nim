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
  {.cast(uncheckedAssign).}:
    bumps()
