# `delay()` with no arguments parks at the NEXT suspension point. A state with
# none behind it has nothing to park at, and that is an ordinary user mistake:
# report it at the call site instead of tripping the transform's assertion.

var saved: Continuation

proc worker() {.passive.} =
  saved = delay()

complete(delay(worker()))
{.feature: "assumeSync".}
