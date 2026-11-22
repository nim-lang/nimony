# `check --hexer` reports what the cps transform reports, not only what the
# duplifier reports: the lowering stops after the last pass that can report.

var saved: Continuation

proc worker() {.passive.} =
  saved = delay()

complete(delay(worker()))
{.feature: "assumeSync".}
