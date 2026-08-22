import std/syncio
import deps/mcrosspassive

# Regression: a `.passive` proc defined in one module, driven from a
# `.passive` proc in another. Used to fail hexer with
# `could not find symbol: pingpong`init.0.<callerModule>` — the helper was
# named for the transforming module rather than the defining one, and even
# once named right, the caller's hexer run has no way to load a signature
# that only ever existed in the defining module's run.

proc driver() {.passive.} =
  echo "driver start"
  pingpong()
  let s = addUp(2, 3)
  echo "sum: ", s
  echo "driver end"

driver()
