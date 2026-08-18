import std/syncio
import deps/mcrosspassive

# Regression: a `.passive` proc defined in one module, composed from a
# `.passive` driver in another. Previously failed hexer with
# `could not find symbol: pingpong`init.0.<callerModule>`.

proc driver() {.passive.} =
  echo "driver start"
  pingpong()
  let s = addUp(2, 3)
  echo "sum: ", s
  echo "driver end"

driver()
