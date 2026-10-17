# #2674: a spliced `.raises` relay lost the error it propagated. The relay's own
# `canRaise` temp shares its name with the caller's, and the inliner took the
# relay's `ret canRaise` for a return of the splice destination, dropping the
# copy on the failure path.

import std/syncio

proc leaf(x: int) {.raises, noinline.} =
  if x < 0: raise IOError

proc relay(x: int) {.raises.} =
  leaf(x)

proc outer(x: int): int {.raises, noinline.} =
  relay(x)
  result = 42

try:
  let v = outer(-1)
  echo "BUG: error lost, outer returned ", v
except:
  echo "ok: caught"

try:
  echo outer(1)
except:
  echo "BUG: spurious error"
