# A value parameter passed by reference is the caller's storage, here a
# global a `proc` changes during the call: its fact does not outlive the call
# and the index keeps its run-time check (doc/language.md, "Aliasing").
import std/syncio
{.feature: "assumeSync".}
type Big = object
  n: int
  pad: array[16, int]
var g = Big(n: 3)
var arr: array[10, int]
proc clobber() =
  g.n = 1000000000
proc f(a: Big): int =
  if a.n >= 0 and a.n < 10:
    clobber()
    result = arr[a.n]
  else:
    result = 0
echo f(g)
