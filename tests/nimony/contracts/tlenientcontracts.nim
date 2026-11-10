# `lenientContracts`: the module's author vouches that a write which does not
# name a variable leaves it alone (doc/language.md, "Aliasing"), so the prover
# keeps its facts across such writes. Without the feature every index below
# is an error in a `staticContracts` module (errmsgs/taliasprover.nim).
{.feature: "staticContracts".}
{.feature: "lenientContracts".}
{.feature: "assumeSync".}
import std/syncio

type
  Obj = object
    n: int

var g = 3
var other = 0
var arr: array[10, int] = [0, 1, 2, 3, 4, 5, 6, 7, 8, 9]
proc elsewhere() = other = 1

proc addrTaken(a: int): int =
  var x = a
  var y = 0
  let p = addr x
  let q = addr y
  result = p[]
  if x >= 0 and x < 10:
    q[] = 1
    result = arr[x]

proc globalAcrossCall(): int =
  result = 0
  if g >= 0 and g < 10:
    elsewhere()
    result = arr[g]

proc borrowedAcrossCall(v: var Obj): int =
  result = 0
  if v.n >= 0 and v.n < 10:
    elsewhere()
    result = arr[v.n]

echo addrTaken(4)
echo globalAcrossCall()
var o = Obj(n: 5)
echo borrowedAcrossCall(o)
