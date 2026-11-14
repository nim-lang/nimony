# A local with a destructor that writes a global dies at the end of every
# iteration: the global's fact does not hold at the top of the next one and
# the index keeps its run-time check (doc/language.md, "Aliasing").
{.feature: "assumeSync".}
import std/syncio
var g = 3
type D = object
  x: int
proc `=wasMoved`(d: var D) = d.x = 0
proc `=destroy`(d: D) =
  g = 1000000000
proc h(arr: array[10, int]): int =
  result = 0
  if g >= 0 and g < 10:
    var i = 0
    while i < 3:
      result = arr[g]
      var d = D(x: 1)
      d.x = 2
      inc i
var arr: array[10, int]
echo h(arr)
