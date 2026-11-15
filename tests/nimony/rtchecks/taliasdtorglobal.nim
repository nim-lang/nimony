# A destructor that writes a global: the global's fact is gone at the scope
# exit and the index keeps its run-time check (doc/language.md, "Aliasing").
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
    block:
      var d = D(x: 1)
      d.x = 2
    result = arr[g]
var arr: array[10, int]
echo h(arr)
