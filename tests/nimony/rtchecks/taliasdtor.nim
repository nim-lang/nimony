# Leaving the scope of a local with a destructor runs it, a call to a `proc`,
# which may write behind a `ref` where a borrowed parameter lies: the index
# keeps its run-time check (doc/language.md, "Aliasing").
import std/syncio
type
  Obj = object
    n: int
  D = object
    r: ref Obj
proc `=wasMoved`(d: var D) = d.r = nil
proc `=destroy`(d: D) =
  if d.r != nil: d.r.n = 1000000000
proc g(v: var Obj; r: ref Obj; arr: array[10, int]): int =
  result = 0
  if v.n >= 0 and v.n < 10:
    block:
      var d = D(r: r)
      discard d
    result = arr[v.n]
var arr: array[10, int]
var r1 = (ref Obj)(n: 3)
let r2 = r1
echo g(r1[], r2, arr)
