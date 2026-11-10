# A `func` with a `var` argument may write behind a `ref` reached from it
# (doc/language.md, "Func"), where a borrowed parameter may lie: the fact
# about `v` is gone after `touch(h)` and the index keeps its run-time check.
import std/syncio
type
  Obj = object
    n: int
  Holder = object
    r: ref Obj
func touch(h: var Holder) =
  h.r.n = 1000000000
proc g(v: var Obj; h: var Holder; arr: array[10, int]): int =
  result = 0
  if v.n >= 0 and v.n < 10:
    touch(h)
    result = arr[v.n]
var arr: array[10, int]
var r = (ref Obj)(n: 3)
var h = Holder(r: r)
echo g(r[], h, arr)
