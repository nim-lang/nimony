# Bug report 87: a write through `addr x` changes `x`, so the fact `x < 10`
# the guard established is gone and the index keeps its run-time check
# (doc/language.md, "Aliasing").
import std/syncio
proc f(a: int; arr: array[10, int]): int =
  var x = a
  let p = addr x
  result = 0
  if x >= 0 and x < 10:
    p[] = 1000000000
    result = arr[x]
var arr: array[10, int]
echo f(3, arr)
