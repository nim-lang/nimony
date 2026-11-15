# A write through a `var` result is a write to the first argument: the fact
# `x < 10` is gone and the index keeps its run-time check
# (doc/language.md, "Aliasing").
import std/syncio
proc forward(x: var int): var int = x

proc f(a: int; arr: array[10, int]): int =
  var x = a
  result = 0
  if x >= 0 and x < 10:
    forward(x) = 1000000000
    result = arr[x]

var arr: array[10, int]
echo f(3, arr)
