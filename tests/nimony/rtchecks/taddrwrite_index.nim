# A write through `addr x` changes `x`: the guard's facts about `x` are gone,
# so `arr[x]` keeps its run-time check (it was proven and removed, and the
# program read far past `arr`).

import std/syncio

proc f(a: int; arr: array[10, int]): int =
  var x = a
  if x >= 0 and x < 10:
    let p = addr x
    p[] = 1000000000
    result = arr[x]
  else:
    result = 0

var arr: array[10, int]
echo f(3, arr)

# Two `var` parameters that are one location, `f(s[i], s[j])` with `i == j`
# or `f(r1.v, r2.v)` with `r2 = r1`, are for the call-site aliasing check to
# reject (doc/language.md, "Call-site aliasing checks"); the prover relies on
# it and assumes them distinct. Until that check catches these two calls,
# they are kept here, disabled, for its fix:
#
#   proc g(a: var int; b: var int; arr: array[10, int]): int =
#     result = 0
#     if a >= 0 and a < 10:
#       b = 1000000000
#       result = arr[a]
#   var s = @[3, 4]
#   var i = 0
#   var j = 0
#   echo g(s[i], s[j], arr)
#   type O = ref object
#     v: int
#   let r1 = O(v: 3)
#   let r2 = r1
#   echo g(r1.v, r2.v, arr)
