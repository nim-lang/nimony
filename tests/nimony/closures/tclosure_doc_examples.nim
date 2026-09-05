## `doc/language.md`'s "Closures" examples in the default dialect: a proc
## literal that captures a local must be annotated `.closure.` itself, the
## `.closure.` proc type of the assignment target does not imply it.
## `tautoclosures.nim` is the same program under `.feature: "autoclosures".`.

import std/syncio

proc createAdder(x: int): (proc(y: int): int {.closure.}) =
  result = proc(y: int): int {.closure.} =
    return x + y

let add5 = createAdder(5)
let add10 = createAdder(10)
echo add5(3)  # 8
echo add10(3) # 13

proc createAccumulator(): (proc(x: int): int {.closure.}) =
  var total = 0
  result = proc(x: int): int {.closure.} =
    total += x
    return total

let acc = createAccumulator()
echo acc(5)  # 5
echo acc(3)  # 8
echo acc(7)  # 15
