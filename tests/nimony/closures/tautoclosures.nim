## `doc/language.md`'s "Closures" examples in their Nim-2 spelling: under
## `.feature: "autoclosures".` a routine nested in another routine is a
## closure implicitly, so the proc literals need no `.closure.` annotation.
## The proc *types* still spell it out, that is not part of the feature.
{.feature: "autoclosures".}

import std/syncio

proc createCounter(): (proc(): int {.closure.}) =
  var count = 0
  result = proc(): int =
    inc count
    return count

let counter = createCounter()
echo counter() # 1
echo counter() # 2
echo counter() # 3

proc createAdder(x: int): (proc(y: int): int {.closure.}) =
  result = proc(y: int): int =
    return x + y

let add5 = createAdder(5)
let add10 = createAdder(10)
echo add5(3)  # 8
echo add10(3) # 13

proc createAccumulator(): (proc(x: int): int {.closure.}) =
  var total = 0
  result = proc(x: int): int =
    total += x
    return total

let acc = createAccumulator()
echo acc(5)  # 5
echo acc(3)  # 8
echo acc(7)  # 15
