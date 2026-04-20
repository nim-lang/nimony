
import std/[syncio]

proc createCounter(): (proc(increment: int): int {.closure.}) =
  var count = 0
  result = proc(increment: int): int {.closure.} =
    inc count, increment
    return count

let counter = createCounter()
echo counter(3) # 3
echo counter(2) # 5
echo counter(1) # 6
