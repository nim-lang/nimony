
import std/[syncio]

proc createCounter(): (proc(): int {.closure.}) =
  var count = 0
  result = proc(): int {.closure.} =
    inc count
    return count

var counter = createCounter()
echo counter() # 1
echo counter() # 2
echo counter() # 3

proc createNilClosure(): (proc(): int {.closure.}) =
  var count = 0
  result = nil

let _ = createNilClosure()

proc createInferCounter(): proc(): int =
  var count = 0
  result = proc(): int =
    inc count
    return count

counter = createInferCounter()
echo counter() # 1
echo counter() # 2
echo counter() # 3