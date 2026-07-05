# Module-level globals as fixed memory slots (zero-initialised — linear memory
# starts at 0). State persists across exported calls in the shared buffer, just
# as it does in the JS backend's `mem`.

var counter: int32
var total: int32

proc bump(): int32 {.exportc.} =
  counter = counter + 1'i32
  total = total + counter
  result = counter

proc getTotal(): int32 {.exportc.} =
  result = total
