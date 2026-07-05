# Compute-only WASM backend test: arithmetic, control flow, calls, recursion,
# int64 and float — no linear memory. Each `exportc` proc becomes a WASM export
# the driver (`tcompute.js`) calls.

proc add2(a, b: int32): int32 {.exportc.} =
  result = a + b

proc fib(n: int32): int32 {.exportc.} =
  if n < 2'i32:
    return n
  return fib(n - 1'i32) + fib(n - 2'i32)

proc sumTo(n: int32): int32 {.exportc.} =
  result = 0'i32
  var i = 0'i32
  while i <= n:
    result = result + i
    i = i + 1'i32

proc arith(a, b: int32): int32 {.exportc.} =
  result = a * b + (a div b) - (a mod b)

proc countdown(n: int32): int32 {.exportc.} =
  # `while true` + `break`: hexer lowers the break to a forward `jmp` to a `lab`
  # after the loop, which the backend maps to a WASM `block` + `br`.
  result = 0'i32
  var i = n
  while true:
    if i == 0'i32:
      break
    result = result + i
    i = i - 1'i32

proc mul64(a, b: int64): int64 {.exportc.} =
  result = a * b

proc fcombine(a, b: float64): float64 {.exportc.} =
  result = a * b + a
