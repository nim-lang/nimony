import std/syncio

# Repro: an early `return` past a suspension point (a call to another
# .passive proc) inside a `while` loop fails to lower in hexer:
#   [Error] unknown statement: (not ...)
# Rewriting the loop single-exit (`while ok and k < n`) compiles fine, so
# only this control-flow shape is affected. Real-world: every WS/HTTP
# receive loop wants `while open: ... if not send(...): return`.

proc inner(x: int): bool {.passive.} =
  result = x < 3

proc outer(n: int): int {.passive.} =
  result = 0
  var k = 0
  while k < n:
    let ok = inner(k)   # suspension point inside the loop
    if not ok:
      return k          # early return past it -> hexer "unknown statement"
    k = k + 1
  result = -1

echo outer(10)
