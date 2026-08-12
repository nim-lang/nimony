# `{.alwaysInline.}` is spliced at every call site regardless of size or
# per-call-site score, and `{.noinline.}` is never spliced. Inlining is not
# observable in the result, so what this pins down is that both pragmas parse,
# survive to NIFC and do not change the program's meaning — the *effect* is
# checked by hand against `nifasm --symmap` (the wrapper disappears from the
# symbol map, the cold tail stays).
#
# The point of the pair: `hotEntry` is far past `InlineTinyBound` once hexer has
# flattened it, and would never be inlined on size. `coldTail` is what keeps it
# from growing further — without `{.noinline.}` the tail gets spliced INTO the
# wrapper, and then the wrapper is not worth inlining anymore.
import std / syncio

proc coldTail(x: int): int {.noinline.} =
  result = 0
  var i = 0
  while i < x:
    result = result + i * 3 - (i div 2)
    if result > 100000: result = result mod 97
    inc i
  result = result + x * 7

proc hotEntry(x: int): int {.alwaysInline.} =
  if x > 1000: result = coldTail(x)
  elif x > 100: result = x * 2 + 1
  elif x > 10: result = x + 5
  else: result = x

# Both pragmas on one proc is a contradiction; `.noinline` wins (refusing is the
# safe reading), which must still compile and run.
proc contradictory(x: int): int {.alwaysInline, noinline.} =
  result = x * 3

proc main =
  var total = 0
  var i = 0
  while i < 50:
    total = total + hotEntry(i)
    inc i
  total = total + hotEntry(500) + hotEntry(2000)
  total = total + contradictory(11)
  echo total

main()
