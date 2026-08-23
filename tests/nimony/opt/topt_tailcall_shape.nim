# `(var :t T (call …)) (ret t)` folds to `(ret (call …))` — the tail-call encoding.
#
# Repro:   bin/nimony c -r -d:danger tests/nimony/opt/topt_tailcall_shape.nim
# Expected (see .output): 13 10 55 x 3 4
#
# The fold is in copyprop (`foldTailCalls`), which by then has already collapsed
# `result` away, so a proc whose body is one call arrives as exactly those two
# statements. What this test pins is that folding does not change what any of
# them COMPUTES — including the two shapes that must not fold at all:
#
#   `tailWithLocal` has a `=destroy` of a live local between the call and the
#   return, which is exactly what makes it not a tail call; the fold's adjacency
#   rule is what excludes it.
#   `twoReads` reads the temp twice, so folding it into the `ret` would drop a
#   read; the one-read rule excludes it.
#
# `tailCond` (a call in one branch, the return outside) is not adjacent either
# and stays as it is — recorded here so a later branch-sinking change has to
# come past this test with the answer unchanged.

import std / syncio

proc callee(a, b: int): int {.noinline.} = a * b + 1

proc tailInt(a, b: int): int {.noinline.} = callee(a, b)

proc mkStr(n: int): string {.noinline.} = "x"

proc tailStr(n: int): string {.noinline.} = mkStr(n)      # destructible result

proc tailCond(a, b: int): int {.noinline.} =
  if a > b: callee(a, b) else: a

proc selfRec(n, acc: int): int {.noinline.} =
  if n == 0: acc else: selfRec(n - 1, acc + n)

proc tailWithLocal(n: int): int {.noinline.} =
  ## a destructor runs between the call and the return: NOT a tail call
  let s = mkStr(n)
  callee(n, s.len)

proc twoReads(a, b: int): int {.noinline.} =
  ## the temp is read twice: must not be folded into the `ret`
  let t = callee(a, b)
  if t > 100: return t
  t

proc main =
  echo tailInt(3, 4), " ", tailCond(9, 1), " ", selfRec(10, 0), " ",
       tailStr(1), " ", tailWithLocal(2), " ", twoReads(1, 3)

main()
