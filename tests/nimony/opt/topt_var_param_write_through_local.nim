# A `var` parameter written through a local copy of its address (issue #2930).
#
# Repro:   bin/nimony c -r --opt:speed tests/nimony/opt/topt_var_param_write_through_local.nim
# Expected (see .output): 2 and 3
#
# `inc value` on a `var int` desugars to `var d = addr value[]; d[] = d[] + 1`.
# funcsummary's `isIdentityExpr` did not treat `addr value[]` as naming
# `value`'s graph, so `d` got no roots, the store through it was dropped and
# `bump` was summarized `(param 0 0 reads escapes)`: no write. copyprop
# trusted that summary and folded the read after the call to the initial `1`.

import std/syncio

proc bump(value: var int) {.noinline.} = inc value

type
  Obj = object
    a, b: int

proc bumpField(o: var Obj) {.noinline.} = inc o.b

proc main() =
  var value = 1
  bump(value)
  echo value
  var o = Obj(a: 1, b: 2)
  bumpField(o)
  echo o.b

main()
