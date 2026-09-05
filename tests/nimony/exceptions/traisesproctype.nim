# A `.raises` routine RETURNS `(ErrorCode, T)` — `ErrorCode` alone when it has
# no value. `raiselowering` puts that in the signature, because `cps` builds a
# coroutine's frame and result slot out of the return type and cannot wait for
# codegen to say what it is.
#
# A PROCTYPE is the one shape that lowering does not reach: a foreign
# `type Fn = proc (): int {.raises.}` is pulled into the output as a type
# declaration, not as code, so no pass in the pipeline ever walks it and
# `lengcgen` still has to rewrite it. This file pins both ends — if the two
# ever disagree, the function pointer and the function have different types
# and nothing but the C compiler notices.

import std / syncio
import raiseslib

type LocalFn = proc (x: int): int {.raises.}

proc localInt(x: int): int {.raises.} =
  if x < 0: raise SyntaxError
  result = x * 2

proc viaLocal(f: LocalFn; x: int) =
  try:
    let v = f(x)
    echo "local fn ", v
  except:
    echo "local fn caught"

proc viaForeign(f: ForeignFn; x: int) =
  try:
    let v = f(x)
    echo "foreign fn ", v
  except:
    echo "foreign fn caught"

viaLocal(localInt, 5)
viaLocal(localInt, -1)
viaForeign(foreignInt, 5)
viaForeign(foreignInt, -1)

# and a direct cross-module call, value-returning and void
proc direct() =
  try:
    let a = foreignInt(4)
    echo "direct ", a
    foreignVoid(-1)
    echo "unreachable"
  except:
    echo "direct caught"
direct()
