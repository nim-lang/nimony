# Only a *resolved direct call* the prover discharged may skip a `.requires`
# guard: the guard-free copy of a routine has a name nothing else can spell.
# A proc value, a callback, a vtable slot can only name the routine itself —
# which is the guarded copy. So this call through a proc value must still
# fail at run time, even though the direct call before it was proven and went
# to the guard-free copy.

import std / syncio

proc half(x: int): int {.requires: x > 0.} =
  result = x div 2

echo half(10)
flushFile(stdout)
let f: proc (x: int): int {.nimcall.} = half
echo f(-4)
