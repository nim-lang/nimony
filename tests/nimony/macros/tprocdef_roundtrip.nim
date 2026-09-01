# A macro must be able to receive a `proc` declaration and return it.
#
# The NIF <-> NimNode codec (lib/std/private/macros_nif.nim) had no `"proc"`
# case, so the declaration arrived as `nnkNone` and was re-serialized as an
# empty node: the routine vanished, and the only symptom was an unrelated
# "undeclared identifier" at the call site.
#
# The identity macro below exercises BOTH directions of the codec in one go —
# `(proc ...)` -> nnkProcDef on the way in, nnkProcDef -> `(proc ...)` on the
# way out — so either mapping regressing fails this test.
import std / [syncio, macros]

macro identity(prc: untyped): untyped =
  result = prc

proc greet(name: string) {.identity.} =
  echo "hello, " & name

greet("world")
