# `{.assembler.}` checking is DELEGATED to the back end (arkham), which owns the
# machine model — NIF carries the original line info, so its diagnostics are as
# precise as a front-end pass would give (see nativenif/doc/intrinsics.md §8).
# What sem still owns is the shape of the pragmas themselves; that is what this
# tests, plus the C backend's refusal to compile a body only an assembler can.

var notARoutine {.assembler.}: int

proc registerNeedsAName(x: uint64): uint64 {.assembler.} =
  var r {.register.}: uint64
  r = x
  result = r
