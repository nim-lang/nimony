## Test that the `.used.` pragma is accepted (for Nim source compatibility)
## on types, constants, routines and locals. It is semantically ignored, so
## the test merely needs to compile and run.

import std/assertions

type
  Color {.pure, used.} = object
    r, g, b: uint8

  Handle {.used.} = distinct int

const
  Mask {.used.} = 0x80'u32

proc cAbs(x: cint): cint {.cdecl, importc: "abs", used.}

proc unusedHelper() {.used.} =
  discard

var unusedGlobal {.used.} = 42

proc main =
  var localUnused {.used.} = 7
  assert cAbs(-5.cint) == 5.cint

main()
