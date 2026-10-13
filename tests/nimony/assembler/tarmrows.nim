# The Arm rows (adds3 … cselhs, umull, umlal) are checked at the declaration
# like every other row: the operand roles fix the signature. The first block
# is well-formed and compiles on any backend (nothing calls it); the second
# breaks one rule each.

proc adds3(a, b: uint64): uint64 {.instruction: "adds3".}
proc adcs3(a, b: uint64): uint64 {.instruction: "adcs3".}
proc subs3(a, b: uint32): uint32 {.instruction: "subs3".}
proc sbcs3(a, b: uint32): uint32 {.instruction: "sbcs3".}
proc mul3(a, b: uint64): uint64 {.instruction: "mul3".}
proc umulh(a, b: uint64): uint64 {.instruction: "umulh".}
proc umull(lo, hi: var uint32; a, b: uint32) {.instruction: "umull".}
proc umlal(lo, hi: var uint32; a, b: uint32) {.instruction: "umlal".}
proc bic3(a, b: uint32): uint32 {.instruction: "bic3".}
proc cselhs(a, b: uint64): uint64 {.instruction: "cselhs".}

proc umullNotVar(lo, hi: uint32; a, b: uint32) {.instruction: "umull".}
proc umulhNarrow(a, b: uint32): uint32 {.instruction: "umulh".}
proc umlalBadArity(lo: var uint32; a, b: uint32) {.instruction: "umlal".}
