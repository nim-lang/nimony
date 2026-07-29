# The declaration IS the typing contract: the row in `lib/intrinsics` is unified
# against the signature once, at the declaration, so nothing downstream needs a
# rule per opcode. Every mismatch below is caught here, not three passes later.

proc unknownOpcode(x: uint64): int32 {.intrinsic: "NoSuchThing".}

proc wrongOperand(x: float): int32 {.intrinsic: "Ctz".}

proc wrongResult(x: uint64): uint64 {.intrinsic: "Ctz".}

proc unsupportedWidth(x: uint8): int32 {.intrinsic: "Ctz".}

proc wrongArity(x, y: uint64): int32 {.intrinsic: "Ctz".}
