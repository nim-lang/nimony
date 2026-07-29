# §4.1: the operand roles determine the signature, with no author choice. A
# two-address instruction reads AND writes its first operand, and `var` is the
# only thing that says so — it is also what makes the call site emit `(haddr d)`,
# the tag that tells the back end to bind d's location rather than materialise a
# pointer to it. So the rule runs in both directions, and both are checked at the
# DECLARATION rather than wherever the instruction is eventually used.

proc missingVar(d: uint64; s: uint64) {.instruction: "add".}

proc spuriousVar(d: var uint64; s: var uint64) {.instruction: "add".}

proc notTwoAddress(x: var uint64): uint64 {.instruction: "bsf".}
