# An `{.assembler.}` body has no C rendering: it names machine registers and
# promises a one-to-one instruction mapping, neither of which C can express. The
# C backend therefore refuses it by name rather than emitting a prototype that
# fails to link with no explanation. Reaching such a proc from a C build means
# assembling it with arkham and linking the object.

proc bsf(x: uint64): uint64 {.instruction: "bsf".}

proc firstBit(x {.register: "rdi".}: uint64): uint64 {.assembler.} =
  var r {.register: "rax".}: uint64
  r = bsf(x)
  result = r

discard firstBit(65536'u64)
