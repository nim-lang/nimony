# Arm's flag-setting arithmetic (`adds3`, `subs3`, …) has a value, but the row
# exists for the flags it ALSO sets: as a value it is `+`. So it is
# assembler-only, like the rows that read a flag, and the C backend says so
# rather than offering a portable spelling of the flags.

proc adds(a, b: uint64): uint64 {.instruction: "adds3".}

proc sumInAnOrdinaryProc(x, y: uint64): uint64 =
  result = adds(x, y)

discard sumInAnOrdinaryProc(1'u64, 2'u64)
