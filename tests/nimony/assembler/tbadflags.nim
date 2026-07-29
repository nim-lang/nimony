# A flag instruction reaching the C backend. There is no portable rendering and
# never will be: C has no condition codes, and what makes a flag usable at all —
# that nothing runs between the instruction that defines it and the test that
# reads it — is exactly what a C compiler will not promise. So the rejection
# names that rather than suggesting the `{.intrinsic.}` form, which for a flag
# does not exist.
#
# The rules that govern flags where they DO work are arkham's, and are checked
# there: `nativenif/tests/arkham/err_flag_*.c.nif`. See
# `nativenif/doc/intrinsics.md` §6.

proc cmp64(a, b: uint64) {.instruction: "cmp".}
proc zf(): bool {.instruction: "zf".}

proc flagsInAnOrdinaryProc(x: uint64): uint64 =
  cmp64(x, x)
  if zf(): result = 1
  else: result = 2

discard flagsInAnOrdinaryProc(1'u64)
