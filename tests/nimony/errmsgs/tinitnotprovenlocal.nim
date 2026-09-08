# The other two ways to reach "cannot prove that ... has been initialized": a
# read of a local that is only assigned on some paths, and an `out` parameter
# left unset on some path. `tinitnotproven.nim` covers the `result` variant.
#
# The point of this test is the *name* in the message: it must be the Nim name
# the programmer wrote (`x`, `dest`), not Nimony's disambiguated symbol
# (`x.0`, `dest.0`).

proc readsPartlyInitializedLocal(cond: bool): int =
  var x: int
  if cond:
    x = 1
  result = x

proc leavesOutParamUnset(cond: bool; dest: out int) =
  if cond:
    dest = 1
