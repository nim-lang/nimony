# `.requires` is discharged at the *call site* (`contracts_fir.checkRequires`).
# Only a contract whose negation follows from what is known here is reported by
# default; an undecided one is left to the runtime guard hexer emits into the
# callee. The proven side lives in `tests/nimony/contracts/tprovencontracts.nim`.

proc needsPositive(x: int) {.requires: x > 0.} =
  discard x

proc literal =
  needsPositive(0)

proc knownValue =
  var y = -4
  needsPositive(y)

proc viaGuard(z: int) =
  if z <= 0:
    needsPositive(z)

proc disequalityDecidesNothing(w: int) =
  # `w != 3` does NOT mean `w >= 4`, so this call is neither proven nor
  # disproven and nothing is reported for it. Reading a disequality as one
  # half of itself used to "prove" contracts on paths where they were false.
  if w != 3:
    needsPositive(w)

literal()
knownValue()
viaGuard(1)
disequalityDecidesNothing(1)
