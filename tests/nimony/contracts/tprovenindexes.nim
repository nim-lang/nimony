# The positive side of array-index checking. Under `staticContracts` an index
# the prover cannot discharge is an error, so every index below is one it must
# prove — and what it proves it also strikes from the `(arrat …)` node, which
# is what stops the backend from emitting a bound check for it. The shapes here
# are the ones that carry the discharge: a literal, a guard, a loop variable,
# an offset array, and a length relation.

{.feature: "staticContracts".}

type
  Fixed = array[8, int]
  Offset = array[2..9, int]

proc literalIndex(a: Fixed): int =
  result = a[0] + a[7]

proc guardedIndex(a: Fixed; i: int): int =
  result = 0
  if 0 <= i and i < 8:
    result = a[i]

proc loopIndex(a: Fixed): int =
  result = 0
  for i in 0 ..< 8:
    result = result + a[i]

proc offsetArray(a: Offset): int =
  result = a[2] + a[9]

proc maskedIndex(a: Fixed; x: int): int =
  # `x and 7` lies in 0..7 whatever `x` is — answered structurally.
  result = a[x and 7]

proc rangeTyped(a: Fixed; i: range[0..7]): int =
  result = a[i]

var fixed: Fixed = [0, 1, 2, 3, 4, 5, 6, 7]
var offset: Offset = [2, 3, 4, 5, 6, 7, 8, 9]

discard literalIndex(fixed)
discard guardedIndex(fixed, 3)
discard loopIndex(fixed)
discard offsetArray(offset)
discard maskedIndex(fixed, 99)
discard rangeTyped(fixed, 5)
