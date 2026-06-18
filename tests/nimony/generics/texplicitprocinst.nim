proc foo[T](x: T) = discard
proc foo[T, U](x: T, y: U) = discard

discard foo[int]
discard foo[int, string]
foo[int](123)
foo[int, string](123, "abc")

import deps/mgenericproc

discard importedGeneric[int](123)

# an explicitly instantiated generic routine is a first-class proc value, so its
# type must be the full proctype, not just the bare params subtree:
proc takesProc(x: proc (x: int)) = discard
proc generic[T](x: T) = discard
takesProc(generic[int])
