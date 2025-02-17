proc foo[T](x: T) = discard
proc foo[T, U](x: T, y: U) = discard

discard foo[int]
discard foo[int, string]
foo[int](123)
foo[int, string](123, "abc")

import deps/mgenericproc

discard importedGeneric[int](123)
