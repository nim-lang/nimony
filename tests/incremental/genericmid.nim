## Imports `genericdep.nim` without re-exporting it; the instance of `incl`
## that `genericuser.nim` makes calls `genericdep`'s routines all the same.
import genericdep

type Wrap* = object

func incl*[T](w: var Wrap; x: T): bool {.inline.} =
  putIt(x)
