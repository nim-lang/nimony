## A candidate whose own constraint is the concept being checked must not
## satisfy it by itself: `Bar <= Bar` only resolves to the generic `<=`,
## which requires `Bar` to be `Orderable2` in the first place.

import std/assertions

type
  Orderable2 = concept
    proc `<=`(x, y: Self): bool
  Bar = object

proc `<=`[T: Orderable2](x, y: T): bool = true

assert not (Bar is Orderable2)
assert int is Orderable2
assert string is Orderable2
