## Concept verdicts are cached per concrete type: two instances of the same
## generic must not share an entry.

import std/assertions

type
  Box[T] = object
    v: T
  Showable = concept
    func show(x: Self): string

func show(x: Box[int]): string = "int"

assert Box[int] is Showable
assert not (Box[string] is Showable)
assert Box[int] is Showable
