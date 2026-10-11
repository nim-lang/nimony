## A generic instance satisfies an atomic concept through *generic* routines
## over the generic type (issue #2500): probing `func `+`[T](a, b: Box[T])`
## for `Box[int]` binds `T = int`, and its result `Box[T]` instantiates to the
## same `Box[int]` the requirement's `Self` names. A candidate constrained by
## the concept being checked still counts, because with `T = int` it asks
## `int is Additive` — a different question, not the one under construction.

import std/assertions

type
  Additive = concept
    func `+`(a, b: Self): Self
    func zero(_: typedesc[Self]): Self

template zero*(_: typedesc[int]): int = 0

assert int is Additive

type
  Box[T: Additive] = object
    v: T

func `+`*[T: Additive](a, b: Box[T]): Box[T] = Box[T](v: a.v + b.v)
template zero*[T: Additive](_: typedesc[Box[T]]): Box[T] = Box[T](v: T.zero)

assert Box[int] is Additive
assert Box[Box[int]] is Additive
assert not (Box[string] is Additive)

proc sum[T: Additive](xs: openArray[T]): T =
  result = T.zero
  for x in xs: result = result + x

assert sum([Box[int](v: 1), Box[int](v: 2), Box[int](v: 3)]).v == 6
