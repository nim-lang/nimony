type
  Foo = concept
    proc one(_: typedesc[Self]): Self
    proc `+`(a, b: Self): Self

# compiler must check against Foo, but not try to resolve T.one,
# which would result in "Error: undeclared identifier: 'one'"

template getTwo[T: Foo](t: typedesc[T]): T =
  t.one() + one(t)

# must work in inherited concepts as well

type
  Bar = concept of Foo
    proc `-`(a, b: Self): Self

template getZero[T: Bar](t: typedesc[T]): T =
  t.one() - one(t)
