import std/assertions

type
  Foo = concept
    proc `+`(a, b: Self): Self

  Bar = concept of Foo
    proc zero(_: typedesc[Self]): Self

template zero*[T: SomeInteger](_: typedesc[T]): T = 0.T

assert int is Foo
assert int is Bar
assert typedesc[int8] is Bar
