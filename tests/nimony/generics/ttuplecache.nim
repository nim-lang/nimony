# issue #692

type
  Foo[T] = object
    data: T

# Compiles without errors.
proc initFooInt(): Foo[int] =
  Foo[int](data: 0)

proc initFooTup(): Foo[(int, int)] =
  # Error: type mismatch
  Foo[(int, int)](data: (0, 0))
