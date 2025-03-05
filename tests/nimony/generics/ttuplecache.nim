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

proc initFooGeneric[T](x: T): Foo[T] =
  Foo[T](data: x)

let x = initFooGeneric((a: 1, b: "abc"))
let x1: int = x.data.a
let x2: string = x.data.b

let y = initFooGeneric((x: "def", y: 2))
let y1: string = y.data.x
let y2: int = y.data.y
