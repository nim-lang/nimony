type
  Partial = object
    x: int

type
  Base = concept
    proc `-`(a, b: Self): Self
    proc `+`(a, b: Self): Self

  NeedsOps = concept of Base

type Box[T: NeedsOps] = object
  v: T

var x: Box[Partial]
