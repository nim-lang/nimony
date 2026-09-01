## Standalone concepts check requirements on plain object types.

type
  Addable = concept
    proc `+`(a, b: Self): Self

type Plain = object
  x: int

type Box[T: Addable] = object
  v: T

var x: Box[Plain]
