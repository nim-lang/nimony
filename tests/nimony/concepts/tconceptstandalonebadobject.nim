## Standalone concepts must check requirements on plain object types,
## not only on ``distinct`` types.

type
  Addable = concept
    proc `+`(a, b: Self): Self

type Plain = object
  x: int

type Box[T: Addable] = object
  v: T

var x: Box[Plain]
