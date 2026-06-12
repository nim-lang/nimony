## Generic bodies with inherited concepts (`concept of`) must pick the
## structurally matching overload, not accept a scalar type parameter when
## the argument is a generic constructor over the same type variable.

type
  PlusEq* = concept
    proc `+`*(a, b: Self): Self

  Elem* = concept of PlusEq

type Wrap*[T: Elem] = object
  v*: T

func pow*[T: Elem](base: Wrap[T], exp: Wrap[T]): Wrap[T] = base
func pow*[T: Elem](base: Wrap[T], exp: T): Wrap[T] = base

var a = Wrap[float64](v: 2.0)
var b = Wrap[float64](v: 1.0)
discard pow(a, b)
discard pow(a, 2.0)
