# A generic type name without type arguments is not a concrete type.
type
  Base[T] = object of RootObj
    x: T
  D = object of Base
  R = ref object of Base
  Ok = object of Base[int]
  G[T] = object of Base[T]
  N[T] = object
    next: ptr N
  F = object
    f: Base

var v: Base
var s: seq[(Base, int)]
proc p(x: Base) = discard
proc q(): Base = discard
proc gp[T](x: T; y: Base) = discard
var cs = cast[Base](0)
# inside a composite constraint `Base` is not a type class either
type HasBase = concept
  proc get(x: Self): Base
proc n3[T: ptr Base; U: (Base, int)](x: T) = discard

# a constraint, an `is` test and an alias name the generic itself:
proc c[T: Base | int](x: T) = discard
c(Base[int](x: 1))
type NB = not Base
proc n1[T: not Base](x: T) = discard
proc n2[T: NB](x: T) = discard
n1(3)
n2(3)
const isBase = int is Base
type B = Base
var b: B[int]
