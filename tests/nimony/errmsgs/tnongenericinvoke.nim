# Generic arguments for a type without generic parameters are one error at
# the invocation. An object constructor used to crash nimsem in bindInvokeArgs.
import deps / mnongeneric

type
  O = object
    x: int
  R = object
    o: O[int]
  G[T] = object
    x: T
  B = object of RootObj
    x: int
  C = object of B[int]
    y: int

discard O[int](x: 1)
discard O[](x: 1)
var v: O[int]
discard Plain[int](x: 1)
discard G[int, int](x: 1)
discard Undeclared[int](x: 1)

template t(): untyped = O[int](x: 1)
discard t()

proc p[T](a: T) =
  discard O[T](x: 1)
p(3)

var r: R
var c: C

# the arguments of a non-generic head are not checked, whatever they contain
type
  Foo = concept
    func foo(x: Self): int
  Bar = concept of Foo[seq[Undecl1]]
    func bar(x: Self): int
proc useBar(x: Bar) = discard
var w: O[array[3, Undecl2]]
