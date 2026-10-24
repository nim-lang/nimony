# issue #2724: the generic arguments of a base type must match
type
  Parent[T] = object of RootObj
    v: T
  Child = object of Parent[int]

proc foo(x: var Parent[string]) =
  x.v = "hello"

var s = Child()
foo(s)
discard s.v
