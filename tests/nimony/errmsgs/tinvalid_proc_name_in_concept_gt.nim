type
  Comparable = concept
    func `>`(x, y: Self): bool

  Foo = distinct int

type Box[T: Comparable] = object
  v: T

var x: Box[Foo]
