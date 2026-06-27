## Standalone concepts (no `of` parent) must check requirements on
## user `distinct` types.

type
  Comparable = concept
    func `==`(a, b: Self): bool
    func `<`(a, b: Self): bool
    func `>`(a, b: Self): bool

  Foo = distinct int

func `==`(a, b: Foo): bool = true

type Box[T: Comparable] = object
  v: T

var x: Box[Foo]
