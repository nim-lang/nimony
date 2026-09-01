## Standalone concepts (no `of` parent) check requirements on user `distinct`
## types. A `distinct string` does not inherit `<` from `string`; `>` is
## provided by the `untyped` template in system, so it is not reported.

type
  Comparable = concept
    func `==`(a, b: Self): bool
    func `<`(a, b: Self): bool
    func `>`(a, b: Self): bool

  Foo = distinct string

func `==`(a, b: Foo): bool = true

type Box[T: Comparable] = object
  v: T

var x: Box[Foo]
