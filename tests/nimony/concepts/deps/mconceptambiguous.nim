## Redeclares `Equatable`, which `system` also exports (nim-lang/nimony#2260).

type
  Equatable* = concept
    func `==`(a, b: Self): bool
