# bug #2440: `of Foo[T]` for a non-generic `Foo` must be an error, not an
# assertion failure inside the compiler.
type
  Foo = concept
    func foo(x: Self): int

  Bar = concept of Foo[T]
    func bar(x: Self): int
