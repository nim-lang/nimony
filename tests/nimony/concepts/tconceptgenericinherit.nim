import std/assertions

## Generic concepts may inherit from other generic concepts via `(at Parent T)`.

type
  Foo*[T] = concept
    func foo(x: Self): T

  Bar*[T] = concept of Foo[T]
    func bar(x: Self): T

func foo*(x: int): int = x
func bar*(x: int): int = x

when int is Bar:
  type CheckIntSatisfiesBar = int
else:
  {.error: "int must satisfy Bar including inherited Foo requirements".}

proc useBar*[T: Bar](x: T): T =
  discard x.foo()
  discard x.bar()
  x

assert useBar(3) == 3
