import std/assertions

## Concept requirement stubs must win ties against equally-good real
## overloads inside a generic body so the call is deferred and re-resolved
## at instantiation with concrete argument types.

type
  Foo* = concept
    func bar(x: Self): int

func bar[T](x: T): int = 1
func bar(x: float64): int = 2

func doBar[T: Foo](x: T): int = bar(x)

assert doBar(1.0'f64) == 2
