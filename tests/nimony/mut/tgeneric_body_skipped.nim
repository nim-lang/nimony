# issue #2704: a module-level `for` over an array literal must not make the
# dangerous-location check walk uninstantiated generic routine bodies.

import std / [assertions, syncio]

type Equatable = concept
  func `==`*(a, b: Self): bool

const zero = 0

func foo*[T: Equatable, P: static[int]](x: T): T = x

func bar*[T: Equatable](x: T): T = foo[T, zero](x)

proc check*[T: Equatable](v: T) =
  assert bar(v) == foo[T, 0](v)

block repro:
  for v in [0.1'f64]:
    check(v)
    echo v
