import std/assertions

## Generic concept requirements with Self must survive instantiation.
## Regression for undeclared instantiated Self syms in requirement signatures.

type
  HasNext = concept
    proc next(rng: var Self): uint64

  HasSample*[T] = concept
    proc sample[G: HasNext](s: Self, g: var G): T

type
  MySampler = object
    x: uint64

proc sample[G: HasNext](s: MySampler, g: var G): uint64 =
  discard g.next()
  s.x

assert MySampler is HasSample[uint64]
