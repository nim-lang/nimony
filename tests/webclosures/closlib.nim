## The closure type lives here so the main module's calls see it as an
## imported proctype; `callHere` calls it from its own module.
type Holder* = object
  f*: proc (n: int): int {.closure.}

proc callHere*(h: Holder; n: int): int = h.f(n)
