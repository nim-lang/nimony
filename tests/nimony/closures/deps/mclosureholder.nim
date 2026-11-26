type Holder* = object
  f*: proc (n: int): int {.closure.}

proc callHere*(h: Holder; n: int): int = h.f(n)
