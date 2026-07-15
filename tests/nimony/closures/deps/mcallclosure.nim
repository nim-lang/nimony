import std/assertions

proc callClosure*(clsr: proc (x: int): int {.closure.}) =
  assert clsr(123) == 369
