import std/syncio

proc outer(n: int) =
  var m = 3
  let f = proc (x = n; y = m): int = x * 10 + y
  m = 4
  echo f()
  echo f(5)
  # a default that is a capturing closure
  proc g(cb: proc (): int {.closure.} = (proc (): int {.closure.} = n)): int = cb()
  echo g()

outer(2)
