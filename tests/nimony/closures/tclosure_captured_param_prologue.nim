# a closure's PARAMETER captured by a nested closure, where the outer
# closure itself receives an env (nested closure-of-closure shape)
import std/syncio

proc outer() =
  var total = 0
  proc mid(x: int) {.closure.} =
    proc inner() {.closure.} =
      total = total + x
    inner()
  mid(7)
  mid(35)
  echo total

outer()
