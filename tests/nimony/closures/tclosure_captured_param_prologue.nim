# a routine's PARAMETER captured by a nested closure: the prologue copies it
# into the routine's environment. A nested routine can own such an environment
# only when it captures nothing itself (one environment per routine, #3160).
import std/syncio

proc outer() =
  proc mid(x: int): int =
    var acc = 0
    proc inner() {.closure.} =
      acc = acc + x
    inner()
    result = acc
  echo mid(7) + mid(35)

outer()
