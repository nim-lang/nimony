
import std / [assertions, syncio]

proc outer =
  var x = 120
  proc inner[T] {.closure.} = echo x
  inner[int]()

outer()
