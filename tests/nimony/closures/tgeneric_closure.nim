
import std / [assertions, syncio]

proc outer =
  var x = 120
  proc inner[T] = echo x
  inner[int]()

outer()
