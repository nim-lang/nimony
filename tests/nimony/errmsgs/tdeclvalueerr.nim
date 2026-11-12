# A declaration whose value has no type gets "has no type" in place of the
# value, never beside it. If the value holds an error, its first error is
# the one reported.

import std/syncio

let b = block: (zz2; zz3)

proc p =
  var c = block: zz4
  let e = echo "x"

proc g[T](x: T) =
  var f = block: zz6

let i = echo "y"
p()
g(1)
