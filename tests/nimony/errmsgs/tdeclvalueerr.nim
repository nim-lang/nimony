# A declaration whose value holds an error reports that error alone, and a
# value without a type is replaced by "has no type": in neither case does the
# declaration grow an extra child.

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
