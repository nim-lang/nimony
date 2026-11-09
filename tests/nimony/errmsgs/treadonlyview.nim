# A `string` buffer is copy-on-write: a view of it is `.readonlyView` and
# cannot be passed to a `var openArray`, even from a mutable string (#2924).
proc poke(p: var openArray[char]) =
  if p.len > 0: p[0] = 'X'

var a = "a much longer string that lives on the heap"
var b = a
poke(b)
poke(toOpenArray(b))
