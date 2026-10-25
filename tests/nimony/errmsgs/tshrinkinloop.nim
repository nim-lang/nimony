# `for i in 0 ..< s.len` reads `s.len` once, at loop entry. A body that shrinks
# or replaces `s` makes `i < s.len` false on a later iteration, so the index
# must not count as proven (#2765). The proven side, a body that only writes
# elements, is `elementWritesKeepTheBound` in
# `tests/nimony/contracts/tprovencontracts.nim`.

{.feature: "staticContracts".}

proc shrinks() =
  var xs = @[10, 20, 30]
  for i in 0 ..< xs.len:
    discard xs[i]
    xs.setLen 0

type
  Owner = object
    xs: seq[string]

proc replaces(o: var Owner) =
  for i in 0 ..< o.xs.len:
    discard o.xs[i]
    o.xs = @["new"]

shrinks()
var o = Owner(xs: @["a", "b"])
replaces(o)
