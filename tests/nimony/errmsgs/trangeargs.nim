# An argument bound to a `range` parameter owes the same proof as a `let` or a
# conversion to that type: the callee is proven from the declared type, so a
# negative `Natural` would read out of bounds there (#2856). The proven side is
# `rangeArgs` in `tests/nimony/contracts/tprovershapes.nim`.

proc h(n: Natural): int = n

proc g(x: int): int = h(x)

proc low3(c: range[0..3]): int = c

proc viaConv(i: int32): char =
  # a conversion to `int` proves the fit to `int`, not to `chr`'s `0..255`
  chr(int(i))

discard h(-1)
discard g(1)
discard low3(4)
discard viaConv(65)
