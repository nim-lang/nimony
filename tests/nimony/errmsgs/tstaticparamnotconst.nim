# The argument of a `static` parameter must be a tier-1 constant expression.
# A runtime variable is not one, and neither is a call: general CTFE is never
# implicit and has to be written `static(sq(3))`.

proc twice(x: static[int]): int = x * 2
proc sq(x: int): int = x * x

var v = 3
discard twice(v)
discard twice(sq(3))
