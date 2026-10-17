# Like tshiftoverflow, but for a shift by a bounded (not constant) amount:
# `digits = digits + fraction` with `fraction` bounded moves every bound on
# `digits` by that bound, and a vacuous bound near `high(xint)` overflows to
# NaN. The NaN fact used to be stored, and the confluence after
# `if result > 0` asserted on it in `join` (#2672). Now it is dropped.
import std/syncio

proc advance(i: var int) =
  discard

proc scan(n: int): int =
  result = 0
  var i = 0
  let integerStart = i
  advance(i)
  var digits = i - integerStart
  var fraction = 0
  if i < n:
    let fractionStart = i
    fraction = i - fractionStart
    digits = digits + fraction
    if result > 0:
      dec digits

echo scan(0)
echo scan(5)
