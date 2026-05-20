# Regression: `case x of <imported-set-const>` used to trip sem into
# re-evaluating the imported enum's value tuples, where `(ord(TgA), "a")`
# then got type-checked as `int -> E` and failed with a misleading
# "type mismatch: got int64 but wanted E" pointing back into the
# declaration of the imported enum.

import std/[assertions]
import tcross_module_ord_a

proc classify(x: E): int =
  case x
  of MySet:
    result = 1
  else:
    result = 0

assert classify(A) == 1
assert classify(B) == 0
assert classify(C) == 1
assert classify(F) == 1
