#
#
#           Nimony Borrow Checker
#        (c) Copyright 2025 Andreas Rumpf
#
#    See the file "license.txt", included in this
#    distribution, for details about the copyright.
#

## Thanks to NJVL this is significantly easier than earlier attempts:
## - If we borrow from local `x`, we must ensure that as long as the borrow is active,
##   `x` is not mutated via `unknown` or directly reassigned. This is because we actually only
##  mind reallocations. There is no rule like "only single owner is allowed to mutate", everybody
##  can mutate as long as it is not a reallocation.
##
## We also want to be able to borrow from nested locations like `r.b.c[i]` effectively. The crucial
## insight here that this gives us a root (`r` in the example) and a "nesting level". Thus if you mutate
## any "expression prefix" like `r` or `r.b` or `r.b.c` the borrow becomes invalid. But this is not
## true for `r.b.d` as it is unrelated to `r.b.c[i]`!

