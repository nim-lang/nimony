# Regression test: object field syms must be numbered per owning type (`.0`,
# bumped only by inheritance shadowing), NOT with a global running counter.
#
# `sum` (in the imported module) reads `Pair`'s fields and is defined before
# `Pair`; instantiating it here re-emits `Pair`'s struct in this module. If the
# field-access sym baked into `sum` were numbered differently than the struct's
# field sym, the C backend would fail with e.g.
#   'Pair_…' has no member named 'a_0'; did you mean 'a_2'?
# See deps/mfieldnumbering for the full shape.

import deps/mfieldnumbering
import std/assertions

let p = Pair[int, int](a: 10, b: 20)
assert sum(0, p) == 30

# A second instantiation with a different field type must also resolve its
# fields against `Pair`'s own scope, not a global counter.
let q = Pair[int, int](a: 1, b: 2)
assert sum("x", q) == 3
