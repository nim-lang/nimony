## Regression test: `for i, (a, b) in seq[(X, Y)]` used to crash the
## hexer's iterinliner with `typ.typeKind == TupleT` because the cursor
## skipped past a `var T` modifier wrapping the inner tuple element.

import std / [syncio]

# pairs() yields `(int, var T)`. Indexed loop with tuple-unpacked second
# var was the original failing case.
var seqA: seq[(int, string)] = @[(1, "a"), (2, "b")]
for i, (sym, name) in seqA:
  echo "v1: ", i, " ", sym, " ", name

# items() (single for-var, tuple unpack). Already worked but kept here
# so the `var T` deref path is exercised in both shapes.
var seqB: seq[(int, string)] = @[(10, "x"), (20, "y")]
for (sym, name) in seqB:
  echo "v2: ", sym, " ", name

# mpairs(): same shape as pairs but mutating. Verifies the haddr
# wrapping on the unpacked sub-vars works for write-throughs.
var seqC: seq[(int, int)] = @[(1, 2), (3, 4)]
for i, (a, b) in mpairs seqC:
  a = a * 10
  b = b * 100
echo "v3: ", seqC[0][0], " ", seqC[0][1], " ", seqC[1][0], " ", seqC[1][1]
