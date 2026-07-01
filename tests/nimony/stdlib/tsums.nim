import std/assertions
import std/sums

# --- basic ---
assert sumKbn([1.0, 2.0, 3.0]) == 6.0
assert sumPairs([1.0, 2.0, 3.0]) == 6.0
assert sumKbn([5.0]) == 5.0
assert sumPairs([5.0]) == 5.0

# --- empty ---
var e = newSeq[float](0)
assert sumKbn(e) == 0.0
assert sumPairs(e) == 0.0

# --- compensated summation beats naive cancellation ---
# The true sum is 2.0; a naive left-to-right sum returns 0.0 because the two
# 1.0 terms are lost next to 1e100.
assert sumKbn([1.0, 1e100, 1.0, -1e100]) == 2.0

# --- large input exercises the pairwise recursion (base case is 128) ---
var big = newSeq[float](1000)
for i in 0 ..< 1000:
  big[i] = 1.0
assert sumPairs(big) == 1000.0
assert sumKbn(big) == 1000.0

# --- negative and fractional values (all exactly representable) ---
assert sumKbn([1.5, -0.5, -1.0]) == 0.0
assert sumPairs([2.0, -2.0, 2.0, -2.0]) == 0.0
