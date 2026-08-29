# Pure-CPU math surface: float arithmetic + the Schubfach `$`, and math.nim's
# pure-Nim procs. Libm-backed procs (sqrt, sin, pow, float `mod`, …) are
# importc into a libc-free target on BOTH legs — out of scope here.
import std/[syncio, math]

# float arithmetic and shortest-round-trip printing
let a = 0.1
let b = 0.2
echo a + b                       # 0.30000000000000004
echo 1.5 * 4.0                   # 6.0
echo 1.0 / 3.0
echo 2.5 - 0.5
echo -7.25
echo 1e300 * 10.0                # inf territory via arithmetic
# (float32 conversion + arithmetic are broken on the NATIVE backend — see
# nativebugs/float32_ops.nim)

# special values from runtime arithmetic (no compile-time folding surprises)
var zero = 0.0
var one = 1.0
echo one / zero                  # inf
echo -one / zero                 # -inf
echo zero / zero                 # nan

# comparisons and conversions
echo 1.5 < 1.6, " ", 2.0 == 2.0
echo int32(3.99), " ", int32(-3.99)   # 3 -3 (truncation)
echo float(7'i32)                # 7.0

# pure-Nim math.nim procs
echo almostEqual(0.1 + 0.2, 0.3)      # true
echo sgn(-9), " ", sgn(0), " ", sgn(14)
echo floorDiv(8, 3), " ", floorDiv(-8, 3)    # 2 -3
echo floorMod(8, 3), " ", floorMod(-8, 3)    # 2 1
echo euclDiv(-8, 3), " ", euclMod(-8, 3)     # -3 1
echo sum(@[1, 2, 3, 4]), " ", prod(@[1, 2, 3, 4])
echo sum(@[1.5, 2.5])            # 4.0
echo gcd(12, 18), " ", lcm(4, 6) # 6 12
echo gcd(@[12, 18, 24])          # 6
echo isPowerOfTwo(64), " ", isPowerOfTwo(63)
echo nextPowerOfTwo(100)         # 128
