{.feature: "lenientFloats".}
import std/syncio

# nim-lang/nimony#1899: with `.feature: "lenientFloats".` a wider float *value*
# (a named constant or an arbitrary expression, not just a literal) narrows to a
# smaller float type. Off by default because the narrowing may lose precision.

const M_PI = 3.14
proc takeF32(f: float32): float32 = f

echo takeF32(M_PI)        # float64 constant -> float32
let big: float64 = 2.5
echo takeF32(big)         # float64 variable -> float32
echo takeF32(big + 1.0)   # float64 expression -> float32
