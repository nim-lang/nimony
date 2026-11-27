# A non-finite float computed by a compile-time call folds further.
import std/[syncio, math]

const inf = cast[float64](0x7FF0000000000000'u64)
const t = trunc(inf)
const a: float32 = float32(t)
const b = t * 2.0
const c = 1.0 + sqrt(-1.0)
const d: float32 = sqrt(-0.25'f32)
echo a, " ", b, " ", c, " ", d
