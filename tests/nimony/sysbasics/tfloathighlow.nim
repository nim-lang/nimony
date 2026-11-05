import std/[syncio, assertions]

# `high`/`low` of a float type are +Inf/-Inf, as in Nim, and keep the
# requested type: `high(float32)` is a `float32`.
let a = high(float32)
let b = low(float32)
let c = float.high
let d = float64.low
assert typeof(a) is float32
assert typeof(b) is float32
assert typeof(c) is float
assert typeof(d) is float64
echo a, " ", b, " ", c, " ", d

var x = 1e30'f32
assert x < high(float32) and x > low(float32)
assert a == a + 1.0'f32
let wide = float32.high.float64
assert wide > 1e300 and -wide < -1e300

proc top[T: SomeFloat](x: T): T = high(T)
proc bottom[T: SomeFloat](x: T): T = low(T)
assert typeof(top(1.0'f32)) is float32
echo top(1.0'f32), " ", bottom(1.0), " ", top(2.0) > 1e308
