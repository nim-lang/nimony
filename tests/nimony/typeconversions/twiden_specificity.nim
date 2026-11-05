import std/[syncio, assertions]

# Mixed-width arithmetic picks the narrowest overload both operands widen to
# (nim-lang/nimony#2869): `int32` converts to `int64` but not the other way
# round, so `+(int32, int32)` is more specific than `+(int64, int64)`.
var i16 = 5'i16
var i32 = 7'i32
var i: int
assert typeof(i16 + 34) is int16
assert typeof(i16 + i) is int
assert typeof(i16 + i32) is int32
assert typeof(i32 + i16) is int32
assert typeof(5'i8 + 2'i16) is int16

var u8 = 3'u8
var u32 = 4'u32
assert typeof(u8 + u32) is uint32

# An overload set with two candidates the argument widens to: the narrower wins.
proc pick(x: int32): string = "int32"
proc pick(x: int64): string = "int64"
echo pick(5'i8)
echo pick(5'i16)
echo pick(5'i64)

# Float literals adapt to a `float32` operand regardless of whether the value
# is exactly representable (nim-lang/nimony#2864).
var f32 = 22.2'f32
var f64 = 22.2
assert typeof(f32 / 2.0) is float32
assert typeof(f32 * 0.1) is float32
assert typeof(22.2 / 2.0'f32) is float32
assert typeof(22.5 / 2.0'f32) is float32
assert typeof(f32 / 2.0'f64) is float64
assert typeof(f32 / f64) is float64
assert typeof(f64 / f32) is float64
echo i16 + i32, " ", u8 + u32
