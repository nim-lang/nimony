## Test that `cast` does bit-reinterpretation (memcpy), not C-level type conversion.
## See https://github.com/nim-lang/nimony/issues/1589

import std/assertions

# --- Runtime casts ---

# cast[float](42) must NOT produce 42.0 (that would be a C-level conversion).
# It must reinterpret the bits of the int as a float.
var x: int = 42
let y = cast[float](x)
assert y != 42.0

# Round-trip: cast to float and back must preserve the original int value.
let z = cast[int](y)
assert z == 42

# Zero round-trip
var zero: int = 0
let fzero = cast[float](zero)
assert fzero == 0.0
let izero = cast[int](fzero)
assert izero == 0

# --- Compile-time casts ---

const cf = cast[float](42)
assert cf != 42.0

const crt = cast[int](cf)
assert crt == 42

const cf0 = cast[float](0)
assert cf0 == 0.0

const ci0 = cast[int](cf0)
assert ci0 == 0

# Compile-time and runtime must agree
var rx: int = 42
assert cast[float](rx) == cf
assert cast[int](cast[float](rx)) == crt

# cast[ptr int](nil) should work at compile time
const np = cast[nil ptr int](nil)
assert np == nil

# A cast to a larger type zeroes the remaining memory: a signed source is not
# sign-extended (planetis-m/nimony-bugs-llms#203).
type Neg = enum
  nm = -2, nz = 0
var i8 = -2'i8
var i32 = -1'i32
var en = nm
proc notI8(x: int8): uint16 = cast[uint16](not x)
assert notI8(1) == 254'u16
assert cast[int16](i8) == 254'i16
assert cast[uint64](i32) == 0xFFFF_FFFF'u64
assert cast[int](i32) == 0xFFFF_FFFF
assert cast[uint64](en) == 0xFFFF_FFFE'u64 # an enum with a negative value is 32 bits
