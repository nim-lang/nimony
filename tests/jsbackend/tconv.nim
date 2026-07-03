## Numeric conversions: narrowing to 8/16-bit, widening to int64/BigInt, and
## the int64<->uint64 signedness reinterpretation.
import std/syncio

let x = 300
echo uint8(x)             # 300 & 0xFF = 44
echo int8(200)            # wraps to -56
echo uint16(70000)        # 70000 & 0xFFFF = 4464
let big = 5'i64
echo int(big)             # BigInt -> Number
echo int64(7)             # Number -> BigInt
let neg = -1'i64
echo uint64(neg)          # 18446744073709551615
