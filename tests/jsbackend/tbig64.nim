## 64-bit integers map to BigInt on the JS target: exact wide arithmetic and
## two's-complement wraparound, distinct from the Number-backed `int`.
import std/syncio

let a = 1000000000'i64
let b = 1000000000'i64
echo a * b                # 10^18, exact — would lose precision as a Number
let big = 9223372036854775807'i64   # high(int64)
echo big
echo big + 1'i64          # wraps to low(int64)
let u = 18446744073709551615'u64    # high(uint64)
echo u
echo u + 1'u64            # wraps to 0
