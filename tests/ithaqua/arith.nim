import std/syncio

# int32 arithmetic kept in range (overflow TRAPS on the native backend, so we
# stay well inside int32 bounds and never provoke it).
let a: int32 = 2_000_000_000'i32
let b: int32 = 100_000_000'i32
echo a - b            # 1900000000
echo a div 7'i32
echo a mod 7'i32

# div/mod with negative operands — sign rules must match between backends.
echo (-17'i32) div 5'i32
echo (-17'i32) mod 5'i32
echo 17'i32 div (-5'i32)
echo 17'i32 mod (-5'i32)

# int64 sums and products, magnitudes an int32 could not hold.
let c: int64 = 9_000_000_000'i64
let d: int64 = 1_234_567'i64
echo c + d
echo c * 2'i64
echo d * d

# comparisons echoed as bools.
echo a > b
echo b > a
echo a == a
