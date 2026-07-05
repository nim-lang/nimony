## Fixed-width (32-bit) wrapping arithmetic must wrap like Nim's sized ints, not
## keep JS `Number`'s full-precision result. Regression guard for the hash-width
## bug: `h * prime + c` computed in a register must equal the same value after it
## round-trips through a 32-bit memory slot, or string-keyed Tables mis-dedup.
import std/syncio

# uint32 multiply wraps modulo 2^32 (0x10000000 * 16 == 2^32 == 0).
var a = 0x10000000'u32
var b = 16'u32
echo a * b                      # 0

# A large uint32 product loses no low bits (would overflow 2^53 as a plain a*b).
var x = 2654435761'u32          # Knuth's multiplicative hash constant
var y = 40503'u32
echo x * y                      # 2654435761*40503 mod 2^32

# int32 multiply wraps into the signed range.
var i = 65536
var j = 65536
echo i * j                      # 2^32 wrapped to int32 -> 0

# Addition wraps too.
var big = 0x7fffffff            # int32 max
echo big + 1                    # -2147483648

# A hand-rolled FNV-1a-style accumulator over bytes, all in uint32, matches the
# wrapped result (exercises repeated *-then-+ in a loop).
var h = 2166136261'u32
for c in [104'u32, 105'u32, 33'u32]:   # "hi!"
  h = (h xor c) * 16777619'u32
echo h
