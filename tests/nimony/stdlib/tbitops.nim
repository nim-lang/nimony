import std/assertions
import std/bitops

# --- bitand / bitor / bitxor / bitnot ---
assert bitand(0b1100'u8, 0b1010'u8) == 0b1000'u8
assert bitor(0b1100'u8, 0b1010'u8) == 0b1110'u8
assert bitxor(0b1100'u8, 0b1010'u8) == 0b0110'u8
assert bitnot(0b0000_1111'u8) == 0b1111_0000'u8
assert bitand(0xF0F0'u16, 0x0FF0'u16) == 0x00F0'u16

# --- countSetBits / popcount / parityBits ---
assert countSetBits(0'u32) == 0
assert countSetBits(0b1011'u8) == 3
assert countSetBits(0xFFFF_FFFF'u32) == 32
assert countSetBits(0xFFFF_FFFF_FFFF_FFFF'u64) == 64
assert popcount(0b1011'u8) == 3
assert parityBits(0b1011'u8) == 1          # 3 set bits -> odd
assert parityBits(0b0011'u8) == 0          # 2 set bits -> even
assert parityBits(0'u64) == 0

# --- firstSetBit (1-based index of least-significant set bit) ---
assert firstSetBit(0'u8) == 0              # defined: 0 when input is 0
assert firstSetBit(0b0001'u8) == 1
assert firstSetBit(0b1000'u8) == 4
assert firstSetBit(0x8000_0000'u32) == 32
assert firstSetBit(0x8000_0000_0000_0000'u64) == 64

# --- trailingZeroBits / leadingZeroBits ---
assert trailingZeroBits(0b1000'u8) == 3
assert trailingZeroBits(0'u32) == 32       # defined: width when input is 0
assert trailingZeroBits(0'u64) == 64
assert leadingZeroBits(0x0000_0001'u32) == 31
assert leadingZeroBits(0x8000_0000'u32) == 0
assert leadingZeroBits(0'u32) == 32        # defined: width when input is 0
assert leadingZeroBits(0x0000_0001'u8) == 7

# --- rotateLeftBits / rotateRightBits ---
assert rotateLeftBits(0b0001_0000'u8, 3) == 0b1000_0000'u8
assert rotateLeftBits(0b1000_0000'u8, 1) == 0b0000_0001'u8   # wraps around
assert rotateRightBits(0b0000_0001'u8, 1) == 0b1000_0000'u8
assert rotateLeftBits(0x0000_0001'u32, 32) == 0x0000_0001'u32  # full turn = identity
assert rotateRightBits(0x1234_5678'u32, 0) == 0x1234_5678'u32

# --- setBit / clearBit / flipBit / testBit ---
var v = 0'u8
setBit(v, 0); assert v == 0b0000_0001'u8
setBit(v, 3); assert v == 0b0000_1001'u8
assert testBit(v, 3) == true
assert testBit(v, 2) == false
clearBit(v, 0); assert v == 0b0000_1000'u8
flipBit(v, 3); assert v == 0'u8
flipBit(v, 5); assert v == 0b0010_0000'u8
