# bitops over EXPLICIT-width types only: results like leadingZeroBits derive
# from sizeof, which differs between the 64-bit oracle and 32-bit wasm for
# plain int/uint.
import std/[syncio, bitops]

let x32 = 0b1011_0000'u32
let x64 = 0x0000_0100_0000_0000'u64

echo countSetBits(x32)           # 3
echo countSetBits(0xFFFF_FFFF_FFFF_FFFF'u64)  # 64
echo popcount(0b101'u32)         # 2
echo parityBits(0b111'u32)       # 1
echo firstSetBit(x32)            # 5 (1-based)
echo trailingZeroBits(x32)       # 4
echo leadingZeroBits(x32)        # 24
echo leadingZeroBits(x64)        # 23
echo trailingZeroBits(x64)       # 40

echo bitand(0xF0'u32, 0x3C'u32)  # 48 (0x30)
echo bitor(0xF0'u32, 0x0F'u32)   # 255
echo bitxor(0xFF'u32, 0x0F'u32)  # 240
echo bitnot(0'u32)               # 4294967295

echo rotateLeftBits(0x8000_0001'u32, 1)   # 3
echo rotateRightBits(0x8000_0001'u32, 1)  # 3221225472
echo rotateLeftBits(1'u64, 63)

var v = 0'u32
setBit(v, 3)
setBit(v, 0)
echo v                           # 9
clearBit(v, 0)
echo v                           # 8
flipBit(v, 4)
echo v                           # 24
echo testBit(v, 4), " ", testBit(v, 5)

# signed explicit widths
echo countSetBits(-1'i32)        # 32
echo leadingZeroBits(1'i64)      # 63

# shifts and masks on explicit widths (plain operators, still bit surface)
echo 1'u32 shl 31                # 2147483648
echo 0x8000_0000'u32 shr 31      # 1
echo (-8'i32) shr 1              # arithmetic shift: -4
echo 0xFF00'u32 and 0x0FF0'u32   # 3840
