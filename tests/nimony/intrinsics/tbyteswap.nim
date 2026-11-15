# `byteSwap`: the `Bswap` intrinsic (`bswap`, `rev`) where the target has it,
# shifts elsewhere. Operands come from runtime values so nothing folds.
import std/[bitops, syncio]

proc swap16(x: uint16): uint16 {.noinline.} = byteSwap(x)
proc swap32(x: uint32): uint32 {.noinline.} = byteSwap(x)
proc swap64(x: uint64): uint64 {.noinline.} = byteSwap(x)
proc swapI16(x: int16): int16 {.noinline.} = byteSwap(x)
proc swapI32(x: int32): int32 {.noinline.} = byteSwap(x)
proc swapI64(x: int64): int64 {.noinline.} = byteSwap(x)

proc readBE32(b: array[4, uint8]): uint32 {.noinline.} =
  # a little-endian load then a swap: big-endian decoding
  var w = 0'u32
  for i in 0 ..< 4: w = w or (uint32(b[i]) shl (8 * i))
  result = byteSwap(w)

echo swap16(0x1122'u16), " ", swap16(0x00FF'u16), " ", swap16(0'u16)
echo swap32(0x11223344'u32), " ", swap32(0x000000FF'u32), " ", swap32(0xFFFFFFFF'u32)
echo swap64(0x1122334455667788'u64), " ", swap64(1'u64), " ", swap64(swap64(0xDEADBEEF'u64))
echo swapI16(0x0180'i16), " ", swapI32(-2'i32), " ", swapI64(0x80'i64)
echo byteSwap(0xAB'u8), " ", byteSwap(uint(0x0102)) != 0'u, " ", byteSwap(byteSwap(-12345))
echo readBE32([0x12'u8, 0x34, 0x56, 0x78])
