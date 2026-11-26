import std/assertions

# unsigned min/max are noSideEffect, so a func can call them
func spread8(a, b: uint8): uint8 = max(a, b) - min(a, b)
func spread64(a, b: uint64): uint64 = max(a, b) - min(a, b)

block:
  assert spread8(3, 9) == 6u8
  assert spread64(high(uint64), 1u64) == high(uint64) - 1u64
  assert min(0x80u8, 1u8) == 1u8 and max(0x80u8, 1u8) == 0x80u8
  assert min(65535u16, 2u16) == 2u16 and max(65535u16, 2u16) == 65535u16
  assert min(70000u32, 3u32) == 3u32 and max(70000u32, 3u32) == 70000u32
  let x = 7u8
  assert min(x, 3) == 3u8 and max(x, 200) == 200u8
  assert min(3, x) == 3u8 and max(200, x) == 200u8
  assert min(9'u, 30'u) == 9'u
