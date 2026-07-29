# Portable intrinsics: `{.intrinsic: "X".}` declares a proc that IS an opcode.
# The C backend lowers each to the matching `__builtin_*`; the native backend
# (arkham) emits the instruction directly — `bsf` on x86-64, `rbit`+`clz` on
# AArch64 — with no ABI call in either case.

import std/assertions

proc ctz32(x: uint32): int32 {.intrinsic: "Ctz".}
proc ctz64(x: uint64): int32 {.intrinsic: "Ctz".}
proc clz32(x: uint32): int32 {.intrinsic: "Clz".}
proc clz64(x: uint64): int32 {.intrinsic: "Clz".}
proc bswap32(x: uint32): uint32 {.intrinsic: "Bswap".}
proc bswap64(x: uint64): uint64 {.intrinsic: "Bswap".}

proc main =
  assert ctz64(65536'u64) == 16'i32
  assert ctz64(1'u64) == 0'i32
  assert ctz32(0x80000000'u32) == 31'i32

  assert clz64(1'u64) == 63'i32
  assert clz64(0x8000000000000000'u64) == 0'i32
  assert clz32(1'u32) == 31'i32

  assert bswap64(0x0100000000000000'u64) == 1'u64
  assert bswap64(0x1122334455667788'u64) == 0x8877665544332211'u64
  assert bswap32(0x11223344'u32) == 0x44332211'u32

  # An intrinsic is an ordinary typed proc, so it composes like one.
  var acc = 0'i32
  for i in 0 ..< 4:
    acc = acc + ctz64(uint64(1 shl i) * 256'u64)
  assert acc == 38'i32      # ctz(256)+ctz(512)+ctz(1024)+ctz(2048) = 8+9+10+11

main()
