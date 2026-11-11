# `copyMem`/`zeroMem`/`cmpMem` on the address of a scalar local or parameter with a
# constant size equal to its width. The inliner splices `(addr x)` straight into
# the `memcpy`/`memset`/`memcmp` call, and arkham lowers that call as a move or
# a compare: no stack slot, no call. memcmp's contract is only the SIGN of its
# result, so the compares print `sgn`. Every shape it accepts is here, plus the near misses that must
# keep the memory path, and every result is printed.

import std / syncio

proc keep(p: ptr int) {.noinline.} =
  inc p[]

proc bits(x: float): uint64 {.noinline.} =
  result = 0
  copyMem(addr result, addr x, 8)                 # float -> int bits

proc fromBits(u: uint64): float {.noinline.} =
  result = 0.0
  copyMem(addr result, addr u, sizeof(result))    # int bits -> float

proc bits32(x: float32): uint32 {.noinline.} =
  result = 0
  copyMem(addr result, addr x, 4)

proc fromBits32(u: uint32): float32 {.noinline.} =
  result = 0.0'f32
  copyMem(addr result, addr u, 4)

proc sameType(a: int): int {.noinline.} =
  var b = 7
  copyMem(addr b, addr a, sizeof(int))            # int <- int
  result = b * 2

proc zeroInt(a: int): int {.noinline.} =
  var x = a
  zeroMem(addr x, sizeof(x))
  result = x + a

proc zeroFloat(a: float): float {.noinline.} =
  var x = a
  zeroMem(addr x, 8)
  result = x + a

proc zeroSmall(a: uint16): uint16 {.noinline.} =
  var x = a
  zeroMem(addr x, 2)
  result = x + 1

proc alsoAddrTaken(a: int): int {.noinline.} =
  # `x` is address-taken by `keep`, so it lives on the stack: the lowering must
  # store to the slot instead of a register.
  var x = 0
  keep(addr x)
  var y = a
  copyMem(addr x, addr y, 8)
  keep(addr x)
  result = x

proc partial(a: int): int {.noinline.} =
  # 4 of 8 bytes: NOT a whole-local move, keeps the memory path.
  var x = a
  var y = -1
  copyMem(addr x, addr y, 4)
  result = x

proc inLoop(n: int): uint64 {.noinline.} =
  result = 0
  var f = 0.5
  for i in 0 ..< n:
    var u = 0'u64
    copyMem(addr u, addr f, 8)
    result = result xor u
    f = f * 3.0

proc sgn(x: int): int = (if x < 0: -1 elif x > 0: 1 else: 0)

proc cmp64(a, b: int): int {.noinline.} =
  var x = a
  var y = b
  result = sgn(cmpMem(addr x, addr y, 8))

proc cmp32(a, b: uint32): int {.noinline.} =
  result = sgn(cmpMem(addr a, addr b, sizeof(uint32)))

proc cmp16(a, b: int16): int {.noinline.} =
  result = sgn(cmpMem(addr a, addr b, 2))

proc cmp8(a, b: uint8): int {.noinline.} =
  result = sgn(cmpMem(addr a, addr b, 1))

proc cmpFloat(a, b: float): int {.noinline.} =
  result = sgn(cmpMem(addr a, addr b, 8))

proc cmpMixed(a: float32; b: int32): int {.noinline.} =
  result = sgn(cmpMem(addr a, addr b, 4))

proc cmpDiscarded(a, b: int): int {.noinline.} =
  var x = a
  discard cmpMem(addr x, addr b, 8)                  # statement position: dead
  result = x

proc cmpPartial(a, b: int): int {.noinline.} =
  # 3 of 8 bytes: keeps the memory path.
  result = sgn(cmpMem(addr a, addr b, 3))

echo bits(1.5)
echo fromBits(4609434218613702656'u64)
echo bits32(1.5'f32)
echo fromBits32(1069547520'u32)
echo sameType(21)
echo zeroInt(5)
echo zeroFloat(2.5)
echo zeroSmall(9)
echo alsoAddrTaken(40)
echo partial(0x100000000)
echo inLoop(5)
echo cmp64(5, 5), " ", cmp64(1, 256), " ", cmp64(256, 1), " ", cmp64(-1, 1)
echo cmp32(0x01020304'u32, 0x04030201'u32), " ", cmp32(7, 7), " ", cmp32(0x100, 0x2)
echo cmp16(1, 2), " ", cmp16(-1, 0), " ", cmp16(0x0100, 0x0001)
echo cmp8(3, 200), " ", cmp8(200, 3), " ", cmp8(9, 9)
echo cmpFloat(1.0, 2.0), " ", cmpFloat(2.0, 2.0), " ", cmpFloat(-0.0, 0.0)
echo cmpMixed(1.0'f32, 0x3F800000'i32), " ", cmpMixed(1.0'f32, 0)
echo cmpDiscarded(3, 4)
echo cmpPartial(0x010203, 0x030201), " ", cmpPartial(0x10_000000, 0x20_000000)
