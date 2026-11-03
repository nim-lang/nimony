# Found by tools/difffuzz.nim (reduced): arkham x64: `p div ((p div q) or 1)` parked the dividend in rax before the divisor, itself a division, overwrote it.
# Do not simplify by hand: several of these only reproduce with this exact
# register allocation.
import std/[syncio, math]

type V = object
  x, y: float64
  n: uint32

proc h0(q0: int32; q1: uint32): int32 =
  q0

proc h1(q0: uint64; q1: uint32): uint64 =
  q0

proc h2(q0: V; q1: float64): V =
  V(x: q0.x, y: 0.5, n: q0.n)

proc h3(q0: int32; q1: uint32): int32 =
  q0

proc t5(p0: uint64; p1: float64; p2: uint32; p3: float32; p4: int32) =
  var a0: uint64 = (p0 div ((p0 div (p0 or 1'u64)) or 1'u64))
  var a1: uint64 = 0'u64
  var a8: float32 = 0.0'f32
  var a9: bool = false
  var v = V(x: 0.0, y: 0.0, n: 0'u32)
  var w = V(x: 0.0, y: 0.0, n: 0'u32)
  var arr: array[8, uint64] = [p0 + 0'u64, p0 + 1'u64, p0 + 2'u64, p0 + 3'u64, p0 + 4'u64, p0 + 5'u64, p0 + 6'u64, p0 + 7'u64]
  echo "t5 ", a0, " ", a1, " ", arr[7]

proc main() =
  t5(0x0000000080000000'u64, 2.5, 0x00007FFF'u32, -0.25'f32, (-1000'i32))

main()
