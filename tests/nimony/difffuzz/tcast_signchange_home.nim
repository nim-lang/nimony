# Found by tools/difffuzz.nim (reduced): arkham: a same-width signedness cast `cast[int32](u32)` re-extended the home of the local in place.
# Do not simplify by hand: several of these only reproduce with this exact
# register allocation.
import std/[syncio, math]

type V = object
  x, y: float64
  n: uint32

proc h0(q0: float64; q1: V; q2: uint32): V =
  V(x: 1e300, y: ((q0 / q0) / q0), n: ((q1.n or 0x00000001'u32) - (q2 xor q2)))

proc h1(q0: float32; q1: int32): float32 =
  float32(((not 0x00000005'u32) + 0x00008000'u32))

proc h2(q0: uint64; q1: uint32): uint64 =
  ((q0 - (0x7FFFFFFFFFFFFFFF'u64 or 0x0000000100000000'u64)) xor (q0 shl ((0x00000000000000FF'u64 or q0) and 63'u64)))

proc h3(q0: float32; q1: int32): float32 =
  (if ((if true: (2147483648'i64) else: (-1'i64)) != (2147483648'i64)): float32((2.5 / cast[float64](0x000FFFFFFFFFFFFF'u64))) else: q0)

proc t11(p0: uint64; p1: float64; p2: uint32; p3: float32; p4: int32) =
  var a0: uint64 = 0'u64
  var a1: uint64 = 0'u64
  var a2: uint32 = 0'u32
  var a3: uint8 = 0'u8
  var a4: int32 = 0'i32
  var a5: int64 = 0'i64
  var a6: float64 = 0.0
  var a7: float64 = 0.0
  var a8: float32 = 0.0'f32
  var a9: bool = false
  var v = V(x: a6, y: a6, n: p2)
  var w = V(x: p1, y: p1, n: p2)
  var arr: array[8, uint64] = [p0 + 0'u64, p0 + 1'u64, p0 + 2'u64, p0 + 3'u64, p0 + 4'u64, p0 + 5'u64, p0 + 6'u64, p0 + 7'u64]
  a0 = (if (p4 <= cast[int32](v.n)): a0 else: p0)
  echo "t11 ", w.n

proc main() =
  t11(0x00000000143EACCF'u64, 4294967296.0, 0xFFFFFFFF'u32, 1e-40'f32, (-396051323'i32))

main()
