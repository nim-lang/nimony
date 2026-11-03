# Found by tools/difffuzz.nim (reduced): arkham x64: an `and` mask that needs 64-bit staging was staged in a register typed by the OUTER conversion's `u8`.
# Do not simplify by hand: several of these only reproduce with this exact
# register allocation.
import std/[syncio, math]

type V = object
  x, y: float64
  n: uint32

proc h0(q0: float64; q1: V; q2: uint32): V =
  V(x: (float64((-1'i32)) + (-(q0))), y: q0, n: ((q1.n + q2) - uint32(ord(true))))

proc h1(q0: float32; q1: int32): float32 =
  float32((-((-(-1.5)))))

proc h2(q0: float64; q1: float64): float64 =
  q0

proc h3(q0: float64; q1: V; q2: uint32): V =
  V(x: -7.25, y: q0, n: q2)

proc t6(p0: uint64; p1: float64; p2: uint32; p3: float32; p4: int32) =
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
  var v = V(x: 0.0, y: 0.0, n: 0'u32)
  var w = V(x: 0.0, y: 0.0, n: 0'u32)
  var arr: array[8, uint64] = [p0 + 0'u64, p0 + 1'u64, p0 + 2'u64, p0 + 3'u64, p0 + 4'u64, p0 + 5'u64, p0 + 6'u64, p0 + 7'u64]
  if (if (p3 < p3): (a8 != cast[float32](0x7F800000'u32)) else: (p3 < p3)):
    discard
    a3 = uint8(uint32((0x0000000000000000'u64 * arr[(p0) and 7'u64]) and 0xffffffff'u64) and 255'u32)
  echo "t6 ", a0, " ", a1, " ", a2, " ", a3, " ", cast[uint32](a4), " ", cast[uint64](a5), " ", (if a6 != a6: 0x7ff8000000000000'u64 else: cast[uint64](a6)), " ", (if a7 != a7: 0x7ff8000000000000'u64 else: cast[uint64](a7)), " ", (if a8 != a8: 0x7fc00000'u32 else: cast[uint32](a8)), " ", ord(a9), " ", (if v.x != v.x: 0x7ff8000000000000'u64 else: cast[uint64](v.x)), " ", (if v.y != v.y: 0x7ff8000000000000'u64 else: cast[uint64](v.y)), " ", (if w.x != w.x: 0x7ff8000000000000'u64 else: cast[uint64](w.x)), " ", (if w.y != w.y: 0x7ff8000000000000'u64 else: cast[uint64](w.y)), " ", v.n, " ", w.n, " ", arr[0], " ", arr[1], " ", arr[2], " ", arr[3], " ", arr[4], " ", arr[5], " ", arr[6], " ", arr[7]

proc main() =
  t6(0x0000000000000007'u64, 4294967296.0, 0x1794021A'u32, 2.5'f32, (-1'i32))

main()
