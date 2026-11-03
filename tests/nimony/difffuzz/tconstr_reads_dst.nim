# Found by tools/difffuzz.nim (reduced): arkham: `v = V(x: p, y: v.x / p)` was built field by field INTO `v`, so `y` read the new `v.x`.
# Do not simplify by hand: several of these only reproduce with this exact
# register allocation.
import std/[syncio, math]

type V = object
  x, y: float64
  n: uint32

proc h0(q0: int32; q1: uint32): int32 =
  cast[int32]((if (not true): q1 else: (if false: q1 else: q1)))

proc h1(q0: float64; q1: V; q2: uint32): V =
  V(x: float64((cast[float32](0x7FC00000'u32) / -1.0'f32)), y: (-((q0 * q0))), n: q1.n)

proc h2(q0: V; q1: float64): V =
  V(x: cast[float64](0x7FF8000000000000'u64), y: -1.5, n: (q0.n or (not q0.n)))

proc h3(q0: float64; q1: V; q2: uint32): V =
  V(x: q1.x, y: q1.x, n: q1.n)

proc t7(p0: uint64; p1: float64; p2: uint32; p3: float32; p4: int32) =
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
  if (not true):
    discard
  else:
    v = V(x: p1, y: (v.x / p1), n: p2)
  echo "t7 ", (if v.y != v.y: 0x7ff8000000000000'u64 else: cast[uint64](v.y))

proc main() =
  t7(0x0000000000000003'u64, -1e300, 0x80000000'u32, 1.0'f32, (-2147483648'i32))

main()
