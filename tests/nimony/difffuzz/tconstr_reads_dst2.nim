# Found by tools/difffuzz.nim (reduced): arkham: a constructor reading its own destination (second shape, via `w.x`).
# Do not simplify by hand: several of these only reproduce with this exact
# register allocation.
import std/[syncio, math]

type V = object
  x, y: float64
  n: uint32

proc h0(q0: float64; q1: V; q2: uint32): V =
  V(x: (-((q1.x / q1.x))), y: q0, n: 0x000000FF'u32)

proc h1(q0: float32; q1: int32): float32 =
  q0

proc h2(q0: int32; q1: uint32): int32 =
  cast[int32](((0x00000005'u32 - q1) shr (q1 and 31'u32)))

proc h3(q0: uint64; q1: uint32): uint64 =
  (if false: (if (0.5 >= -1e300): (q0 shl (q0 and 63'u64)) else: q0) else: q0)

proc t0(p0: uint64; p1: float64; p2: uint32; p3: float32; p4: int32) =
  var a0: uint64 = 0xFFFFFFFFFFFFFFFF'u64
  var a1: uint64 = 0'u64
  var a2: uint32 = 0'u32
  var a3: uint8 = 0'u8
  var a4: int32 = 0'i32
  var a5: int64 = 0'i64
  var a6: float64 = 0.0
  var a7: float64 = (cast[float64](a0) - sqrt(p1))
  var a8: float32 = 0.0'f32
  var a9: bool = false
  var v = V(x: 0.0, y: 0.0, n: 0'u32)
  var w = V(x: a7, y: p1, n: p2)
  var arr: array[8, uint64] = [p0 + 0'u64, p0 + 1'u64, p0 + 2'u64, p0 + 3'u64, p0 + 4'u64, p0 + 5'u64, p0 + 6'u64, p0 + 7'u64]
  w = V(x: (if false: (if a9: v.x else: p1) else: v.x), y: (float64(p4) + (w.x - -1e300)), n: uint32(ord((not false))))
  echo "t0 ", (if w.y != w.y: 0x7ff8000000000000'u64 else: cast[uint64](w.y))

proc main() =
  t0(0x0000000000000002'u64, 0.0, 0x00000001'u32, 3.4e38'f32, (1000'i32))

main()
