# Found by tools/difffuzz.nim (reduced): arkham: `float64(16777217.0'f32)` materialized the literal at f64 precision instead of rounding it to f32 first (x64 and a64).
# Do not simplify by hand: several of these only reproduce with this exact
# register allocation.
import std/[syncio, math]

type V = object
  x, y: float64
  n: uint32

proc h0(q0: V; q1: float64): V =
  V(x: floor(cast[float64](0x8000000000000000'u64)), y: q1, n: (if true: (not q0.n) else: 0x00000005'u32))

proc h1(q0: V; q1: float64): V =
  V(x: q1, y: cast[float64](0x0000000000000001'u64), n: ((q0.n + q0.n) mod ((0x00000001'u32 * q0.n) or 1'u32)))

proc h2(q0: V; q1: float64): V =
  V(x: (-(pow(q0.x, q1))), y: max(q1, q0.x), n: 0x00000002'u32)

proc h3(q0: int32; q1: uint32): int32 =
  cast[int32](uint32(1'u8))

proc t6(p0: uint64; p1: float64; p2: uint32; p3: float32; p4: int32) =
  var v = V(x: 0.0, y: 0.0, n: 0'u32)
  v = V(x: 0.0, y: float64(16777217.0'f32), n: 0x00000001'u32)
  echo "t6 ", (if v.y != v.y: 0x7ff8000000000000'u64 else: cast[uint64](v.y))

proc main() =
  t6(0x0000000000000000'u64, -1.5, 0x000000FF'u32, cast[float32](0x7F800000'u32), (-2147483648'i32))

main()
