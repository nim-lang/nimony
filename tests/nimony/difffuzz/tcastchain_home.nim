# Found by tools/difffuzz.nim (reduced): arkham: `int64(cast[int32](u))` sign-extended the register HOME of `u` in place (a cast chain over a register-homed local).
# Do not simplify by hand: several of these only reproduce with this exact
# register allocation.
import std/[syncio, math]

type V = object
  x, y: float64
  n: uint32

proc h0(q0: uint64; q1: uint32): uint64 =
  q0

proc h1(q0: float64; q1: V; q2: uint32): V =
  V(x: q1.x, y: (-(q1.x)), n: (not q1.n))

proc h2(q0: int32; q1: uint32): int32 =
  (-2'i32)

proc h3(q0: uint64; q1: uint32): uint64 =
  (((0x0000000000000001'u64 shl (q0 and 63'u64)) mod ((if false: q0 else: 0x0000000000000003'u64) or 1'u64)) - h0((q0 or 0xFFFFFFFFFFFFFFFF'u64), q1))

proc t6(p0: uint64; p1: float64; p2: uint32; p3: float32; p4: int32) =
  var a4: int32 = 0'i32
  var a6: float64 = 0.0
  var a7: float64 = 0.0
  var a9: bool = false
  var v = V(x: 0.0, y: 0.0, n: 0'u32)
  var w = V(x: (-1e300 * a6), y: p1, n: p2)
  var arr: array[8, uint64] = [p0 + 0'u64, p0 + 1'u64, p0 + 2'u64, p0 + 3'u64, p0 + 4'u64, p0 + 5'u64, p0 + 6'u64, p0 + 7'u64]
  a9 = (int64(a4) != int64(cast[int32](w.n)))
  v = V(x: a7, y: (w.x - min(a6, -2147483648.0)), n: (if (if a9: a9 else: false): w.n else: 0x00000000'u32))
  echo "t6 ", w.n

proc main() =
  t6(0x7FFFFFFFFFFFFFFF'u64, 0.1, 0xA2C0EE8E'u32, 16777217.0'f32, (2'i32))

main()

