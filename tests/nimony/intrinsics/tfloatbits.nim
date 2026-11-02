# Float codegen regressions found by `bench/raytracer.nim` on `nimony n`.
import std/[math, syncio]

func hi(x: float64): uint32 {.inline.} = cast[uint32](cast[uint64](x) shr 32)

proc overflowBranch(z: float64): int =
  # A same-width `uint32 -> int32` cast reused the zero-extended register
  # without re-extending it, so the signed compare saw 3225616409.
  let j = cast[int32](hi(z))
  if j >= 0x40900000'i32: 1 else: 0

proc aliasFold(r, z0: float64): float64 =
  # `z = 1.0 - (r - z)` with `z` in a register: the lhs used to be computed
  # into z's own register before the rhs had read z.
  var z = z0 * 1.0
  z = 1.0 - (r - z)
  z = 2.0 - z
  z

proc norm(x, y, z: float64): float64 = sqrt(x*x + y*y + z*z)

var a = 2.0
var f = 0.25'f32
echo sqrt(a), " ", sqrt(f), " ", sqrt(-1.0).isNaN, " ", norm(1.0, 2.0, 2.0)
echo overflowBranch(-38.0), " ", overflowBranch(1e300)
echo aliasFold(0.25, 0.5), " ", pow(0.9, 250.0), " ", pow(1.4, 2.5)
echo cast[uint64](-0.0), " ", cast[float64](0x4000000000000000'u64)
echo floor(-1.5), " ", round(2.5), " ", ceil(-0.3), " ", trunc(-7.25)
