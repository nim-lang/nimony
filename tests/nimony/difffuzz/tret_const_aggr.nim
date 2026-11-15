# Reduced from bench/raytracer.nim: returning a >16-byte `const` object copies
# it through the hidden result pointer. arkham's arm64 copy took the source's
# address with `lea`, which only knows stack slots; the const is a rodata label
# and needs `adr` (nifasm: "lea source must be a memory operand").
import std/syncio

type Vec3 = object
  x, y, z: float64

const
  Black = Vec3(x: 0.0, y: 0.0, z: 0.0)
  Grey = Vec3(x: 0.5, y: 0.25, z: 0.125)

proc pick(i: int): Vec3 =
  # Recursive, like `traceRay`, so it stays an out-of-line call with a hidden
  # result pointer instead of being inlined into its caller.
  if i <= 0:
    return Black
  if i == 1:
    return Grey
  return pick(i - 2)

let a = pick(2)
let b = pick(3)
echo a.x, " ", a.y, " ", a.z
echo b.x, " ", b.y, " ", b.z
