# Constant aggregate read folding in Shoggoth (`vmrewriter.nim`).
# Covers:
# - field of a const array element at a literal index (straight-line and via cunroll),
# - non-literal index into a const array (not folded, still correct),
# - nested fields (const array -> object -> object, const object -> object/array, 2D const array),
# - float64 and float32 fields (including default-initialized float32), bool, char, enum,
# - address-taken const element (lvalue must stay addressable),
# - out-of-bounds literal index on a const array stays a compile error.

import std / [syncio, assertions]

type
  Color = enum
    Red, Green, Blue

  Vec3 = object
    x, y, z: float64

  Thing = object
    kind: int
    flag: bool
    tag: char
    color: Color
    center: Vec3
    radius2: float64
    scale: float32

  Config = object
    origin: Vec3
    weights: array[3, int]

const
  Weights = [10, 20, 30, 40]
  F32s = [1.25'f32, -0.5'f32, 2.0'f32]
  Grid: array[2, array[3, int]] = [[1, 2, 3], [4, 5, 6]]
  Things = [
    Thing(kind: 0, flag: true, tag: 'a', color: Red,
          center: Vec3(x: 0.0, y: 0.0, z: 0.0), radius2: 0.0),
    Thing(kind: 1, flag: false, tag: 'b', color: Green,
          center: Vec3(x: 1.5, y: 2.0, z: -0.25), radius2: 1.0, scale: -1.5'f32),
    Thing(kind: 1, flag: true, tag: 'c', color: Blue,
          center: Vec3(x: -1.0, y: 0.5, z: 1.5), radius2: 0.25, scale: 0.5'f32)
  ]
  Cfg = Config(origin: Vec3(x: 3.0, y: 2.0, z: 4.0), weights: [5, 6, 7])

proc sumLoop(x: float64): float64 {.noinline.} =
  var acc = 0.0
  for i in 0 ..< 3:
    if Things[i].kind == 1:
      acc = acc + Things[i].center.x + Things[i].center.z + Things[i].radius2
    else:
      acc = acc + x + Things[i].center.y
  result = acc

proc directScalars(f: float32): int {.noinline.} =
  var s = Weights[0] + Weights[3]
  if Things[0].flag and not Things[1].flag:
    s = s + 1
  if Things[1].tag == 'b' and Things[2].color == Blue:
    s = s + 2
  s = s + Grid[1][2] + Cfg.weights[2]
  if Things[0].scale == f:
    s = s + 1
  result = s

proc directFloats(scale: float64): float64 {.noinline.} =
  let f32sum = F32s[0] + F32s[1] + Things[1].scale
  result = Cfg.origin.x * scale + Things[1].center.z + float64(f32sum)

proc runtimeWeight(idx: int): int {.noinline.} =
  result = Weights[idx]

proc runtimeCenterX(idx: int): float64 {.noinline.} =
  result = Things[idx].center.x

proc runtimeGrid(row: int): int {.noinline.} =
  result = Grid[row][1]

proc readAddrConst(): int {.noinline.} =
  let p = addr(Weights[2])
  result = p[]

proc guardedOob(flag: bool; idx: int): int {.noinline.} =
  # When `flag` is false, this branch never runs; if the optimizer sees a
  # literal out-of-bounds index it must preserve existing behaviour (not invent a value).
  if flag and idx == 4:
    result = Weights[idx]
  else:
    result = Weights[1]

echo sumLoop(2.0)
echo directScalars(0.0'f32)
echo directFloats(2.0)
echo runtimeWeight(0), " ", runtimeWeight(1), " ", runtimeWeight(2)
echo runtimeCenterX(0), " ", runtimeCenterX(1), " ", runtimeCenterX(2)
echo runtimeGrid(0), " ", runtimeGrid(1)
echo readAddrConst()
echo guardedOob(false, 4)
