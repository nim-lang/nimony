# Reads of a `const` at literal positions -- `C.f`, `C[k]`, `C[k].f` -- are
# projected out of the constant's value (scalarizer's constructor projection),
# and a branch on one folds. Locations stay reads of the constant: an `addr`
# operand, and anything that is not a literal (an aggregate, a string).

import std / [syncio, assertions]

type
  Color = enum
    Red, Green, Blue
  Vec = object
    x, y: float
  Thing = object
    kind: int
    flag: bool
    tag: char
    color: Color
    center: Vec
    name: string

const
  Weights = [10, 20, 30]
  Things = [
    Thing(kind: 0, flag: true, tag: 'a', color: Red, center: Vec(x: 1.5, y: -2.0), name: "a"),
    Thing(kind: 1, flag: false, tag: 'b', color: Blue, center: Vec(x: 3.0, y: 4.5), name: "b")]
  Origin = Vec(x: 0.25, y: 0.5)

proc literal(x: float): float {.noinline.} =
  result = x
  if Things[1].kind == 1 and not Things[1].flag and Things[1].color == Blue:
    result = result + Things[1].center.y + Origin.x + float(Weights[2])
  if Things[0].tag != 'a':
    result = -1.0

proc variable(i: int): int {.noinline.} =
  result = Weights[i] + Things[i].kind

proc wholes(): string {.noinline.} =
  let c = Things[1].center
  result = Things[0].name & Things[1].name & $c.x

proc location(): int {.noinline.} =
  let p = addr Weights[1]
  result = p[]

echo literal(1.0)
echo variable(0), " ", variable(1)
echo wholes()
echo location()
