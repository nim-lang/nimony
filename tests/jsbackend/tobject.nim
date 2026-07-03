## Objects: field access, mutation through a var, and passing by value.
import std/syncio

type
  Point = object
    x, y: int

proc manhattan(p: Point): int = p.x + p.y

var p = Point(x: 3, y: 4)
echo p.x
echo p.y
echo manhattan(p)
p.x = 10
p.y = 20
echo manhattan(p)
