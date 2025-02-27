
import std / [syncio]

proc p1[T](x, y: T) = echo "two params"
proc p1[T](x: T) = echo "one param"

p1[int](23)
p1[int](23, 89)
