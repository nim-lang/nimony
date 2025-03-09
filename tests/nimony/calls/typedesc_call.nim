
import std / [syncio]

proc foo(x: typedesc[int]) = echo "int"

foo int


