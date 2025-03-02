
import std / [syncio]

type
  E* = enum
    Value1, Value2

let x = $Value1
echo x

proc generic[T: enum](a: T): string =
  result = $a

echo generic(Value2)
