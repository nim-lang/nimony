
import std / [syncio]

type
  E* = enum
    Value1, Value2

let x = $Value1
echo x

# can be changed back into `T: enum` when `or` types can match themselves:
proc generic[T: OrdinalEnum](a: T): string =
  result = $a

echo generic(Value2)
