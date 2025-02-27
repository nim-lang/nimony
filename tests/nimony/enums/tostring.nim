
import std / [syncio]

type
  E* = enum
    Value1, Value2

let x = $Value1
echo x
