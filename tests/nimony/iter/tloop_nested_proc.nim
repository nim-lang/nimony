# Each copy of a loop body declares its own instance of a nested routine; the
# copies of different routines must not share a name (#2645).
import std/syncio

proc first(): int =
  result = 0
  for i in 0..<2:
    proc v(): int = 42
    result += v()

proc second(): int =
  result = 0
  for i in 0..<2:
    proc v(): int = 21
    result += v()

echo first(), " ", second()
