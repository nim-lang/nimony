# issue #2612: a `case`/`if` expression whose leading branch ends in `return`.
import std/syncio

proc p() = discard

proc a(c: int32): int =
  case c
  of 0: return 4
  of 2: 3
  else: 6

proc b(c: int32): int =
  case c
  of 0:
    if c < 0: return -1
    p()
    return 3
  else: 4

proc d(c: int32): int =
  if c == 0:
    if c < 0: return -1
    p()
    return 3
  else: 4

echo a(0), " ", a(2), " ", a(5), " ", b(0), " ", b(1), " ", d(0), " ", d(1)
