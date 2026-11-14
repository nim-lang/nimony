# The proven side of `tests/nimony/errmsgs/tforexit.nim` and `ttryentry.nim`:
# what holds on every way out of a `for` loop, or into an `except`/`finally`,
# is still known there. Facts about locations a loop or a `try` body never
# writes survive it.

{.feature: "staticContracts".}

import std/syncio

proc risky(x: int) {.raises.} =
  if x > 3: raise Failure

proc sumUntil3(s: seq[int]): int =
  result = 0
  if s.len > 2:
    for k in 0 ..< s.len:
      if s[k] == 3: break
      result += s[k]
    result += s[2]                # `s.len > 2` holds after both exits

proc firstAt(s: seq[int]; x: int): int =
  result = -1
  for k in 0 ..< s.len:
    if s[k] == x:
      result = k
      break
  if result >= 0 and result < s.len:
    result = s[result]            # proven under the guard, as for a `while`

proc guarded(s: seq[int]; x: int): int =
  result = 0
  if x >= 0 and x < s.len:
    var y = 0
    try:
      y = x
      risky(x)
      result = s[x]
    except:
      result = s[x]               # `x` and `s` are not written in the body
    finally:
      echo s[x]
    result += s[x]

echo sumUntil3(@[1, 2, 3, 4]), " ", firstAt(@[5, 6, 7], 6), " ", firstAt(@[5], 9)
echo guarded(@[1, 2, 3], 1)
echo guarded(@[1, 2, 3, 4, 5], 4)
