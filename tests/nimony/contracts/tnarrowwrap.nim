# The proven side of `tests/nimony/errmsgs/tnarrowwrap.nim`: a wrapping `+`,
# `-` or `inc` still reads mathematically once the facts keep it inside its type.
import std/syncio

proc add(a, b: int8): int8 =
  result = 0
  if a < 0 or b < 0 or a > 60 or b > 60: return
  let at = a + b
  {.assert: at >= 0.}
  result = at

proc constantGuard(i: int8): int =
  result = -1
  if i >= 0 and i < 100 and i + 1 < 2:
    {.assert: i < 1.}
    result = int(i)

proc unsignedGuard(i: uint8): int =
  result = -1
  if i >= 1 and i - 1 > 5:
    {.assert: i > 6.}
    result = int(i)

const Near = 250'u8

proc constantFold(i: uint8): int =
  result = -1
  if i >= Near + 1:
    {.assert: i > 3.}
    result = int(i)

proc stepped(i: int8): int8 =
  result = i
  if result >= 0 and result < 100:
    inc result
    {.assert: result > 0.}

proc wide(a, b: int): int =
  # signed overflow at full width is undefined in C, not a wrap: unchanged
  result = 0
  if a < 0 or b < 0: return
  let at = a + b
  {.assert: at >= 0.}
  result = at

echo add(60'i8, 50'i8), " ", constantGuard(0'i8), " ", unsignedGuard(9'u8), " ",
  stepped(5'i8), " ", wide(3, 4), " ", constantFold(252'u8)
