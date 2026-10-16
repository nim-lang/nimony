# The proven side of `tests/nimony/errmsgs/tunsignedwrap.nim`: an unsigned `+`,
# `-` or `inc` still reads mathematically once the facts keep it inside its
# type. Signed arithmetic is read mathematically at every width: overflowing
# it is an error, not a wrap.
import std/syncio

proc add(a: uint8): uint8 =
  result = 0
  if a >= 50 and a <= 100:
    let at = a + 10
    {.assert: at >= 60.}
    result = at

proc plusGuard(i: uint64): int =
  result = -1
  if i < 100 and i + 1 < 2:
    {.assert: i < 1.}
    result = 0

proc minusGuard(i: uint8): int =
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

proc stepped(i: uint8): uint8 =
  result = i
  if result >= 5 and result < 200:
    inc result
    {.assert: result >= 6.}

proc signedNarrow(i: int8): int =
  result = -1
  if i >= 0 and i + 1 < 2:
    {.assert: i < 1.}
    result = int(i)

echo add(60'u8), " ", plusGuard(0'u64), " ", minusGuard(9'u8), " ",
  constantFold(252'u8), " ", stepped(5'u8), " ", signedNarrow(0'i8)
