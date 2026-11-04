# Signed arithmetic wraps (#2840): `127'i8 + 1'i8` is `-128'i8`. The prover
# reads a signed operation mathematically where the facts keep it inside its
# type, as it does for unsigned arithmetic. The unprovable side is
# `tests/nimony/errmsgs/tsignedwrap.nim`.
import std/syncio

proc add8(a, b: int8): int8 = a + b
proc sub16(a, b: int16): int16 = a - b
proc mul32(a, b: int32): int32 = a * b
proc add64(a, b: int): int = a + b
proc neg64(a: int): int = -a

proc sum8(a, b: int8): int =
  # bounded operands: the sum fits
  result = 0
  if a >= 0 and a < 64 and b >= 0 and b < 64:
    {.assert: a + b >= 0.}
    result = int(a + b)

proc ltGuard(i, n: int): int =
  # `i < n - 1`, the form of `i + 1 < n` that cannot wrap where `n > 0`
  result = 0
  if i >= 0 and n > 0 and i < n - 1:
    {.assert: i + 1 < n.}
    result = 1

proc lenGuard(s: string; i: int): int =
  # `i + 1 <= s.len` is exact once `i < s.len`: a `len` is below `high(int)`
  result = 0
  if i >= 0 and i < s.len and i + 1 <= s.len:
    result = ord(s[i])

proc counter(s: string): int =
  # `inc i` under `i < s.len` cannot wrap, so `i` stays non-negative
  var i = 0
  result = 0
  while i < s.len:
    result = result + ord(s[i])
    inc i

echo add8(127'i8, 1'i8), " ", add8(-128'i8, -1'i8), " ", add8(3'i8, 4'i8)
echo sub16(-32768'i16, 1'i16), " ", sub16(32767'i16, -1'i16)
echo mul32(65536'i32, 65536'i32), " ", mul32(65537'i32, 65537'i32)
echo add64(high(int), 1) == low(int), " ", neg64(low(int)) == low(int)
echo sum8(63'i8, 63'i8), " ", ltGuard(1, 5), " ", lenGuard("ab", 1), " ", counter("ab")
