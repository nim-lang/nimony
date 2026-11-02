{.feature: "saturatingArith".}
# bug #2840: signed `+`, `-` and `*` saturate. `127'i8 + 1'i8` is `127`, not a
# wrapped `-128`, so a sum of non-negative operands stays non-negative and the
# index in `f` is in range for every input. The prover may read the sum
# mathematically there because clamping never moves a value past a bound that
# lies inside the type. The unprovable side is
# `tests/nimony/errmsgs/tsaturatecmp.nim`.
import std/syncio

proc f(a, b: int8): int =
  var data = default(array[256, int])
  for i in 0 ..< data.len: data[i] = i
  result = -1
  if a < 0 or b < 0: return
  result = data[int(a + b)]

proc add8(a, b: int8): int8 = a + b
proc sub16(a, b: int16): int16 = a - b
proc mul32(a, b: int32): int32 = a * b
proc add64(a, b: int): int = a + b
proc mul64(a, b: int): int = a * b

proc counter(n: int): int =
  # a saturating counter only grows, which is all its lower bound needs
  var i = 0
  var s = 0
  while i < n:
    s = s + i
    inc i
  result = s

proc ltGuard(i, n: int): int =
  # `i + 1 < n` cannot hold for a clamped `i + 1`: it is exact
  result = 0
  if i + 1 < n:
    {.assert: i < n - 1.}
    result = 1

proc lenGuard(s: string; i: int): int =
  # `i + 1 <= s.len` is exact too: a `len` is below `high(int)`
  result = 0
  if i >= 0 and i + 1 <= s.len:
    result = ord(s[i])

echo f(100'i8, 27'i8), " ", f(127'i8, 1'i8), " ", f(127'i8, 127'i8)
echo add8(127'i8, 1'i8), " ", add8(-128'i8, -1'i8), " ", add8(3'i8, 4'i8)
echo sub16(-32768'i16, 1'i16), " ", sub16(32767'i16, -1'i16)
echo mul32(65536'i32, 65536'i32), " ", mul32(-65536'i32, 65536'i32)
echo add64(high(int), 1), " ", add64(low(int), -1)
echo mul64(high(int), 2), " ", mul64(high(int), -2), " ", mul64(6, 7)
echo counter(5), " ", ltGuard(1, 5), " ", lenGuard("ab", 1)
