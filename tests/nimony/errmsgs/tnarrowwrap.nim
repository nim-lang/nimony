# Unsigned arithmetic wraps at every width, and so does a signed type narrower
# than C's `int`: it is computed promoted and truncated back. Reading such a `+`
# or `-` as the mathematical one "proved" claims that are false at run time —
# `add(127, 1)` returns -128, and `i + 1 < 2` holds for `i = 127'i8` and for
# `i = high(uint64)`. Each claim below is false for some input; the proven side
# is `tests/nimony/contracts/tnarrowwrap.nim`.

proc add(a, b: int8): int8 =
  result = 0
  if a < 0 or b < 0: return
  let at = a + b
  {.assert: at >= 0.}
  result = at

proc constantGuard(i: int8): int =
  result = -1
  if i >= 0 and i + 1 < 2:
    {.assert: i < 1.}
    result = int(i)

proc unsignedGuard(i: uint8): int =
  result = -1
  if i - 1 > 5:
    {.assert: i > 6.}
    result = int(i)

proc wideUnsigned(i: uint64): int =
  result = -1
  if i + 1 < 2:
    {.assert: i < 1.}
    result = 0

proc int16Guard(i: int16): int =
  result = -1
  if i >= 0 and i + 1 < 2:
    {.assert: i < 1.}
    result = int(i)

const Top = 255'u8

proc constantFold(i: uint8): int =
  # `Top + 1` is 0: the guard holds for every `i`
  result = -1
  if i >= Top + 1:
    {.assert: i > 3.}
    result = int(i)

proc stepped(i: int8): int8 =
  result = i
  if result >= 0:
    inc result
    {.assert: result > 0.}

discard add(127'i8, 1'i8)
discard constantGuard(127'i8)
discard unsignedGuard(0'u8)
discard wideUnsigned(high(uint64))
discard int16Guard(high(int16))
discard constantFold(0'u8)
discard stepped(127'i8)
