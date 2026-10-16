# Unsigned arithmetic wraps at every width (doc/language.md). Reading such a
# `+` or `-` as the mathematical one "proved" claims that are false at run time:
# `i + 1 < 2` holds for `i = high(uint64)`, and `add(250)` returns 4. Each
# claim below is false for some input; the proven side is
# `tests/nimony/contracts/tunsignedwrap.nim`.

proc add(a: uint8): uint8 =
  result = 0
  if a >= 250:
    let at = a + 10
    {.assert: at >= 260.}
    result = at

proc plusGuard(i: uint64): int =
  result = -1
  if i + 1 < 2:
    {.assert: i < 1.}
    result = 0

proc minusGuard(i: uint8): int =
  result = -1
  if i - 1 > 5:
    {.assert: i > 6.}
    result = int(i)

proc narrowGuard(i: uint16): int =
  result = -1
  if i + 1 < 2:
    {.assert: i < 1.}
    result = int(i)

const Top = 255'u8

proc constantFold(i: uint8): int =
  # `Top + 1` is 0: the guard holds for every `i`
  result = -1
  if i >= Top + 1:
    {.assert: i > 3.}
    result = int(i)

proc stepped(i: uint8): uint8 =
  result = i
  if result >= 255:
    inc result
    {.assert: result >= 256.}

discard add(250'u8)
discard plusGuard(high(uint64))
discard minusGuard(0'u8)
discard narrowGuard(high(uint16))
discard constantFold(0'u8)
discard stepped(255'u8)
