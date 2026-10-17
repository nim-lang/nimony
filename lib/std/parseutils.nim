## This module contains helpers for parsing tokens, numbers, integers, floats,
## identifiers, etc.

{.feature: "lenientnils".}
{.feature: "staticContracts".}

# TODO: Replace `quit` with exceptions when it is implemented
from std/syncio import quit
import std/[assertions]
import private/dec2flt

const
  Whitespace = {' ', '\t', '\v', '\r', '\l', '\f'}
  IdentChars = {'a'..'z', 'A'..'Z', '0'..'9', '_'}
  IdentStartChars = {'a'..'z', 'A'..'Z', '_'}
    ## copied from strutils

func toLower(c: char): char {.inline.} =
  result = if c in {'A'..'Z'}: chr(ord(c)-ord('A')+ord('a')) else: c

func parseHex*[T: SomeInteger](s: openArray[char], number: var T, maxLen = 0): int {.untyped.} =
  ## Parses a hexadecimal number and stores its value in ``number``.
  ##
  ## Returns the number of the parsed characters or 0 in case of an error.
  ## If error, the value of ``number`` is not changed.
  ##
  ## If ``maxLen == 0``, the parsing continues until the first non-hex character
  ## or to the end of the string. Otherwise, no more than ``maxLen`` characters
  ## are parsed starting from the ``start`` position.
  ##
  ## It does not check for overflow. If the value represented by the string is
  ## too big to fit into ``number``, only the value of last fitting characters
  ## will be stored in ``number`` without producing an error.
  runnableExamples:
    var num: int
    assert parseHex("4E_69_ED", num) == 8
    assert num == 5138925
    assert parseHex("X", num) == 0
    assert parseHex("#ABC", num) == 4
    var num8: int8
    assert parseHex("0x_4E_69_ED", num8) == 11
    assert num8 == 0xED'i8
    assert parseHex("0x_4E_69_ED", num8, 3, 2) == 2
    assert num8 == 0x4E'i8
    var num8u: uint8
    assert parseHex("0x_4E_69_ED", num8u) == 11
    assert num8u == 237
    var num64: int64
    assert parseHex("4E69ED4E69ED", num64) == 12
    assert num64 == 86216859871725
  result = 0
  var i = 0
  var output = T(0)
  var foundDigit = false
  let last = min(s.len, if maxLen == 0: s.len else: i + maxLen)
  if i + 1 < last and s[i] == '0' and (s[i+1] in {'x', 'X'}): inc(i, 2)
  elif i < last and s[i] == '#': inc(i)
  while i < last:
    case s[i]
    of '_': discard
    of '0'..'9':
      output = output shl 4 or T(ord(s[i]) - ord('0'))
      foundDigit = true
    of 'a'..'f':
      output = output shl 4 or T(ord(s[i]) - ord('a') + 10)
      foundDigit = true
    of 'A'..'F':
      output = output shl 4 or T(ord(s[i]) - ord('A') + 10)
      foundDigit = true
    else: break
    inc(i)
  if foundDigit:
    number = output
    result = i

func parseHex*[T: SomeInteger](s: string, number: var T, start = 0,
    maxLen = 0): int =
  ## Parses a hexadecimal number and stores its value in ``number``.
  ##
  ## Returns the number of the parsed characters or 0 in case of an error.
  ## If error, the value of ``number`` is not changed.
  ##
  ## If ``maxLen == 0``, the parsing continues until the first non-hex character
  ## or to the end of the string. Otherwise, no more than ``maxLen`` characters
  ## are parsed starting from the ``start`` position.
  ##
  ## It does not check for overflow. If the value represented by the string is
  ## too big to fit into ``number``, only the value of last fitting characters
  ## will be stored in ``number`` without producing an error.
  runnableExamples:
    var num: int
    assert parseHex("4E_69_ED", num) == 8
    assert num == 5138925
    assert parseHex("X", num) == 0
    assert parseHex("#ABC", num) == 4
    var num8: int8
    assert parseHex("0x_4E_69_ED", num8) == 11
    assert num8 == 0xED'i8
    assert parseHex("0x_4E_69_ED", num8, 3, 2) == 2
    assert num8 == 0x4E'i8
    var num8u: uint8
    assert parseHex("0x_4E_69_ED", num8u) == 11
    assert num8u == 237
    var num64: int64
    assert parseHex("4E69ED4E69ED", num64) == 12
    assert num64 == 86216859871725
  parseHex(s.toOpenArray(start, s.high), number, maxLen)

func rawParseInt(s: openArray[char], b: var BiggestInt): int =
  ## On success returns the number of processed characters and stores the value
  ## in `b`. Returns 0 if `s` does not start with an integer. Returns a negative
  ## number `-n` if the first `n` characters form an integer literal whose value
  ## does not fit in `BiggestInt`; in that case `b` is left unchanged. The whole
  ## digit run is still consumed on overflow so the caller can skip past it.
  var
    sign: BiggestInt = -1
    i = 0
    res = BiggestInt(0)
    overflow = false
  if i < s.len:
    if s[i] == '+': inc(i)
    elif s[i] == '-':
      inc(i)
      sign = 1
  if i < s.len and s[i] in {'0'..'9'}:
    while i < s.len and s[i] in {'0'..'9'}:
      let c = ord(s[i]) - ord('0')
      if not overflow:
        if res >= (low(BiggestInt) + c) div 10:
          res = res * 10 - c
        else:
          overflow = true
      inc(i)
      while i < s.len and s[i] == '_': inc(i) # underscores are allowed and ignored
    if sign == -1 and res == low(BiggestInt):
      overflow = true
    if overflow:
      result = -i
    else:
      b = res * sign
      result = i
  else:
    result = 0

func parseBiggestInt*(s: openArray[char], number: var BiggestInt): int {.
  noSideEffect.} =
  ## Parses an integer and stores the value into `number`.
  ## Result is the number of processed chars, or 0 if there is no integer.
  ## If the parsed integer is out of the valid `BiggestInt` range the result is
  ## a **negative** number `-n` (where `n` chars form the out-of-range literal)
  ## and `number` is left unchanged, so the caller can detect the overflow and
  ## handle it instead of aborting.
  runnableExamples:
    var ret: BiggestInt
    assert parseBiggestInt("9223372036854775807", ret) == 19
    assert ret == 9223372036854775807
    assert parseBiggestInt("-2024_05_09", ret) == 11
    assert ret == -20240509
    # out of range: negative result, `ret` untouched
    assert parseBiggestInt("9223372036854775808", ret) == -19
    assert ret == -20240509
  var res = BiggestInt(0)
  # use 'res' so 'number' is only written on a successful, in-range parse:
  result = rawParseInt(s, res)
  if result > 0:
    number = res

func rawParseUInt(s: openArray[char], b: var BiggestUInt): int =
  ## On success returns the number of processed characters and stores the value
  ## in `b`. Returns 0 if `s` does not start with an unsigned integer. Returns a
  ## negative number `-n` if the first `n` characters are out of the valid
  ## `BiggestUInt` range (including a leading `-` before digits); in that case
  ## `b` is left unchanged.
  var
    res = 0.BiggestUInt
    i = 0
    overflow = false
  if i < s.len - 1 and s[i] == '-' and s[i + 1] in {'0'..'9'}:
    # a negative value is out of range for an unsigned integer
    overflow = true
    inc(i)
  if i < s.len and s[i] == '+': inc(i) # Allow
  if i < s.len and s[i] in {'0'..'9'}:
    while i < s.len and s[i] in {'0'..'9'}:
      if not overflow:
        if res > BiggestUInt.high div 10: # Highest value that you can multiply 10 without overflow
          overflow = true
        else:
          res = res * 10
          let prev = res
          inc res, (ord(s[i]) - ord('0')).BiggestUInt
          if prev > res:
            overflow = true
      inc(i)
      while i < s.len and s[i] == '_': inc(i) # underscores are allowed and ignored
    if overflow:
      result = -i
    else:
      b = res
      result = i
  else:
    result = 0

func parseBiggestUInt*(s: openArray[char], number: var BiggestUInt): int {.
  noSideEffect.} =
  ## Parses an unsigned integer and stores the value into `number`.
  ## Result is the number of processed chars, or 0 if there is no integer.
  ## If the parsed integer is out of the valid `BiggestUInt` range (including a
  ## leading `-`) the result is a **negative** number `-n` and `number` is left
  ## unchanged, so the caller can detect the overflow instead of aborting.
  runnableExamples:
    var ret: BiggestUInt
    assert parseBiggestUInt("12", ret) == 2
    assert ret == 12
    assert parseBiggestUInt("1111111111111111111", ret) == 19
    assert ret == 1111111111111111111'u64
  var res = BiggestUInt(0)
  # use 'res' so 'number' is only written on a successful, in-range parse:
  result = rawParseUInt(s, res)
  if result > 0:
    number = res

# Float parsing is `private/dec2flt`, a port of Rust's `core::num::dec2flt`:
# Clinger's fast path, then Eisel–Lemire, then an exact big-decimal fallback.
# It is correctly rounded (to nearest, ties to even) for float32 and float64
# alike and needs no `strtod`, so a float reads the same on every backend,
# including the libc-free ones.

func parseFloat*(s: openArray[char]; number: var float64): int =
  ## Parses a float and stores the value into `number`, correctly rounded.
  ## Accepts `[sign] digits ['.' digits] [('e'|'E') [sign] digits]` (either
  ## side of the `.` may be empty, not both), as well as `nan`, `inf` and
  ## `infinity` in any case. Underscores after a digit are allowed and
  ## ignored, as in `1_000.5`. Returns the number of processed chars, or 0
  ## (leaving `number` unchanged) if `s` does not start with a float. An `e`
  ## without digits after it is not part of the number.
  runnableExamples:
    var x = 0.0
    doAssert parseFloat("1.5e3 m", x) == 5
    doAssert x == 1500.0
    doAssert parseFloat("1_000.25", x) == 8
    doAssert x == 1000.25
  result = parseFloat64(s, number)

func parseFloat*(s: openArray[char]; number: var float32): int =
  ## Like `parseFloat` for `float64`, but rounds directly to `float32`. Parsing
  ## a `float64` and converting it would round twice and can be off by one ulp.
  runnableExamples:
    var x = 0.0'f32
    doAssert parseFloat("16777217", x) == 8
    doAssert x == 16777216.0'f32
  result = parseFloat32(s, number)

func parseBiggestFloat*(s: openArray[char]; number: var BiggestFloat): int =
  ## Parses a float and stores the value into `number`.
  ## Result is the number of processed chars or 0 if a parsing error
  ## occurred. See `parseFloat` for the accepted syntax.
  result = parseFloat64(s, number)

func skipIgnoreCase*(s, token: openArray[char]): int =
  ## Same as `skip` but case is ignored for token matching.
  runnableExamples:
    doAssert skipIgnoreCase("CAPlow", "CAP", 0) == 3
    doAssert skipIgnoreCase("CAPlow", "cap", 0) == 3
  result = 0
  while result < s.len and result < token.len and
      toLower(s[result]) == toLower(token[result]): inc(result)
  if result != token.len: result = 0

func skipUntil*(s: openArray[char], until: set[char]): int {.inline.} =
  ## Skips all characters until one char from the set `until` is found
  ## or the end is reached.
  ## Returns number of characters skipped.
  result = 0
  while result < s.len and s[result] notin until: inc(result)

func skipUntil*(s: openArray[char], until: char): int {.inline.} =
  ## Skips all characters until the char `until` is found or the end is reached.
  ## Returns number of characters skipped.
  result = 0
  while result < s.len and s[result] != until: inc(result)

func skipUntil*(s: string, until: set[char], start = 0): int {.inline.} =
  ## Skips all characters until one char from the set `until` is found
  ## or the end is reached.
  ## Returns number of characters skipped.
  skipUntil(s.toOpenArray(start, s.high), until)

func skipUntil*(s: string, until: char, start = 0): int {.inline.} =
  ## Skips all characters until the char `until` is found or the end is reached.
  ## Returns number of characters skipped.
  skipUntil(s.toOpenArray(start, s.high), until)
