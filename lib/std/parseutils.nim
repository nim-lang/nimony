## This module contains helpers for parsing tokens, numbers, integers, floats,
## identifiers, etc.

# TODO: Replace `quit` with exceptions when it is implemented
from std/syncio import quit
import std/[assertions]

const
  Whitespace = {' ', '\t', '\v', '\r', '\l', '\f'}
  IdentChars = {'a'..'z', 'A'..'Z', '0'..'9', '_'}
  IdentStartChars = {'a'..'z', 'A'..'Z', '_'}
    ## copied from strutils

proc toLower(c: char): char {.inline.} =
  result = if c in {'A'..'Z'}: chr(ord(c)-ord('A')+ord('a')) else: c

proc integerOutOfRangeError() {.noinline, noreturn.} =
  # TODO: Uncomment when exception is implemented.
  #raise newException(ValueError, "Parsed integer outside of valid range")
  quit "Parsed integer outside of valid range"

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

proc rawParseInt(s: openArray[char], b: var BiggestInt): int =
  var
    sign: BiggestInt = -1
    i = 0
  if i < s.len:
    if s[i] == '+': inc(i)
    elif s[i] == '-':
      inc(i)
      sign = 1
  if i < s.len and s[i] in {'0'..'9'}:
    b = 0
    while i < s.len and s[i] in {'0'..'9'}:
      let c = ord(s[i]) - ord('0')
      if b >= (low(BiggestInt) + c) div 10:
        b = b * 10 - c
      else:
        integerOutOfRangeError()
      inc(i)
      while i < s.len and s[i] == '_': inc(i) # underscores are allowed and ignored
    if sign == -1 and b == low(BiggestInt):
      integerOutOfRangeError()
    else:
      b = b * sign
      result = i
  else:
    result = 0

proc parseBiggestInt*(s: openArray[char], number: var BiggestInt): int {.
  noSideEffect.} =
  ## Parses an integer and stores the value into `number`.
  ## Result is the number of processed chars or 0 if there is no integer.
  ## `ValueError` is raised if the parsed integer is out of the valid range.
  runnableExamples:
    var ret: BiggestInt
    assert parseBiggestInt("9223372036854775807", ret) == 19
    assert ret == 9223372036854775807
    assert parseBiggestInt("-2024_05_09", ret) == 11
    assert ret == -20240509
  var res = BiggestInt(0)
  # use 'res' for exception safety (don't write to 'number' in case of an
  # overflow exception):
  result = rawParseInt(s, res)
  if result != 0:
    number = res

proc rawParseUInt(s: openArray[char], b: var BiggestUInt): int =
  var
    res = 0.BiggestUInt
    i = 0
  if i < s.len - 1 and s[i] == '-' and s[i + 1] in {'0'..'9'}:
    integerOutOfRangeError()
  if i < s.len and s[i] == '+': inc(i) # Allow
  if i < s.len and s[i] in {'0'..'9'}:
    b = 0
    while i < s.len and s[i] in {'0'..'9'}:
      if res > BiggestUInt.high div 10: # Highest value that you can multiply 10 without overflow
        integerOutOfRangeError()
      res = res * 10
      let prev = res
      inc res, (ord(s[i]) - ord('0')).BiggestUInt
      if prev > res:
        integerOutOfRangeError()
      inc(i)
      while i < s.len and s[i] == '_': inc(i) # underscores are allowed and ignored
    b = res
    result = i
  else:
    result = 0

proc parseBiggestUInt*(s: openArray[char], number: var BiggestUInt): int {.
  noSideEffect.} =
  ## Parses an unsigned integer and stores the value
  ## into `number`.
  ## `ValueError` is raised if the parsed integer is out of the valid range.
  runnableExamples:
    var ret: BiggestUInt
    assert parseBiggestUInt("12", ret, 0) == 2
    assert ret == 12
    assert parseBiggestUInt("1111111111111111111", ret, 0) == 19
    assert ret == 1111111111111111111'u64
  var res = BiggestUInt(0)
  # use 'res' for exception safety (don't write to 'number' in case of an
  # overflow exception):
  result = rawParseUInt(s, res)
  if result != 0:
    number = res

# Following parseBiggestFloat code is copied from `lib/system/strmantle.nim` in Nim 2.

proc c_strtod(buf: cstring, endptr: ptr cstring): float64 {.
  importc: "strtod", header: "<stdlib.h>", noSideEffect.}

proc parseBiggestFloat*(s: openArray[char]; number: var BiggestFloat): int {.
  noSideEffect.} =
  ## Parses a float and stores the value into `number`.
  ## Result is the number of processed chars or 0 if a parsing error
  ## occurred.

  # This routine attempt to parse float that can parsed quickly.
  # i.e. whose integer part can fit inside a 53bits integer.
  # their real exponent must also be <= 22. If the float doesn't follow
  # these restrictions, transform the float into this form:
  #  INTEGER * 10 ^ exponent and leave the work to standard `strtod()`.
  # This avoid the problems of decimal character portability.
  # see: https://www.exploringbinary.com/fast-path-decimal-to-floating-point-conversion/

  # TODO: Change `let` to `const` when following initial value can be const.
  let IdentChars = {'a'..'z', 'A'..'Z', '0'..'9', '_'}

  const
    powtens =  [1e0, 1e1, 1e2, 1e3, 1e4, 1e5, 1e6, 1e7, 1e8, 1e9,
                1e10, 1e11, 1e12, 1e13, 1e14, 1e15, 1e16, 1e17, 1e18, 1e19,
                1e20, 1e21, 1e22]

  var
    i = 0
    sign = 1.0
    kdigits, fdigits = 0
    exponent = 0
    integer = uint64(0)
    fracExponent = 0
    expSign = 1
    firstDigit = -1
    hasSign = false

  # Sign?
  if i < s.len and (s[i] == '+' or s[i] == '-'):
    hasSign = true
    if s[i] == '-':
      sign = -1.0
    inc(i)

  # NaN?
  if i+2 < s.len and (s[i] == 'N' or s[i] == 'n'):
    if s[i+1] == 'A' or s[i+1] == 'a':
      if s[i+2] == 'N' or s[i+2] == 'n':
        if i+3 >= s.len or s[i+3] notin IdentChars:
          number = NaN
          return i+3
    return 0

  # Inf?
  if i+2 < s.len and (s[i] == 'I' or s[i] == 'i'):
    if s[i+1] == 'N' or s[i+1] == 'n':
      if s[i+2] == 'F' or s[i+2] == 'f':
        if i+3 >= s.len or s[i+3] notin IdentChars:
          number = Inf*sign
          return i+3
    return 0

  if i < s.len and s[i] in {'0'..'9'}:
    firstDigit = (s[i].ord - '0'.ord)
  # Integer part?
  while i < s.len and s[i] in {'0'..'9'}:
    inc(kdigits)
    integer = integer * 10'u64 + (s[i].ord - '0'.ord).uint64
    inc(i)
    while i < s.len and s[i] == '_': inc(i)

  # Fractional part?
  if i < s.len and s[i] == '.':
    inc(i)
    # if no integer part, Skip leading zeros
    if kdigits <= 0:
      while i < s.len and s[i] == '0':
        inc(fracExponent)
        inc(i)
        while i < s.len and s[i] == '_': inc(i)

    if firstDigit == -1 and i < s.len and s[i] in {'0'..'9'}:
      firstDigit = (s[i].ord - '0'.ord)
    # get fractional part
    while i < s.len and s[i] in {'0'..'9'}:
      inc(fdigits)
      inc(fracExponent)
      integer = integer * 10'u64 + (s[i].ord - '0'.ord).uint64
      inc(i)
      while i < s.len and s[i] == '_': inc(i)

  # if has no digits: return error
  if kdigits + fdigits <= 0 and
     (i == 0 or # no char consumed (empty string).
     (i == 1 and hasSign)): # or only '+' or '-
    return 0

  if i+1 < s.len and s[i] in {'e', 'E'}:
    inc(i)
    if s[i] == '+' or s[i] == '-':
      if s[i] == '-':
        expSign = -1

      inc(i)
    if s[i] notin {'0'..'9'}:
      return 0
    while i < s.len and s[i] in {'0'..'9'}:
      exponent = exponent * 10 + (ord(s[i]) - ord('0'))
      inc(i)
      while i < s.len and s[i] == '_': inc(i) # underscores are allowed and ignored

  var realExponent = expSign*exponent - fracExponent
  let expNegative = realExponent < 0
  var absExponent = abs(realExponent)

  # if exponent greater than can be represented: +/- zero or infinity
  if absExponent > 999:
    if integer == 0:
      number = 0.0
    elif expNegative:
      number = 0.0*sign
    else:
      number = Inf*sign
    return i

  # if integer is representable in 53 bits:  fast path
  # max fast path integer is  1<<53 - 1 or  8999999999999999 (16 digits)
  let digits = kdigits + fdigits
  if digits <= 15 or (digits <= 16 and firstDigit <= 8):
    # max float power of ten with set bits above the 53th bit is 10^22
    if absExponent <= 22:
      if expNegative:
        number = sign * integer.float / powtens[absExponent]
      else:
        number = sign * integer.float * powtens[absExponent]
      return i

    # if exponent is greater try to fit extra exponent above 22 by multiplying
    # integer part is there is space left.
    let slop = 15 - kdigits - fdigits
    if absExponent <= 22 + slop and not expNegative:
      number = sign * integer.float * powtens[slop] * powtens[absExponent-slop]
      return i

  # if failed: slow path with strtod.
  var t {.noinit.}: array[500, char] # flaviu says: 325 is the longest reasonable literal
  var ti = 0
  let maxlen = t.len - 1 - "e+000".len # reserve enough space for exponent

  let endPos = i
  result = endPos
  i = 0
  # re-parse without error checking, any error should be handled by the code above.
  if i < endPos and s[i] == '.': i.inc
  while i < endPos and s[i] in {'0'..'9','+','-'}:
    if ti < maxlen:
      t[ti] = s[i]; inc(ti)
    inc(i)
    while i < endPos and s[i] in {'.', '_'}: # skip underscore and decimal point
      inc(i)

  # insert exponent
  t[ti] = 'E'
  inc(ti)
  t[ti] = if expNegative: '-' else: '+'
  inc(ti, 4)

  # insert adjusted exponent
  t[ti-1] = ('0'.ord + absExponent mod 10).char
  absExponent = absExponent div 10
  t[ti-2] = ('0'.ord + absExponent mod 10).char
  absExponent = absExponent div 10
  t[ti-3] = ('0'.ord + absExponent mod 10).char
  # array not zeroed out:
  t[ti] = '\0'
  number = c_strtod(cast[cstring](addr t), nil)

proc skipIgnoreCase*(s, token: openArray[char]): int =
  ## Same as `skip` but case is ignored for token matching.
  runnableExamples:
    doAssert skipIgnoreCase("CAPlow", "CAP", 0) == 3
    doAssert skipIgnoreCase("CAPlow", "cap", 0) == 3
  result = 0
  while result < s.len and result < token.len and
      toLower(s[result]) == toLower(token[result]): inc(result)
  if result != token.len: result = 0
