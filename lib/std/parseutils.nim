## This module contains helpers for parsing tokens, numbers, integers, floats,
## identifiers, etc.

{.feature: "lenientnils".}

# TODO: Replace `quit` with exceptions when it is implemented
from std/syncio import quit
import std/[assertions]

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

# Following parseBiggestFloat code is copied from `lib/system/strmantle.nim` in Nim 2.

when defined(nimNoLibc):
  # ── freestanding, CORRECTLY ROUNDED decimal → float64 ────────────────────
  #
  # Only `parseBiggestFloat`'s slow path reaches this, and it always hands us a
  # normalized, NUL-terminated `[-]<digits>E<sign><exp>` buffer. The obvious
  # implementation — accumulate the mantissa in a uint64 and scale by 10^exp in
  # floating point — is off by up to a few ulp because each scaling step rounds:
  # `1e300` came back as `1.0000000000000002e+300` and `2.225073858507201e-308`
  # as `2.2250738585072004e-308`, which is visible as float literals that differ
  # between the native and the C (`strtod`) backend.
  #
  # This is instead Go's `strconv` big-decimal algorithm (`decimal.go`/`atof.go`):
  # hold the value as an EXACT decimal digit string plus a decimal-point position
  # and shift it by powers of two — exactly, in decimal — until the binary
  # mantissa is normalized, then round exactly once. Every intermediate step is
  # exact, so that single rounding is the correctly-rounded result. It is much
  # slower than a float multiply, which does not matter: the fast paths above
  # already take every literal that fits 15-16 digits with |exponent| <= 22.
  const
    MaxDecDigits = 800   ## enough for any float64 shift chain (Go's bound too)
    MaxShift = 60        ## keeps `leftShift`'s per-digit accumulator in uint64
    Pow2Digits = [       ## decimal digits of 2^k — an upper bound on the digits
                         ## a `leftShift(k)` adds (it adds this or one less; the
                         ## code detects which instead of carrying Go's cutoff
                         ## table of 61 decimal strings)
      1, 1, 1, 1, 2, 2, 2, 3, 3, 3, 4, 4,
      4, 4, 5, 5, 5, 6, 6, 6, 7, 7, 7, 7,
      8, 8, 8, 9, 9, 9, 10, 10, 10, 10, 11, 11,
      11, 12, 12, 12, 13, 13, 13, 13, 14, 14, 14, 15,
      15, 15, 16, 16, 16, 16, 17, 17, 17, 18, 18, 18,
      19]
    Log2Pow10 = [1, 3, 6, 9, 13, 16, 19, 23, 26]
      ## How far the value may be shifted in one step without overshooting the
      ## [0.5, 1) target, indexed by the decimal-point position (`floor(log2(10^i))`,
      ## except entry 0, which must be >= 1 so the normalization loop terminates).
    Log2Pow10Len = 9

  type
    BigDec = object
      ## value = 0.d[0..nd-1] * 10^dp  (sign in `neg`)
      d: array[MaxDecDigits, char]
      nd: int
      dp: int
      neg: bool
      trunc: bool     ## a nonzero digit fell off the end — the stored value is
                      ## slightly LOW, which `shouldRoundUp` needs to know

  func trimBig(a: var BigDec) =
    while a.nd > 0 and a.d[a.nd-1] == '0': dec a.nd
    if a.nd == 0: a.dp = 0

  func rightShiftBig(a: var BigDec; k: int) =
    ## a = a / 2^k, exactly (digits are produced until they run out).
    var r = 0            # read cursor
    var w = 0            # write cursor
    var n = 0'u64
    # Pick up enough leading digits to cover the first shift.
    while (n shr k) == 0'u64:
      if r >= a.nd:
        if n == 0'u64:
          a.nd = 0
          return
        while (n shr k) == 0'u64:
          n = n * 10'u64
          inc r
        break
      n = n * 10'u64 + uint64(ord(a.d[r]) - ord('0'))
      inc r
    a.dp = a.dp - (r - 1)
    let mask = (1'u64 shl k) - 1'u64
    # Pick up a digit, put down a digit.
    while r < a.nd:
      let c = uint64(ord(a.d[r]) - ord('0'))
      let dig = n shr k
      n = n and mask
      a.d[w] = chr(int(dig) + ord('0'))
      inc w
      n = n * 10'u64 + c
      inc r
    # Put down the extra digits the division produced.
    while n > 0'u64:
      let dig = n shr k
      n = n and mask
      if w < MaxDecDigits:
        a.d[w] = chr(int(dig) + ord('0'))
        inc w
      elif dig > 0'u64:
        a.trunc = true
      n = n * 10'u64
    a.nd = w
    trimBig(a)

  func leftShiftBig(a: var BigDec; k: int) =
    ## a = a * 2^k, exactly. Digits are produced right to left into the region
    ## `[0, nd+delta)`; `delta` over-estimates by at most one, and the surplus
    ## shows up as unwritten leading slots, which the compaction below removes.
    let delta = Pow2Digits[k]
    var r = a.nd - 1
    var w = a.nd + delta        # exclusive: the next digit goes to w-1
    var n = 0'u64
    while r >= 0:
      n = n + (uint64(ord(a.d[r]) - ord('0')) shl k)
      let quo = n div 10'u64
      let rem = n - 10'u64 * quo
      dec w
      if w < MaxDecDigits: a.d[w] = chr(int(rem) + ord('0'))
      elif rem != 0'u64: a.trunc = true
      n = quo
      dec r
    while n > 0'u64:
      let quo = n div 10'u64
      let rem = n - 10'u64 * quo
      dec w
      if w < MaxDecDigits: a.d[w] = chr(int(rem) + ord('0'))
      elif rem != 0'u64: a.trunc = true
      n = quo
    # `w` is the index of the most significant digit actually written: 0 when the
    # estimate was exact, 1 when it was one too generous.
    let stored = min(a.nd + delta, MaxDecDigits)
    if w > 0:
      var j = 0
      while w + j < stored:
        a.d[j] = a.d[w + j]
        inc j
      a.nd = j
    else:
      a.nd = stored
    a.dp = a.dp + delta - w
    trimBig(a)

  func shiftBig(a: var BigDec; k: int) =
    ## a = a * 2^k for k > 0, a / 2^-k for k < 0. Chunked so no single step
    ## exceeds `MaxShift`.
    if a.nd == 0: return
    var k = k
    while k > MaxShift:
      leftShiftBig(a, MaxShift); k = k - MaxShift
    if k > 0: leftShiftBig(a, k)
    while k < -MaxShift:
      rightShiftBig(a, MaxShift); k = k + MaxShift
    if k < 0: rightShiftBig(a, -k)

  func shouldRoundUpBig(a: BigDec; nd: int): bool =
    if nd < 0 or nd >= a.nd:
      result = false
    elif a.d[nd] == '5' and nd+1 == a.nd:
      # Exactly halfway — unless digits were dropped, in which case the true
      # value is above halfway and rounds up regardless.
      if a.trunc: result = true
      else: result = nd > 0 and ((ord(a.d[nd-1]) - ord('0')) mod 2) != 0
    else:
      result = a.d[nd] >= '5'

  func roundedIntegerBig(a: BigDec): uint64 =
    if a.dp > 20:
      result = 0xFFFFFFFFFFFFFFFF'u64
    else:
      var i = 0
      var n = 0'u64
      while i < a.dp and i < a.nd:
        n = n * 10'u64 + uint64(ord(a.d[i]) - ord('0'))
        inc i
      while i < a.dp:
        n = n * 10'u64
        inc i
      if shouldRoundUpBig(a, a.dp): n = n + 1'u64
      result = n

  func floatBitsBig(a: var BigDec): uint64 =
    ## IEEE-754 binary64 bit pattern of `a`, correctly rounded (ties to even).
    const
      MantBits = 52
      ExpBits = 11
      Bias = -1023
    var exp = 0
    var mant = 0'u64
    var overflow = false
    if a.nd == 0:
      exp = Bias
    elif a.dp > 310:
      overflow = true
    elif a.dp < -330:
      exp = Bias                      # underflows to zero
    else:
      # Scale by powers of two until the value is in [0.5, 1).
      while a.dp > 0:
        let n = if a.dp >= Log2Pow10Len: 27 else: Log2Pow10[a.dp]
        shiftBig(a, -n)
        exp = exp + n
      while a.dp < 0 or (a.dp == 0 and a.nd > 0 and a.d[0] < '5'):
        let n = if -a.dp >= Log2Pow10Len: 27 else: Log2Pow10[-a.dp]
        shiftBig(a, n)
        exp = exp - n
      # Binary floats are normalized to [1, 2), not [0.5, 1).
      dec exp
      # Below the smallest normal exponent the mantissa loses bits instead.
      if exp < Bias+1:
        let n = Bias + 1 - exp
        shiftBig(a, -n)
        exp = exp + n
      if exp - Bias >= (1 shl ExpBits) - 1:
        overflow = true
      else:
        shiftBig(a, 1 + MantBits)     # extract 1+MantBits significant bits
        mant = roundedIntegerBig(a)
        if mant == (2'u64 shl MantBits):
          # rounding carried into a new leading bit
          mant = mant shr 1
          inc exp
          if exp - Bias >= (1 shl ExpBits) - 1: overflow = true
        if not overflow and (mant and (1'u64 shl MantBits)) == 0'u64:
          exp = Bias                  # subnormal
    if overflow:
      mant = 0'u64
      exp = (1 shl ExpBits) - 1 + Bias
    result = mant and ((1'u64 shl MantBits) - 1'u64)
    result = result or (uint64((exp - Bias) and ((1 shl ExpBits) - 1)) shl MantBits)
    if a.neg: result = result or (1'u64 shl (MantBits + ExpBits))

  func c_strtod(buf: cstring, endptr: ptr cstring): float64 {.noSideEffect.} =
    ## Freestanding (`nimony n`, libc-free) decimal→float64, correctly rounded.
    ## The input is `parseBiggestFloat`'s normalized `[-]<digits>E<sign><exp>`
    ## buffer; `endptr` is ignored (the only caller passes nil).
    var a = BigDec(nd: 0, dp: 0, neg: false, trunc: false)
    var i = 0
    if buf[i] == '-':
      a.neg = true; inc i
    elif buf[i] == '+':
      inc i
    while buf[i] == '0': inc i           # leading zeros carry no information
    var nd = 0
    while buf[i] in {'0'..'9'}:
      if nd < MaxDecDigits:
        a.d[nd] = buf[i]
        inc nd
      elif buf[i] != '0':
        a.trunc = true
      inc i
    a.nd = nd
    var se = nd                          # digits are integral: value = D * 10^exp
    if buf[i] == 'E' or buf[i] == 'e':
      inc i
      var eneg = false
      if buf[i] == '-': (eneg = true; inc i)
      elif buf[i] == '+': inc i
      var e = 0
      while buf[i] in {'0'..'9'}:
        if e < 100000: e = e * 10 + (ord(buf[i]) - ord('0'))
        inc i
      se = se + (if eneg: -e else: e)
    a.dp = se
    trimBig(a)
    result = cast[float64](floatBitsBig(a))
else:
  func c_strtod(buf: cstring, endptr: ptr cstring): float64 {.
    importc: "strtod", header: "<stdlib.h>", noSideEffect.}

func parseBiggestFloat*(s: openArray[char]; number: var BiggestFloat): int {.
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
