## Text → integer conversion.
##
## A port of Rust's integer parser, `from_str_radix` and its helpers in
## `library/core/src/num/mod.rs` (rust-lang/rust commit
## 70424d844bbe2c6dbc8f38dda80affd754a7cfe4; Rust is dual-licensed MIT /
## Apache-2.0). Line numbers below refer to that commit.
##
## The digits are summed most significant first. A string short enough that
## no value of its length can overflow the target type is summed with plain
## wrapping arithmetic; a longer one checks every multiply and add. A negative
## number is summed downwards from zero, so `low(T)`, whose magnitude does
## not fit in `T`, needs no special case.
##
## `fromStrRadix` is the port itself, with Rust's whole-string contract and
## error kinds. `parseDecimal` is the same algorithm as a `parseutils`-style
## prefix reader (decimal only, `_` separators, `-n` on overflow); it is what
## `parseBiggestInt` and `parseBiggestUInt` call.

{.feature: "staticContracts".}

import std/assertions

type
  IntErrorKind* = enum
    ## Why a string is not an integer of the requested type. Rust's
    ## `IntErrorKind` (`library/core/src/num/error.rs`), plus `ikNone`.
    ikNone          ## no error: the string is an integer of the type
    ikEmpty         ## the string is empty
    ikInvalidDigit  ## a character is not a digit of the radix, or the
                    ## string is only a sign
    ikPosOverflow   ## the value is greater than `high(T)`
    ikNegOverflow   ## the value is less than `low(T)`
    ikZero          ## the value is zero but the type is non-zero; Rust's
                    ## `NonZero` parsers only, never produced here

func canNotOverflow*(radix: int; isSignedTy: bool; size, digitsLen: int): bool {.
    inline.} =
  ## mod.rs:1582, `can_not_overflow`. With `N` the value of `digitsLen`
  ## digits in base `radix <= 16`:
  ##
  ##   |N| <= radix^digitsLen - 1
  ##       <= 16^(2 * size - isSigned) - 1
  ##       == 2^(8 * size - 4 * isSigned) - 1
  ##       <= 2^(8 * size - isSigned) - 1  == high(T)
  ##
  ## so the condition is sufficient for no overflow. `size` is `sizeof(T)`;
  ## with the radix known at the call site only the length test is left.
  result = radix <= 16 and digitsLen <= size * 2 - ord(isSignedTy)

func toDigit(c: char; radix: int): int {.inline.} =
  ## The value of the digit `c` in base `radix`, or a value `>= radix` if `c`
  ## is not one. `char::to_digit`, `library/core/src/char/methods.rs:409`:
  ## `0`-`9` are 0-9, `A`-`Z` and `a`-`z` are 10-35.
  let u = uint32(ord(c))
  var value: uint32
  if c > '9' and radix > 10:
    # Clearing bit 5 after the subtraction maps `a`-`z` onto `A`-`Z`; every
    # other character lands at 36 or above.
    value = ((u - uint32(ord('A'))) and not 0b0010_0000'u32) + 10'u32
  else:
    # Characters below `0` wrap around to huge values.
    value = u - uint32(ord('0'))
  result = if value < uint32(radix): int(value) else: radix

func fromStrRadix*[T: SomeInteger](src: openArray[char]; radix: int;
                                   value: var T): IntErrorKind {.untyped.} =
  ## mod.rs:1792, `from_ascii_bytes_radix_impl`, the body of
  ## `T::from_str_radix`. The whole of `src` must be an optional sign (only
  ## `+` for an unsigned `T`) followed by at least one digit of `radix`; no
  ## whitespace, no `_`. On success stores the number in `value` and returns
  ## `ikNone`; otherwise returns the reason and leaves `value` unchanged.
  ## A `radix` outside `2..36` is a programming error and aborts.
  if radix < 2 or radix > 36:                                   # mod.rs:1796
    failedAssertImpl("fromStrRadix: radix must lie in the range [2, 36]")
  if src.len == 0:                                              # mod.rs:1800
    return ikEmpty
  const isSignedTy = T is SomeSignedInt                         # mod.rs:1805
  # mod.rs:1807: a lone sign is an invalid digit; a `-` for an unsigned type
  # is left in the digits, where it is one.
  if src.len == 1 and (src[0] == '+' or src[0] == '-'):
    return ikInvalidDigit
  var isPositive = true
  var i = 0
  if src[0] == '+':
    i = 1
  elif src[0] == '-' and isSignedTy:
    isPositive = false
    i = 1
  var res = T(0)
  let r = T(radix)
  if canNotOverflow(radix, isSignedTy, sizeof(T), src.len - i): # mod.rs:1827
    # No value of this length overflows `T`: wrapping arithmetic is exact.
    # Rust uses `wrapping_mul`/`wrapping_add`; Nimony's `*`, `+`, `-` wrap.
    while i < src.len:                                          # mod.rs:1841
      res = res * r
      let x = toDigit(src[i], radix)
      if x >= radix:
        return ikInvalidDigit
      if isPositive:
        res = res + T(x)
      else:
        res = res - T(x)
      inc i
  else:
    # mod.rs:1855: `checked_mul`, then `checked_add` (positive) or
    # `checked_sub` (negative). The multiply is issued before the digit is
    # looked at, as in Rust, so its latency overlaps the digit test; an
    # invalid digit is still reported ahead of an overflow from that multiply.
    let overflowKind = if isPositive: ikPosOverflow else: ikNegOverflow
    while i < src.len:
      var mulOverflow = false
      var addOverflow = false
      {.keepOverflowFlag.}:
        res = res * r
        mulOverflow = overflowFlag()
        let x = toDigit(src[i], radix)
        if x >= radix:
          return ikInvalidDigit
        if isPositive:
          res = res + T(x)
        else:
          res = res - T(x)
        addOverflow = overflowFlag()
      if mulOverflow or addOverflow:
        return overflowKind
      inc i
  value = res
  result = ikNone

# ── parseutils' decimal prefix reader ──────────────────────────────────────

func parseDecimal*[T: SomeInteger](s: openArray[char]; value: var T): int {.
    untyped.} =
  ## Reads a decimal integer from the start of `s`: an optional sign (`+`, or
  ## `-` for a signed `T`) and one or more digits, where any run of `_` after
  ## a digit is skipped. Returns the number of characters read and stores the
  ## number in `value`; returns 0 if `s` does not start with an integer. If
  ## the number does not fit in `T` it returns `-n`, `n` being the length of
  ## the whole number, and leaves `value` unchanged. For an unsigned `T` a `-`
  ## followed by a digit starts a number that is always out of range.
  ##
  ## `fromStrRadix` adapted to the `parseutils` contract: it reads a prefix,
  ## so the length of the digit run is not known in advance. Instead of
  ## choosing between the wrapping and the checked loop by length, the first
  ## `safeDigits` characters are summed without checks and only the
  ## digits after them are checked.
  const isSignedTy = T is SomeSignedInt
  # The most decimal digits that cannot overflow `T`: the largest `n` with
  # `10^n - 1 <= high(T)`. `canNotOverflow` bounds every radix up to 16 at
  # once, `2 * sizeof(T) - isSigned` (15 for `int64`); for radix 10 alone the
  # exact bound is larger (18), so fewer numbers need checks.
  const safeDigits =
    when sizeof(T) == 1: 2       # 99 <= 127
    elif sizeof(T) == 2: 4       # 9999 <= 32767
    elif sizeof(T) == 4: 9       # 999999999 <= 2147483647
    elif isSignedTy: 18          # 10^18 - 1 <= 2^63 - 1
    else: 19                     # 10^19 - 1 <= 2^64 - 1
  var i = 0
  var isPositive = true
  var outOfRange = false
  if i < s.len:
    if s[i] == '+':
      inc i
    elif s[i] == '-':
      if isSignedTy:
        isPositive = false
        inc i
      elif i + 1 < s.len and s[i + 1] >= '0' and s[i + 1] <= '9':
        # a negative number is out of range for an unsigned type
        outOfRange = true
        inc i
  if i >= s.len or s[i] < '0' or s[i] > '9':
    return 0
  var res = T(0)
  # The wrapping loop. Its characters, digits and `_`s together, stop at
  # `safeDigits`, so it sums at most that many digits.
  let fastEnd = min(s.len, i + safeDigits)
  while i < fastEnd and s[i] >= '0' and s[i] <= '9':
    let x = T(ord(s[i]) - ord('0'))
    if isPositive:
      res = res * T(10) + x
    else:
      res = res * T(10) - x
    inc i
    while i < s.len and s[i] == '_': inc i
  # The checked loop, for any digits left; past an overflow it only skips.
  while i < s.len and s[i] >= '0' and s[i] <= '9':
    if not outOfRange:
      let x = T(ord(s[i]) - ord('0'))
      var mulOverflow = false
      var addOverflow = false
      {.keepOverflowFlag.}:
        res = res * T(10)
        mulOverflow = overflowFlag()
        if isPositive:
          res = res + x
        else:
          res = res - x
        addOverflow = overflowFlag()
      outOfRange = mulOverflow or addOverflow
    inc i
    while i < s.len and s[i] == '_': inc i
  if outOfRange:
    result = -i
  else:
    value = res
    result = i
