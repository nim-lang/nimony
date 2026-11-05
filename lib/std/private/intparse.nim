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
