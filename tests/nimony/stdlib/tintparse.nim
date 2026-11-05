import std/[assertions]
import std/private/intparse

# Rust's tests for integer parsing (rust-lang/rust commit
# 70424d844bbe2c6dbc8f38dda80affd754a7cfe4, MIT / Apache-2.0), ported to
# `fromStrRadix`: `library/coretests/tests/num/mod.rs`, `int_macros.rs` and
# `uint_macros.rs`. Rust's `x.parse::<T>()` is `fromStrRadix(x, 10, v)`.
# `i128`/`u128` have no Nimony counterpart and are left out.

proc expectOk[T: SomeInteger](s: string; radix: int; expected: T) {.untyped.} =
  var v = T(0)
  assert fromStrRadix(s, radix, v) == ikNone, s
  assert v == expected, s

proc expectErr[T: SomeInteger](s: string; radix: int; kind: IntErrorKind) {.untyped.} =
  var v = T(7)
  assert fromStrRadix(s, radix, v) == kind, s
  assert v == T(7), s           # left unchanged on error

proc expectSomeErr[T: SomeInteger](s: string; radix: int) {.untyped.} =
  var v = T(7)
  assert fromStrRadix(s, radix, v) != ikNone, s
  assert v == T(7), s

proc parseOk[T: SomeInteger](s: string; expected: T) {.untyped.} =
  expectOk[T](s, 10, expected)

proc parseErr[T: SomeInteger](s: string; kind: IntErrorKind) {.untyped.} =
  expectErr[T](s, 10, kind)

proc signedTests[T: SomeSignedInt]() {.untyped.} =
  # int_macros.rs:272, test_from_str
  parseOk[T]("0", 0)
  parseOk[T]("3", 3)
  parseOk[T]("10", 10)
  parseOk[int32]("123456789", 123456789)
  parseOk[T]("00100", 100)
  parseOk[T]("-1", -1)
  parseOk[T]("-3", -3)
  parseOk[T]("-10", -10)
  parseOk[int32]("-123456789", -123456789)
  parseOk[T]("-00100", -100)
  expectSomeErr[T]("", 10)
  expectSomeErr[T](" ", 10)
  expectSomeErr[T]("x", 10)
  # int_macros.rs:294, test_from_str_radix
  expectOk[T]("123", 10, 123)
  expectOk[T]("1001", 2, 9)
  expectOk[T]("123", 8, 83)
  expectOk[int32]("123", 16, 291)
  expectOk[int32]("ffff", 16, 65535)
  expectOk[int32]("FFFF", 16, 65535)
  expectOk[T]("z", 36, 35)
  expectOk[T]("Z", 36, 35)
  expectOk[T]("-123", 10, -123)
  expectOk[T]("-1001", 2, -9)
  expectOk[T]("-123", 8, -83)
  expectOk[int32]("-123", 16, -291)
  expectOk[int32]("-ffff", 16, -65535)
  expectOk[int32]("-FFFF", 16, -65535)
  expectOk[T]("-z", 36, -35)
  expectOk[T]("-Z", 36, -35)
  expectSomeErr[T]("Z", 35)
  expectSomeErr[T]("-9", 2)
  expectSomeErr[T]("10_0", 10)
  expectSomeErr[uint32]("-9", 10)

proc unsignedTests[T: SomeUnsignedInt]() {.untyped.} =
  # uint_macros.rs:328, test_from_str
  parseOk[T]("0", 0)
  parseOk[T]("3", 3)
  parseOk[T]("10", 10)
  parseOk[uint32]("123456789", 123456789)
  parseOk[T]("00100", 100)
  expectSomeErr[T]("", 10)
  expectSomeErr[T](" ", 10)
  expectSomeErr[T]("x", 10)
  # uint_macros.rs:341, test_parse_bytes
  expectOk[T]("123", 10, 123)
  expectOk[T]("1001", 2, 9)
  expectOk[T]("123", 8, 83)
  expectOk[uint16]("123", 16, 291)
  expectOk[uint16]("ffff", 16, 65535)
  expectOk[T]("z", 36, 35)
  expectSomeErr[T]("Z", 10)
  expectSomeErr[T]("_", 2)

signedTests[int8]()
signedTests[int16]()
signedTests[int32]()
signedTests[int64]()
signedTests[int]()       # isize
unsignedTests[uint8]()
unsignedTests[uint16]()
unsignedTests[uint32]()
unsignedTests[uint64]()
unsignedTests[uint]()    # usize

block: # mod.rs:116, from_str_issue7588
  expectSomeErr[uint8]("1000", 10)
  expectSomeErr[int16]("80000", 10)

block: # mod.rs:131, test_int_from_str_overflow
  parseOk[int8]("127", 127)
  parseErr[int8]("128", ikPosOverflow)
  parseOk[int8]("-128", -128)
  parseErr[int8]("-129", ikNegOverflow)

  parseOk[int16]("32767", 32_767)
  parseErr[int16]("32768", ikPosOverflow)
  parseOk[int16]("-32768", -32_768)
  parseErr[int16]("-32769", ikNegOverflow)

  parseOk[int32]("2147483647", 2_147_483_647)
  parseErr[int32]("2147483648", ikPosOverflow)
  parseOk[int32]("-2147483648", -2_147_483_648)
  parseErr[int32]("-2147483649", ikNegOverflow)

  parseOk[int64]("9223372036854775807", 9_223_372_036_854_775_807'i64)
  parseErr[int64]("9223372036854775808", ikPosOverflow)
  parseOk[int64]("-9223372036854775808", low(int64))
  parseErr[int64]("-9223372036854775809", ikNegOverflow)

block: # mod.rs:158, test_can_not_overflow
  # Rust passes the digit slice; the port takes `sizeof(T)` and its length.
  proc canOverflow[T: SomeInteger](radix: int; input: string): bool {.untyped.} =
    result = not canNotOverflow(radix, T is SomeSignedInt, sizeof(T), input.len)

  # Positive tests:
  assert not canOverflow[int8](16, "F")
  assert not canOverflow[uint8](16, "FF")
  assert not canOverflow[int8](10, "9")
  assert not canOverflow[uint8](10, "99")

  # Negative tests: the smallest overflowing number, `high(T) + 1`, written in
  # every base, must be deemed able to overflow. Rust computes it in `u128`;
  # here `high(T)` is written in the base and one is added to the digits.
  proc digitsOfSuccessor(x: uint64; base: int): int =
    var digits: seq[int] = @[]
    var y = x
    while true:
      digits.add int(y mod uint64(base))
      y = y div uint64(base)
      if y == 0'u64: break
    var carry = 1
    var k = 0
    while carry != 0 and k < digits.len:
      let d = digits[k] + carry
      digits[k] = d mod base
      carry = d div base
      inc k
    result = digits.len + carry

  proc check[T: SomeInteger]() {.untyped.} =
    for base in 2..36:
      let n = digitsOfSuccessor(uint64(high(T)), base)
      assert not canNotOverflow(base, T is SomeSignedInt, sizeof(T), n)

  check[int8]()
  check[int16]()
  check[int32]()
  check[int64]()
  check[int]()
  check[uint]()
  check[uint8]()
  check[uint16]()
  check[uint32]()
  check[uint64]()

block: # mod.rs:227, test_leading_plus
  parseOk[uint8]("+127", 127)
  parseOk[int64]("+9223372036854775807", 9223372036854775807'i64)

block: # mod.rs:233, test_invalid
  parseErr[int8]("--129", ikInvalidDigit)
  parseErr[int8]("++129", ikInvalidDigit)
  parseErr[uint8]("Съешь", ikInvalidDigit)
  parseErr[uint8]("123Hello", ikInvalidDigit)
  parseErr[int8]("--", ikInvalidDigit)
  parseErr[int8]("-", ikInvalidDigit)
  parseErr[int8]("+", ikInvalidDigit)
  parseErr[uint8]("-1", ikInvalidDigit)

block: # mod.rs:245, test_empty
  parseErr[uint8]("", ikEmpty)

block: # mod.rs:256, _TEST_CONST_PARSE (run time here)
  expectOk[int16]("-8000", 16, -0x8000)
  expectOk[uint64]("12345", 10, 12345)
  expectErr[int8]("+", 10, ikInvalidDigit)

# `parseDecimal`, the `parseutils` prefix reader, must agree with
# `fromStrRadix` on every string both accept: the whole string, no `_`.

proc agrees[T: SomeInteger](s: string) {.untyped.} =
  var a = T(7)
  var b = T(7)
  let kind = fromStrRadix(s, 10, a)
  let n = parseDecimal(s, b)
  if T is SomeUnsignedInt and kind == ikInvalidDigit and s.len > 1 and
      s[0] == '-' and fromStrRadix(s.toOpenArray(1, s.len - 1), 10, a) != ikInvalidDigit:
    # the one intended difference: `-digits` is out of range (`-n`) for
    # `parseutils`, an invalid digit for Rust
    assert n == -s.len, s
    assert b == T(7), s
  elif kind == ikNone:
    assert n == s.len, s
    assert a == b, s
  elif kind == ikPosOverflow or kind == ikNegOverflow:
    assert n == -s.len, s
    assert b == T(7), s
  else:
    assert n != s.len and n != -s.len, s

proc exhaustive[T: SomeInteger]() {.untyped.} =
  # every value of `T`, one past each end, and leading zeros
  var v = int64(low(T)) - 1
  while v <= int64(high(T)) + 1:
    agrees[T]($v)
    agrees[T]("+" & $v)
    agrees[T]("000" & $v)
    inc v

exhaustive[int8]()
exhaustive[uint8]()
exhaustive[int16]()
exhaustive[uint16]()

block: # 64 bits: random values of every length, and the ends
  var x = 0x9E3779B97F4A7C15'u64
  for k in 0 ..< 20000:
    x = x xor (x shl 13)
    x = x xor (x shr 7)
    x = x xor (x shl 17)
    let shifted = x shr (k mod 64)
    agrees[uint64]($shifted)
    agrees[int64]($cast[int64](shifted))
    agrees[int32]($cast[int64](shifted))
    agrees[uint32]($shifted)
    var back = 0'u64
    assert parseDecimal($shifted, back) == len($shifted)
    assert back == shifted
  for s in ["9223372036854775807", "9223372036854775808", "-9223372036854775808",
            "-9223372036854775809", "18446744073709551615", "18446744073709551616",
            "99999999999999999999", "-99999999999999999999", "4294967295",
            "4294967296", "2147483647", "2147483648", "-2147483648", "-2147483649"]:
    agrees[int64](s)
    agrees[uint64](s)
    agrees[int32](s)
    agrees[uint32](s)
    agrees[int](s)
    agrees[uint](s)
