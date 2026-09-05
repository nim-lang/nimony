import std/[assertions, math, parseutils]

block: # parseHex
  var num: int = 0
  assert parseHex("", num) == 0
  assert num == 0
  assert parseHex("4E_69_ED", num) == 8
  assert num == 0x4E69ED
  assert parseHex("X", num) == 0
  assert num == 0x4E69ED
  assert parseHex("#ABC", num) == 4
  assert num == 0xABC
  assert parseHex("ABC", num, maxLen = 1) == 1
  assert num == 0xA
  assert parseHex("ABX", num, maxLen = 2) == 2
  assert num == 0xAB
  var num8: int8 = 0
  assert parseHex("0x_4E_69_ED", num8) == 11
  assert num8 == 0xED'i8
  assert parseHex("0x_4E_69_ED", num8, 3, 2) == 2
  assert num8 == 0x4E'i8
  var num8u: uint8 = 0
  assert parseHex("0x_4E_69_ED", num8u) == 11
  assert num8u == 237
  var num64: int64 = 0
  assert parseHex("4E69ED4E69ED", num64) == 12
  assert num64 == 86216859871725

  assert parseHex("x2x", num, start = 1) == 1
  assert num == 2
  assert parseHex("123def", num, start = 2, maxLen = 2) == 2
  assert num == 0x3d

block: # skipUntil
  assert skipUntil("Hello World", {'W', 'e'}) == 1
  assert skipUntil("Hello World", {'W'}) == 6
  assert skipUntil("Hello World", {'W', 'd'}) == 6
  assert skipUntil("Hello World", 'o') == 4
  assert skipUntil("Hello World", 'o', 4) == 0
  assert skipUntil("Hello World", 'W') == 6
  assert skipUntil("Hello World", 'w') == 11

block:
  var ret: int64 = 3'i64
  assert parseBiggestInt("0", ret) == 1
  assert ret == 0
  assert parseBiggestInt("1", ret) == 1
  assert ret == 1
  assert parseBiggestInt("-1", ret) == 2
  assert ret == -1
  assert parseBiggestInt("2", ret) == 1
  assert ret == 2
  assert parseBiggestInt("-2", ret) == 2
  assert ret == -2
  assert parseBiggestInt("10", ret) == 2
  assert ret == 10
  assert parseBiggestInt("-10", ret) == 3
  assert ret == -10
  assert parseBiggestInt("123", ret) == 3
  assert ret == 123
  assert parseBiggestInt("-123", ret) == 4
  assert ret == -123
  assert parseBiggestInt($high(int64), ret) == 19
  assert ret == high(int64)
  assert parseBiggestInt($low(int64), ret) == 20
  assert ret == low(int64)
  # out of range -> negative processed-char count, `ret` left unchanged
  ret = 7'i64
  assert parseBiggestInt("9223372036854775808", ret) == -19   # int64.high + 1
  assert ret == 7'i64
  assert parseBiggestInt("-9223372036854775809", ret) == -20  # int64.low - 1
  assert ret == 7'i64
  assert parseBiggestInt("99999999999999999999999", ret) == -23
  assert ret == 7'i64
  # no integer at all is still 0 (distinct from the overflow signal)
  assert parseBiggestInt("abc", ret) == 0
  assert ret == 7'i64

block:
  var ret: uint64 = 3'u64
  assert parseBiggestUInt("0", ret) == 1
  assert ret == 0'u64
  assert parseBiggestUInt("1", ret) == 1
  assert ret == 1'u64
  assert parseBiggestUInt("2", ret) == 1
  assert ret == 2'u64
  assert parseBiggestUInt("10", ret) == 2
  assert ret == 10'u64
  assert parseBiggestUInt("123", ret) == 3
  assert ret == 123'u64
  assert parseBiggestUInt($high(uint64), ret) == 20
  assert ret == high(uint64)
  # out of range -> negative processed-char count, `ret` left unchanged
  ret = 7'u64
  assert parseBiggestUInt("18446744073709551616", ret) == -20  # uint64.high + 1
  assert ret == 7'u64
  assert parseBiggestUInt("-5", ret) == -2                     # negative is out of range
  assert ret == 7'u64
  assert parseBiggestUInt("abc", ret) == 0
  assert ret == 7'u64

block:
  var ret: float64 = 3.0
  assert parseBiggestFloat("0", ret) == 1
  assert ret == 0.0
  assert parseBiggestFloat("0?", ret) == 1
  assert ret == 0.0
  assert parseBiggestFloat("1", ret) == 1
  assert ret == 1.0
  assert parseBiggestFloat("-1", ret) == 2
  assert ret == -1.0
  assert parseBiggestFloat("0.5", ret) == 3
  assert ret == 0.5
  assert parseBiggestFloat("-0.5", ret) == 4
  assert ret == -0.5
  assert parseBiggestFloat("0.25", ret) == 4
  assert ret == 0.25
  assert parseBiggestFloat("-0.25", ret) == 5
  assert ret == -0.25
  assert parseBiggestFloat("-0.25a", ret) == 5
  assert ret == -0.25
  assert parseBiggestFloat("1234567890123456", ret) == 16
  assert ret == 1234567890123456.0
  assert parseBiggestFloat("-1234567890123456", ret) == 17
  assert ret == -1234567890123456.0
  assert parseBiggestFloat("1.234567890123456", ret) == 17
  assert ret == 1.234567890123456
  assert parseBiggestFloat("-1.234567890123456", ret) == 18
  assert ret == -1.234567890123456
  assert parseBiggestFloat("1e0", ret) == 3
  assert ret == 1.0
  assert parseBiggestFloat("1e+0", ret) == 4
  assert ret == 1.0
  assert parseBiggestFloat("+1e+0", ret) == 5
  assert ret == 1.0
  assert parseBiggestFloat("-1e0", ret) == 4
  assert ret == -1.0
  assert parseBiggestFloat("-1e-0", ret) == 5
  assert ret == -1.0
  assert parseBiggestFloat("1e1", ret) == 3
  assert ret == 10
  assert parseBiggestFloat("+1e+1", ret) == 5
  assert ret == 10
  assert parseBiggestFloat("1e-1", ret) == 4
  assert ret == 0.1
  assert parseBiggestFloat("-1e-1", ret) == 5
  assert ret == -0.1
  assert parseBiggestFloat("+1e-1", ret) == 5
  assert ret == 0.1
  assert parseBiggestFloat("1e16", ret) == 4
  assert ret == 1e16
  assert parseBiggestFloat("+1e+16", ret) == 6
  assert ret == 1e16
  assert parseBiggestFloat("1e-16", ret) == 5
  assert ret == 1e-16
  assert parseBiggestFloat("1e300", ret) == 5
  assert ret == 1e300
  assert parseBiggestFloat("1e-300", ret) == 6
  assert ret == 1e-300
  assert parseBiggestFloat("2.3456789e300", ret) == 13
  assert ret == 2.3456789e300
  assert parseBiggestFloat("-2.3456789e-300", ret) == 15
  assert ret == -2.3456789e-300
  assert parseBiggestFloat("nan", ret) == 3
  assert ret.classify == fcNan
  assert parseBiggestFloat("NAN", ret) == 3
  assert ret.classify == fcNan
  assert parseBiggestFloat("inf", ret) == 3
  assert ret.classify == fcInf
  assert parseBiggestFloat("-inf", ret) == 4
  assert ret.classify == fcNegInf

block: # correctly rounded decimal -> float64, no `strtod`
  # The slow path is a big-decimal reader, so these must come out bit for bit
  # as the C library reads them: an exact halfway between two doubles goes
  # half-to-even, and digits past the 17th still decide the result.
  var ret: float64 = 3.0
  assert parseBiggestFloat("2.2250738585072011e-308", ret) == 23
  assert cast[int64](ret) == 0x000FFFFFFFFFFFFF'i64   # the largest subnormal
  assert parseBiggestFloat("2.2250738585072012e-308", ret) == 23
  assert cast[int64](ret) == 0x0010000000000000'i64   # rounds up to the smallest normal
  assert parseBiggestFloat("9007199254740993", ret) == 16        # 2^53 + 1
  assert cast[int64](ret) == 0x4340000000000000'i64
  assert parseBiggestFloat("9007199254740992.5", ret) == 18      # a tie, to even
  assert cast[int64](ret) == 0x4340000000000000'i64
  assert parseBiggestFloat("2.4703282292062327e-324", ret) == 23 # just under half of
  assert cast[int64](ret) == 0x0000000000000000'i64              # the smallest subnormal
  assert parseBiggestFloat("2.4703282292062328e-324", ret) == 23
  assert cast[int64](ret) == 0x0000000000000001'i64
  assert parseBiggestFloat("7.8459735791271921e65", ret) == 21
  assert cast[int64](ret) == 0x4D9DCD0089C1314E'i64
  assert parseBiggestFloat("123456789012345678901234567890", ret) == 30
  assert cast[int64](ret) == 0x45F8EE90FF6C373E'i64
  # The exact expansion of the double nearest 0.1, and one digit past it.
  assert parseBiggestFloat("0.1000000000000000055511151231257827021181583404541015625", ret) == 57
  assert cast[int64](ret) == 0x3FB999999999999A'i64
  assert parseBiggestFloat("0.10000000000000000555111512312578270211815834045410156251", ret) == 58
  assert cast[int64](ret) == 0x3FB999999999999A'i64
  assert parseBiggestFloat("1e-323", ret) == 6
  assert cast[int64](ret) == 0x0000000000000002'i64
  assert parseBiggestFloat("1.7976931348623158e308", ret) == 22  # still finite
  assert cast[int64](ret) == 0x7FEFFFFFFFFFFFFF'i64
  assert parseBiggestFloat("1e309", ret) == 5
  assert ret.classify == fcInf
