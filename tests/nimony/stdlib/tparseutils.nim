import std/[assertions, math, parseutils]

block:
  var ret: int64
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

block:
  var ret: uint64
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

block:
  var ret: float64
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
