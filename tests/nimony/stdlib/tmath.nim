import std/[math, assertions]

let
  FLT_MIN {.importc: "FLT_MIN", header: "<float.h>".}: float32
  DBL_MIN {.importc: "DBL_MIN", header: "<float.h>".}: float64

block:  # signbit
  assert not signbit(0.0)
  assert signbit(-0.0)
  assert signbit(0.0 * -1.0)
  assert signbit(-0.1)
  assert not signbit(0.1)
  assert signbit(-1.0'f)
  assert not signbit(0.0'f32)
  assert signbit(-0.0'f32)
  assert not signbit(1.0'f32)
  assert signbit(-1.0'f32)

  assert not signbit(Inf)
  assert signbit(-Inf)
  assert not signbit(NaN)
  assert signbit(-NaN)

block:  # copySign
  assert copySign(10.0, 1.0) == 10.0
  assert copySign(10.0, -1.0) == -10.0
  assert copySign(-10.0, -1.0) == -10.0
  assert copySign(-10.0, 1.0) == 10.0
  assert copySign(float(10), -1.0) == -10.0

  assert copySign(10.0'f64, 1.0) == 10.0
  assert copySign(10.0'f64, -1.0) == -10.0
  assert copySign(-10.0'f64, -1.0) == -10.0
  assert copySign(-10.0'f64, 1.0) == 10.0
  assert copySign(10'f64, -1.0) == -10.0

  assert copySign(10.0'f32, 1.0'f32) == 10.0
  assert copySign(10.0'f32, -1.0'f32) == -10.0
  assert copySign(-10.0'f32, -1.0'f32) == -10.0
  assert copySign(-10.0'f32, 1.0'f32) == 10.0
  assert copySign(10'f32, -1.0'f32) == -10.0

  assert copySign(Inf, -1.0) == -Inf
  assert copySign(-Inf, 1.0) == Inf
  assert copySign(Inf, 1.0) == Inf
  assert copySign(-Inf, -1.0) == -Inf
  assert copySign(Inf, 0.0) == Inf
  assert copySign(Inf, -0.0) == -Inf
  assert copySign(-Inf, 0.0) == Inf
  assert copySign(-Inf, -0.0) == -Inf
  assert copySign(1.0, -0.0) == -1.0
  assert copySign(0.0, -0.0) == -0.0
  assert copySign(-1.0, 0.0) == 1.0
  assert copySign(10.0, 0.0) == 10.0
  assert copySign(-1.0, NaN) == 1.0
  assert copySign(10.0, NaN) == 10.0

  assert copySign(NaN, NaN).isNaN
  assert copySign(-NaN, NaN).isNaN
  assert copySign(NaN, -NaN).isNaN
  assert copySign(-NaN, -NaN).isNaN
  assert copySign(NaN, 0.0).isNaN
  assert copySign(NaN, -0.0).isNaN
  assert copySign(-NaN, 0.0).isNaN
  assert copySign(-NaN, -0.0).isNaN

  assert copySign(-1.0, NaN) == 1.0
  assert copySign(-1.0, -NaN) == -1.0
  assert copySign(1.0, copySign(NaN, -1.0)) == -1.0

block: # classify
  assert classify(1.0) == fcNormal
  assert classify(-1.0) == fcNormal
  assert classify(0.0) == fcZero
  assert classify(0.0 * -1.0) == fcNegZero
  assert classify(0.0 / 0.0) == fcNan
  assert classify(1.0 / 0.0) == fcInf
  assert classify(-1.0 / 0.0) == fcNegInf
  assert classify(Inf) == fcInf
  assert classify(NaN) == fcNan
  assert classify(DBL_MIN / 2.0) == fcSubnormal
  assert classify(1.0'f32) == fcNormal
  assert classify(FLT_MIN / 2.0'f32) == fcSubnormal

block: # isNaN
  assert NaN.isNaN
  assert not Inf.isNaN
  assert isNaN(Inf - Inf)
  assert not isNaN(0.0)
  assert not isNaN(3.1415926)
  assert not isNaN(0'f32)
  assert (-NaN).isNaN
  assert (NaN + 1.0).isNaN

block: # almostEqual
  assert almostEqual(3.141592653589793, 3.1415926535897936)
  assert almostEqual(1.6777215e7'f32, 1.6777216e7'f32)
  assert almostEqual(Inf, Inf)
  assert almostEqual(-Inf, -Inf)
  assert not almostEqual(Inf, -Inf)
  assert not almostEqual(-Inf, Inf)
  assert not almostEqual(Inf, NaN)
  assert not almostEqual(NaN, NaN)

block: # sgn
  assert sgn(1'i8) == 1
  assert sgn(1'i16) == 1
  assert sgn(1'i32) == 1
  assert sgn(1'i64) == 1
  assert sgn(1'u8) == 1
  assert sgn(1'u16) == 1
  assert sgn(1'u32) == 1
  assert sgn(1'u64) == 1
  assert sgn(-12342.8844'f32) == -1
  assert sgn(123.9834'f64) == 1
  assert sgn(0'i32) == 0
  assert sgn(0'f32) == 0
  assert sgn(-0.0'f64) == 0
  assert sgn(-Inf) == -1
  assert sgn(Inf) == 1
  assert sgn(NaN) == 0

block: # frexp
  assert frexp(8.0) == (0.5, 4)
  assert frexp(-8.0) == (-0.5, 4)
  assert frexp(0.0) == (0.0, 0)
  assert frexp(14.0) == (0.5 + 0.25 + 0.125, 4)

  # special cases:
  assert frexp(-0.0).frac.signbit # signbit preserved for +-0
  assert frexp(Inf).frac == Inf # +- Inf preserved
  assert frexp(NaN).frac.isNaN

block: # floor
  assert floor(2.9) == 2.0
  assert floor(2.1) == 2.0
  assert floor(0.0) == 0.0
  assert floor(-2.1) == -3.0
  assert floor(-2.9) == -3.0

block: # ceil
  assert ceil(2.9) == 3.0
  assert ceil(2.1) == 3.0
  assert ceil(0.0) == 0.0
  assert ceil(-2.1) == -2.0
  assert ceil(-2.9) == -2.0

block: # round
  assert round(2.9) == 3.0
  assert round(2.1) == 2.0
  assert round(0.0) == 0.0
  assert round(-2.1) == -2.0
  assert round(-2.9) == -3.0

block: # trunc
  assert trunc(2.9) == 2.0
  assert trunc(2.1) == 2.0
  assert trunc(0.0) == 0.0
  assert trunc(-2.1) == -2.0
  assert trunc(-2.9) == -2.0

block: # mod
  assert  6.5 mod  2.5 ==  1.5
  assert -6.5 mod  2.5 == -1.5
  assert  6.5 mod -2.5 ==  1.5
  assert -6.5 mod -2.5 == -1.5

block: # floorMod/floorDiv
  assert floorDiv(8, 3) == 2
  assert floorMod(8, 3) == 2

  assert floorDiv(8, -3) == -3
  assert floorMod(8, -3) == -1

  assert floorDiv(-8, 3) == -3
  assert floorMod(-8, 3) == 1

  assert floorDiv(-8, -3) == 2
  assert floorMod(-8, -3) == -2

  assert floorMod(8.0, -3.0) == -1.0
  assert floorMod(-8.5, 3.0) == 0.5

  assert floorDiv(int.high, 1) == int.high
  assert floorMod(int.high, 1) == 0

block: # euclDiv/euclMod
  assert euclDiv(8, 3) == 2
  assert euclMod(8, 3) == 2

  assert euclDiv(8, -3) == -2
  assert euclMod(8, -3) == 2

  assert euclDiv(-8, 3) == -3
  assert euclMod(-8, 3) == 1

  assert euclDiv(-8, -3) == 3
  assert euclMod(-8, -3) == 1

  assert euclMod(8.0, -3.0) == 2.0
  assert euclMod(-8.5, 3.0) == 0.5

  assert euclDiv(9, 3) == 3
  assert euclMod(9, 3) == 0

  assert euclDiv(9, -3) == -3
  assert euclMod(9, -3) == 0

  assert euclDiv(-9, 3) == -3
  assert euclMod(-9, 3) == 0

  assert euclDiv(-9, -3) == 3
  assert euclMod(-9, -3) == 0

block: # ceilDiv
  assert ceilDiv(8,  3) ==  3
  assert ceilDiv(8,  4) ==  2
  assert ceilDiv(8,  5) ==  2
  assert ceilDiv(11, 3) ==  4
  assert ceilDiv(12, 3) ==  4
  assert ceilDiv(13, 3) ==  5
  assert ceilDiv(41, 7) ==  6
  assert ceilDiv(0,  1) ==  0
  assert ceilDiv(1,  1) ==  1
  assert ceilDiv(1,  2) ==  1
  assert ceilDiv(2,  1) ==  2
  assert ceilDiv(2,  2) ==  1
  assert ceilDiv(0, high(int)) == 0
  assert ceilDiv(1, high(int)) == 1
  assert ceilDiv(0, high(int) - 1) == 0
  assert ceilDiv(1, high(int) - 1) == 1
  assert ceilDiv(high(int) div 2, high(int) div 2 + 1) == 1
  assert ceilDiv(high(int) div 2, high(int) div 2 + 2) == 1
  assert ceilDiv(high(int) div 2 + 1, high(int) div 2) == 2
  assert ceilDiv(high(int) div 2 + 2, high(int) div 2) == 2
  assert ceilDiv(high(int) div 2 + 1, high(int) div 2 + 1) == 1
  assert ceilDiv(high(int), 1) == high(int)
  assert ceilDiv(high(int) - 1, 1) == high(int) - 1
  assert ceilDiv(high(int) - 1, 2) == high(int) div 2
  assert ceilDiv(high(int) - 1, high(int)) == 1
  assert ceilDiv(high(int) - 1, high(int) - 1) == 1
  assert ceilDiv(high(int) - 1, high(int) - 2) == 2
  assert ceilDiv(high(int), high(int)) == 1
  assert ceilDiv(high(int), high(int) - 1) == 2
  assert ceilDiv(255'u8,  1'u8) == 255'u8
  assert ceilDiv(254'u8,  2'u8) == 127'u8
  #[ doAssertRaises is not implemented yet
  when not defined(danger):
    doAssertRaises(AssertionDefect): discard ceilDiv(41,  0)
    doAssertRaises(AssertionDefect): discard ceilDiv(41, -1)
    doAssertRaises(AssertionDefect): discard ceilDiv(-1,  1)
    doAssertRaises(AssertionDefect): discard ceilDiv(-1, -1)
    doAssertRaises(AssertionDefect): discard ceilDiv(254'u8, 3'u8)
    doAssertRaises(AssertionDefect): discard ceilDiv(255'u8, 2'u8)
  ]#

block: # divmod
  assert divmod(int.high, 1) == (int.high, 0)
  assert divmod(-1073741823, 17) == (-63161283, -12)
  assert divmod(int32.high, 1.int32) == (int32.high, 0.int32)
  assert divmod(1073741823.int32, 5.int32) == (214748364.int32, 3.int32)
  assert divmod(4611686018427387903.int64, 5.int64) == (922337203685477580.int64, 3.int64)

block: # sum
  let empty: seq[int] = @[]
  assert sum(empty) == 0
  assert sum([1, 2, 3, 4]) == 10
  assert sum([-4, 3, 5]) == 4

block: # cumsum
  block:  # int
    var counts = [1, 2, 3, 4]
    counts.cumsum
    assert counts == [1, 3, 6, 10]
    var empty: seq[int] = @[]
    empty.cumsum
    assert empty == @[]

  block: # float
    var counts = [1.0, 2.0, 3.0, 4.0]
    counts.cumsum
    assert counts == [1.0, 3.0, 6.0, 10.0]
    var empty: seq[float] = @[]
    empty.cumsum
    assert empty == @[]

block: # prod
  let empty: seq[int] = @[]
  assert prod(empty) == 1
  assert prod([1, 2, 3, 4]) == 24
  assert prod([-4, 3, 5]) == -60
  assert almostEqual(prod([1.5, 3.4]), 5.1)
  let x: seq[float] = @[]
  assert prod(x) == 1.0

block: # cumprod
  block: # int
    var counts = [1, 2, 3, 4]
    counts.cumprod
    assert counts == [1, 2, 6, 24]

  block: # float
    var counts = [1.0, 2.0, 3.0, 4.0]
    counts.cumprod
    assert counts == [1.0, 2.0, 6.0, 24.0]

block: # sqrt
  assert sqrt(-1.0).isNaN
  assert sqrt(0.0) == 0.0
  assert sqrt(1.0) == 1.0
  assert almostEqual(sqrt(4.0), 2.0)
  assert almostEqual(sqrt(4.0'f32), 2.0'f32)
  assert almostEqual(sqrt(9.0), 3.0)

block: # cbrt
  assert cbrt(-1.0) == -1.0
  assert cbrt(-1.0'f32) == -1.0'f32
  assert cbrt(0.0) == 0.0
  assert cbrt(1.0) == 1.0
  assert almostEqual(cbrt(8.0), 2.0)
  assert almostEqual(cbrt(27.0), 3.0)

block: # pow
  assert pow(1.0, 1.0) == 1.0
  assert pow(2.0, 0.0) == 1.0
  assert pow(2.0, -1.0) == 0.5
  assert pow(2.0, 2.0) == 4.0
  assert pow(2.0'f32, 2.0'f32) == 4.0'f32

block: # hypot
  assert almostEqual(hypot(3.0, 4.0), 5.0)
  assert almostEqual(hypot(3.0'f32, 4.0'f32), 5.0'f32)
  assert almostEqual(hypot(6.0, 8.0), 10.0)
  assert almostEqual(hypot(6.0'f32, 8.0'f32), 10.0'f32)

block: # exp
  assert exp(0.0) == 1.0
  assert almostEqual(exp(1.0), E)
  assert almostEqual(exp(2.0), E * E)
  assert almostEqual(exp(1.0'f32), float32(E))

block: # ln
  assert almostEqual(ln(1.0), 0.0)
  assert almostEqual(ln(1.0'f32), 0.0'f32)
  assert almostEqual(ln(E), 1.0)
  assert ln(0.0) == -Inf
  assert ln(-0.0) == -Inf
  assert ln(-12.0).isNaN

block: # log2
  assert log2(8.0'f64) == 3.0'f64
  assert log2(4.0'f64) == 2.0'f64
  assert log2(2.0'f64) == 1.0'f64
  assert log2(1.0'f64) == 0.0'f64
  assert classify(log2(0.0'f64)) == fcNegInf

  assert log2(8.0'f32) == 3.0'f32
  assert log2(4.0'f32) == 2.0'f32
  assert log2(2.0'f32) == 1.0'f32
  assert log2(1.0'f32) == 0.0'f32
  assert classify(log2(0.0'f32)) == fcNegInf

block: # log10
  assert log10(10.0) == 1.0
  assert log10(10.0'f32) == 1.0'f32
  assert log10(100.0) == 2.0
  assert log10(100.0'f32) == 2.0'f32
  assert log10(0.0).classify == fcNegInf
  assert log10(-0.0).classify == fcNegInf
  assert log10(0.0'f32).classify == fcNegInf
  assert log10(-12.0).isNaN
  assert log10(-12.0'f32).isNaN

block: # log
  assert log(9.0, 3.0) == 2.0
  assert log(0.125, 0.5) == 3.0
  assert log(2.0, 4.0) == 0.5
  assert log(0.25, 4.0) == -1.0
  assert log(1.0, 100.0) == 0.0
  assert log(100000.0, 10.0) == 5.0
  assert log(-1.0, 2.0).isNaN
  assert log(4.0, -2.0).isNaN
  assert log(9'f32, 3'f32) == 2'f32
  assert log(0.125'f32, 0.5'f32) == 3'f32
  assert log(2'f32, 4'f32) == 0.5'f32
  assert log(0.25'f32, 4'f32) == -1'f32
  assert log(1'f32, 100'f32) == 0'f32
  assert log(100000'f32, 10'f32) == 5'f32
  assert log(-1'f32, 2'f32).isNaN
  assert log(4'f32, -2'f32).isNaN

block: # `^`
  assert 5 ^ 2 == 25
  assert 0.5 ^ 2 == 0.25
  assert 0.5'f32 ^ 2 == 0.25'f32
  assert 2 ^ 3 == 8
  assert 0 ^ 0 == 1
  assert 0 ^ 1 == 0
  assert 1 ^ 1 == 1
  assert -1 ^ 0 == 1
  assert -1 ^ 1 == -1
  assert -1 ^ 2 == 1
  assert -1 ^ 11 == -1
  assert -1 ^ 111 == -1
  assert -1 ^ 112 == 1
  assert 10 ^ 5 == 100000

block: # isPowerOfTwo
  assert not isPowerOfTwo(-16)
  assert not isPowerOfTwo(-3)
  assert not isPowerOfTwo(-2)
  assert not isPowerOfTwo(-1)
  assert not isPowerOfTwo(0)
  assert isPowerOfTwo(1)
  assert isPowerOfTwo(2)
  assert not isPowerOfTwo(3)
  assert isPowerOfTwo(4)
  assert not isPowerOfTwo(5)
  assert not isPowerOfTwo(6)
  assert isPowerOfTwo(16)
  assert isPowerOfTwo(int.high div 2 + 1)
  assert not isPowerOfTwo(int.high)

block: # nextPowerOfTwo
  assert nextPowerOfTwo(-16) == 1
  assert nextPowerOfTwo(0) == 1
  assert nextPowerOfTwo(1) == 1
  assert nextPowerOfTwo(2) == 2
  assert nextPowerOfTwo(3) == 4
  assert nextPowerOfTwo(4) == 4
  assert nextPowerOfTwo(5) == 8
  assert nextPowerOfTwo(6) == 8
  assert nextPowerOfTwo(7) == 8
  assert nextPowerOfTwo(8) == 8
  assert nextPowerOfTwo(9) == 16
  assert nextPowerOfTwo(15) == 16
  assert nextPowerOfTwo(16) == 16
  assert nextPowerOfTwo(17) == 32
  assert nextPowerOfTwo(int.high div 2) == int.high div 2 + 1

block: # degToRad
  assert almostEqual(degToRad(-45.0), -PI/4.0)
  assert almostEqual(degToRad(0.0), 0.0)
  assert almostEqual(degToRad(45.0), PI/4.0)
  assert almostEqual(degToRad(180.0), PI)

block: # radToDeg
  assert almostEqual(radToDeg(-PI/4.0), -45.0)
  assert almostEqual(radToDeg(0.0), 0.0)
  assert almostEqual(radToDeg(PI/4.0), 45.0)
  assert almostEqual(radToDeg(PI), 180.0)

block: # sin
  assert almostEqual(sin(-PI/6.0), -0.5)
  assert almostEqual(sin(0.0), 0.0)
  assert almostEqual(sin(PI/6.0), 0.5)
  assert almostEqual(sin(PI/2.0), 1.0)
  assert almostEqual(sin(float32((-PI/6.0))), -0.5'f32)
  assert almostEqual(sin(0.0'f32), 0.0'f32)
  assert almostEqual(sin(float32(PI/6.0)), 0.5'f32)
  assert almostEqual(sin(float32(PI/2.0)), 1.0'f32)

block: # cos
  assert almostEqual(cos(-PI/3.0), 0.5)
  assert almostEqual(cos(0.0), 1.0)
  assert almostEqual(cos(PI/3.0), 0.5)
  assert almostEqual(cos(PI), -1.0)
  assert almostEqual(cos(float32(-PI/3.0)), 0.5'f32)
  assert almostEqual(cos(0.0'f32), 1.0'f32)
  assert almostEqual(cos(float32(PI/3.0)), 0.5'f32)
  assert almostEqual(cos(float32(PI)), -1.0'f32)

block: # tan
  assert almostEqual(tan(-PI/4.0), -1.0)
  assert almostEqual(tan(0.0), 0.0)
  assert almostEqual(tan(PI/4.0), 1.0)
  assert almostEqual(tan(PI/3.0), sqrt(3.0))
  assert almostEqual(tan(float32(-PI/4.0)), -1.0'f32)
  assert almostEqual(tan(0.0'f32), 0.0'f32)
  assert almostEqual(tan(float32(PI/4.0)), 1.0'f32)
  assert almostEqual(tan(float32(PI/3.0)), sqrt(3.0'f32))

block: # arcsin
  assert almostEqual(arcsin(-0.5), -PI/6.0)
  assert almostEqual(arcsin(0.0), 0.0)
  assert almostEqual(arcsin(0.5), PI/6.0)
  assert almostEqual(arcsin(1.0), PI/2.0)
  assert almostEqual(arcsin(-0.5'f32), float32(-PI/6.0))
  assert almostEqual(arcsin(0.0'f32), 0.0'f32)
  assert almostEqual(arcsin(0.5'f32), float32(PI/6.0))
  assert almostEqual(arcsin(1.0'f32), float32(PI/2.0))

block: # arccos
  assert almostEqual(arccos(1.0), 0.0)
  assert almostEqual(arccos(0.5), PI/3.0)
  assert almostEqual(arccos(-0.5), PI*2.0/3.0)
  assert almostEqual(arccos(-1.0), PI)
  assert almostEqual(arccos(1.0'f32), 0.0'f32)
  assert almostEqual(arccos(0.5'f32), float32(PI/3.0))
  assert almostEqual(arccos(-0.5'f32), float32(PI*2.0/3.0))
  assert almostEqual(arccos(-1.0'f32), float32(PI))

block: # arctan
  assert almostEqual(arctan(-1.0), -PI/4.0)
  assert almostEqual(arctan(0.0), 0.0)
  assert almostEqual(arctan(1.0), PI/4.0)
  assert almostEqual(arctan(sqrt(3.0)), PI/3.0)
  assert almostEqual(arctan(-1.0'f32), float32(-PI/4.0))
  assert almostEqual(arctan(0.0'f32), 0.0'f32)
  assert almostEqual(arctan(1.0'f32), float32(PI/4.0))
  assert almostEqual(arctan(sqrt(3.0'f32)), float32(PI/3.0))

block: # arctan2
  assert almostEqual(arctan2(-1.0, -1.0), -PI*3.0/4.0)
  assert almostEqual(arctan2(0.0, 1.0), 0.0)
  assert almostEqual(arctan2(1.0, 1.0), PI/4.0)
  assert almostEqual(arctan2(1.0, -1.0), PI*3.0/4.0)
  assert almostEqual(arctan2(-1.0'f32, -1.0'f32), float32(-PI*3.0/4.0))
  assert almostEqual(arctan2(0.0'f32, 1.0'f32), 0.0'f32)
  assert almostEqual(arctan2(1.0'f32, 1.0'f32), float32(PI/4.0))
  assert almostEqual(arctan2(1.0'f32, -1.0'f32), float32(PI*3.0/4.0))

block: # sinh
  assert almostEqual(sinh(ln(-1.0 + sqrt(2.0))), -1.0)
  assert almostEqual(sinh(0.0), 0.0)
  assert almostEqual(sinh(ln(0.5 + sqrt(1.25))), 0.5)
  assert almostEqual(sinh(ln(1.0 + sqrt(2.0))), 1.0)
  assert almostEqual(sinh(ln(-1'f32 + sqrt(2'f32))), -1'f32)
  assert almostEqual(sinh(0'f32), 0'f32)
  assert almostEqual(sinh(ln(0.5'f32 + sqrt(1.25'f32))), 0.5'f32)
  assert almostEqual(sinh(ln(1'f32 + sqrt(2'f32))), 1'f32)

block: # cosh
  assert almostEqual(cosh(0.0), 1.0)
  assert almostEqual(cosh(ln(sqrt(2.0) + 1.0)), sqrt(2.0))
  assert almostEqual(cosh(ln(2.0 + sqrt(3.0))), 2.0)
  assert almostEqual(cosh(ln(2.0 - sqrt(3.0))), 2.0)
  assert almostEqual(cosh(0'f32), 1'f32)
  assert almostEqual(cosh(ln(sqrt(2'f32) + 1'f32)), sqrt(2'f32))
  assert almostEqual(cosh(ln(2'f32 + sqrt(3'f32))), 2'f32)
  assert almostEqual(cosh(ln(2'f32 - sqrt(3'f32))), 2'f32)

block: # tanh
  assert almostEqual(tanh(0.0), 0.0)
  assert almostEqual(tanh(0.5 * ln(3.0)), 0.5)
  assert almostEqual(tanh(-0.5 * ln(3.0)), -0.5)
  assert almostEqual(tanh(0.5 * ln(7.0)), 0.75)
  assert almostEqual(tanh(0'f32), 0'f32)
  assert almostEqual(tanh(0.5'f32 * ln(3'f32)), 0.5'f32)
  assert almostEqual(tanh(-0.5'f32 * ln(3'f32)), -0.5'f32)
  assert almostEqual(tanh(0.5'f32 * ln(7'f32)), 0.75'f32)

block: # arcsinh
  assert almostEqual(arcsinh(-1.0), ln(sqrt(2.0) - 1.0))
  assert almostEqual(arcsinh(0.0), 0.0)
  assert almostEqual(arcsinh(0.75), ln(2.0))
  assert almostEqual(arcsinh(4.0/3.0), ln(3.0))
  assert almostEqual(arcsinh(-1'f32), ln(sqrt(2'f32) - 1'f32))
  assert almostEqual(arcsinh(0'f32), 0'f32)
  assert almostEqual(arcsinh(0.75'f32), ln(2'f32))
  assert almostEqual(arcsinh(4'f32/3'f32), ln(3'f32))

block: # arccosh
  assert almostEqual(arccosh(1.0), 0.0)
  assert almostEqual(arccosh(2.0), ln(2.0 + sqrt(3.0)))
  assert almostEqual(arccosh(1.25), ln(2.0))
  assert almostEqual(arccosh(5.0/3.0), ln(3.0))
  assert almostEqual(arccosh(1'f32), 0'f32)
  assert almostEqual(arccosh(2'f32), ln(2'f32 + sqrt(3'f32)))
  assert almostEqual(arccosh(1.25'f32), ln(2'f32))
  assert almostEqual(arccosh(5'f32/3'f32), ln(3'f32))

block: # arctanh
  assert almostEqual(arctanh(-0.5), -0.5 * ln(3.0))
  assert almostEqual(arctanh(0.0), 0.0)
  assert almostEqual(arctanh(0.5), 0.5 * ln(3.0))
  assert almostEqual(arctanh(0.75), 0.5 * ln(7.0))
  assert almostEqual(arctanh(-0.5'f32), -0.5'f32 * ln(3'f32))
  assert almostEqual(arctanh(0'f32), 0'f32)
  assert almostEqual(arctanh(0.5'f32), 0.5'f32 * ln(3'f32))
  assert almostEqual(arctanh(0.75'f32), 0.5'f32 * ln(7'f32))

block: # erf
  assert almostEqual(erf(0.0), 0.0)
  assert erf(6.0) > erf(5.0)
  assert abs(erf(3.0) - 1.0) < 0.01
  assert almostEqual(erf(1.0), -erf(-1.0))
  assert almostEqual(erf(0'f32), 0'f32)
  assert erf(6'f32) >= erf(5'f32)
  assert abs(erf(3'f32) - 1'f32) < 0.01'f32
  assert almostEqual(erf(1'f32), -erf(-1'f32))

block: # erfc
  assert almostEqual(erfc(0.0), 1.0)
  assert erfc(6.0) < erfc(5.0)
  assert almostEqual(1.0 - erfc(1.0), erfc(-1.0) - 1.0)
  assert abs(erfc(3.0)) < 0.01
  assert almostEqual(erfc(0'f32), 1'f32)
  assert erfc(6'f32) < erfc(5'f32)
  assert almostEqual(1'f32 - erfc(1'f32), erfc(-1'f32) - 1'f32)
  assert abs(erfc(3'f32)) < 0.01'f32

block: # gamma
  assert gamma(5.0) == 24.0 # 4!
  assert almostEqual(gamma(0.5), sqrt(PI))
  assert almostEqual(gamma(-0.5), -2.0 * sqrt(PI))
  assert almostEqual(gamma(2.0), 1.0)
  assert gamma(5'f32) == 24'f32 # 4!
  assert almostEqual(gamma(0.5'f32), sqrt(PI.float32))
  assert almostEqual(gamma(-0.5'f32), -2'f32 * sqrt(PI.float32))
  assert almostEqual(gamma(2'f32), 1'f32)

block: # lgamma
  assert lgamma(1.0) == 0.0 # ln(1.0) == 0.0
  assert almostEqual(lgamma(0.5), 0.5 * ln(PI))
  assert lgamma(0.0).classify == fcInf
  assert lgamma(-1.0).classify == fcInf
  assert lgamma(1'f32) == 0'f32
  assert almostEqual(lgamma(0.5'f32), 0.5'f32 * ln(PI.float32))
  assert lgamma(0'f32).classify == fcInf
  assert lgamma(-1'f32).classify == fcInf

block: # splitDecimal
  assert splitDecimal(54.674).intpart == 54.0
  assert almostEqual(splitDecimal(54.674).floatpart, 0.674)
  assert splitDecimal(-693.4375).intpart == -693.0
  assert almostEqual(splitDecimal(-693.4375).floatpart, -0.4375)
  assert splitDecimal(0.0) == (0.0, 0.0)
  assert splitDecimal(1.0) == (1.0, 0.0)
  assert splitDecimal(0.0625) == (0.0, 0.0625)
  assert splitDecimal(0.9375) == (0.0, 0.9375)
  assert splitDecimal(1.0625) == (1.0, 0.0625)
  assert splitDecimal(1.9375) == (1.0, 0.9375)
  assert splitDecimal(-1.0) == (-1.0, 0.0)
  assert splitDecimal(-0.0625) == (0.0, -0.0625)
  assert splitDecimal(-0.9375) == (0.0, -0.9375)
  assert splitDecimal(-1.0625) == (-1.0, -0.0625)
  assert splitDecimal(-1.9375) == (-1.0, -0.9375)
  assert splitDecimal(65536.0) == (65536.0, 0.0)
  assert splitDecimal(65536.5) == (65536.0, 0.5)
  assert splitDecimal(-65536.0) == (-65536.0, 0.0)
  assert splitDecimal(-65536.5) == (-65536.0, -0.5)

  assert splitDecimal(54.674'f32).intpart == 54'f32
  assert almostEqual(splitDecimal(54.674'f32).floatpart, 0.674'f32)
  assert splitDecimal(-693.4375'f32).intpart == -693'f32
  assert almostEqual(splitDecimal(-693.4375'f32).floatpart, -0.4375'f32)
  assert splitDecimal(0'f32) == (0'f32, 0'f32)
  assert splitDecimal(1'f32) == (1'f32, 0'f32)
  assert splitDecimal(0.1'f32) == (0'f32, 0.1'f32)
  assert splitDecimal(0.0625'f32) == (0'f32, 0.0625'f32)
  assert splitDecimal(0.9375'f32) == (0'f32, 0.9375'f32)
  assert splitDecimal(1.0625'f32) == (1'f32, 0.0625'f32)
  assert splitDecimal(1.9375'f32) == (1'f32, 0.9375'f32)
  assert splitDecimal(-1'f32) == (-1'f32, 0'f32)
  assert splitDecimal(-0.0625'f32) == (0'f32, -0.0625'f32)
  assert splitDecimal(-0.9375'f32) == (0'f32, -0.9375'f32)
  assert splitDecimal(-1.0625'f32) == (-1'f32, -0.0625'f32)
  assert splitDecimal(-1.9375'f32) == (-1'f32, -0.9375'f32)
  assert splitDecimal(65536'f32) == (65536'f32, 0'f32)
  assert splitDecimal(65536.5'f32) == (65536'f32, 0.5'f32)
  assert splitDecimal(-65536'f32) == (-65536'f32, 0'f32)
  assert splitDecimal(-65536.5'f32) == (-65536'f32, -0.5'f32)

block: # fac
  assert fac(0) == 1
  assert fac(1) == 1
  assert fac(2) == 2
  assert fac(3) == 6
  assert fac(4) == 24
  assert fac(5) == 120

block: # binom
  assert binom(0, 0) == 1
  assert binom(1, 0) == 1
  assert binom(1, 1) == 1
  assert binom(2, 1) == 2
  assert binom(3, 1) == 3
  assert binom(4, 1) == 4
  assert binom(5, 1) == 5
  assert binom(2, 2) == 1
  assert binom(3, 2) == 3
  assert binom(4, 2) == 6
  assert binom(5, 2) == 10
  assert binom(6, 2) == 15
  assert binom(3, 3) == 1
  assert binom(4, 3) == 4
  assert binom(5, 3) == 10
  assert binom(6, 3) == 20
  assert binom(7, 3) == 35
  assert binom(4, 4) == 1
  assert binom(5, 4) == 5
  assert binom(6, 4) == 15
  assert binom(7, 4) == 35
  assert binom(5, 5) == 1
  assert binom(6, 5) == 6
  assert binom(7, 5) == 21
  assert binom(43, 21) == 1052049481860
  assert binom(101, 100) == 101
  assert binom(9999, 2) == 49985001

block:  # gcd
  assert gcd(0, 0) == 0
  assert gcd(0, 1) == 1
  assert gcd(0, 0) == 0
  assert gcd(0, 1) == 1
  assert gcd(1, 0) == 1
  assert gcd(1, 1) == 1
  assert gcd(1, -1) == 1
  assert gcd(-1, 1) == 1
  assert gcd(-1, -1) == 1
  assert gcd(1, 2) == 1
  assert gcd(2, 1) == 1
  assert gcd(2, 2) == 2
  assert gcd(2, 3) == 1
  assert gcd(3, 2) == 1
  assert gcd(3, 3) == 3
  assert gcd(4, 2) == 2
  assert gcd(2, 4) == 2
  assert gcd(4, 4) == 4
  assert gcd(-4, 2) == 2
  assert gcd(-2, 4) == 2
  assert gcd(-4, 4) == 4
  assert gcd(-4, -2) == 2
  assert gcd(-2, -4) == 2
  assert gcd(-4, -4) == 4
  assert gcd(2048, 128) == 128
  assert gcd(12345, 67890) == 15
  assert gcd(-12345, 67890) == 15

  assert gcd(1.0, 1.0) == 1.0
  assert gcd(2.0, 1.0) == 1.0
  assert gcd(1.0, 2.0) == 1.0
  assert gcd(2.0, 2.0) == 2.0
  assert gcd(0.1, 0.1) == 0.1
  assert gcd(1.0, 0.375) == 0.125
  assert gcd(3.5, 5.5) == 0.5

  assert gcd(@[2, 6, 12]) == 2

block:  # lcm
  assert lcm(1, 1) == 1
  assert lcm(1, 2) == 2
  assert lcm(2, 1) == 2
  assert lcm(2, 2) == 2
  assert lcm(2, 3) == 6
  assert lcm(3, 2) == 6
  assert lcm(3, 3) == 3
  assert lcm(4, 2) == 4
  assert lcm(2, 4) == 4
  assert lcm(4, 4) == 4
  assert lcm(2048, 128) == 2048
  assert lcm(12345, 67890) == 55873470

  assert lcm([3, 5, 7]) == 105
  assert lcm([4, 6, 9, 12]) == 36
