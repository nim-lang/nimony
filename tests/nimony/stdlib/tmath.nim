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
