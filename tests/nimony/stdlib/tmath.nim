import std/[math, assertions]

let
  FLT_MIN {.importc: "FLT_MIN", header: "<float.h>".}: float32
  DBL_MIN {.importc: "DBL_MIN", header: "<float.h>".}: float64

block:  #signbit
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
