import std/[math, assertions]

let
  FLT_MIN {.importc: "FLT_MIN", header: "<float.h>".}: float32
  DBL_MIN {.importc: "DBL_MIN", header: "<float.h>".}: float64

assert not signbit(0.0)
assert signbit(0.0 * -1.0)
assert signbit(-0.1)
assert not signbit(0.1)
assert signbit(-1.0'f)

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
