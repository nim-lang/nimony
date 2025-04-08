import std/[math, assertions]

assert not signbit(0.0)
assert signbit(0.0 * -1.0)
assert signbit(-0.1)
assert not signbit(0.1)

assert classify(1.0) == fcNormal
assert classify(-1.0) == fcNormal
assert classify(0.0) == fcZero
assert classify(0.0 * -1.0) == fcNegZero
assert classify(0.0 / 0.0) == fcNan
assert classify(1.0 / 0.0) == fcInf
assert classify(-1.0 / 0.0) == fcNegInf
