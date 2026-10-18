import std/assertions

proc test =
  const myPair = (a: 10, b: 20)
  assert myPair[0] == 10
  assert myPair.b == 20

test()

# a const holding `low(int64)` crashed the compile-time evaluator (#2634)
const int64Extremes = [low(int64), -1'i64, 0'i64, high(int64)]
assert int64Extremes[0] == -9223372036854775807'i64 - 1
assert int64Extremes[3] == 9223372036854775807'i64
const lowInt64 = low(int64)
assert -(lowInt64 + 1) == high(int64)
