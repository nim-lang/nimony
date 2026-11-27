# planetis-m/nimony-bugs-fuzzers#60: arkham widened a negative signed value
# converted to a wider unsigned type below 64 bits by sign extension only, so
# `uint16(not x)` kept the upper 48 bits set.
import std/syncio

proc t(x: int8; y: int16) =
  echo uint16(not x)
  echo uint32(y)
  var u: uint16 = uint16(not x)
  echo uint32(u) + 1'u32

for k in 0..0: t(1'i8 + int8(k), -3'i16 + int16(k))
