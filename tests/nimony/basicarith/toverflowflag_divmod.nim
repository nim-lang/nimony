import std/syncio

# `div` and `mod` inside `{.keepOverflowFlag.}` compile and compute their
# values for every integer width. Whether they set `overflowFlag()` is an open
# question, so this test never reads the flag after one.

proc d64(a, b: int64): int64 =
  {.keepOverflowFlag.}:
    result = a div b

proc m64(a, b: int64): int64 =
  {.keepOverflowFlag.}:
    result = a mod b

proc dmInt(a, b: int): int =
  {.keepOverflowFlag.}:
    result = a div b * 10 + a mod b

proc d32(a, b: int32): int32 =
  {.keepOverflowFlag.}:
    result = a div b

proc m8(a, b: int8): int8 =
  {.keepOverflowFlag.}:
    result = a mod b

proc dmU64(a, b: uint64): uint64 =
  {.keepOverflowFlag.}:
    result = a div b + a mod b

proc dU8(a, b: uint8): uint8 =
  {.keepOverflowFlag.}:
    result = a div b

proc dGen[T](a, b: T): T {.untyped.} =
  {.keepOverflowFlag.}:
    result = a div b

template mTmpl(a, b: int): int =
  var r = 0
  {.keepOverflowFlag.}:
    r = a mod b
  r

echo d64(7, 2), " ", m64(7, 2), " ", d64(-7, 2), " ", m64(-7, 2)
echo dmInt(17, 5)
echo d32(7'i32, 2'i32), " ", m8(-7'i8, 3'i8)
echo dmU64(7'u64, 2'u64), " ", dU8(200'u8, 3'u8)
echo dGen(9, 4), " ", dGen(9'i32, 4'i32), " ", mTmpl(9, 4)
