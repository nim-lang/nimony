# An integer operation yields a value of its type whatever its operands, since
# it wraps (doc/language.md). So `x shr 56` of a `uint64` is in `0..255` also
# when `x` is an inline `+` or `*`, whose mathematical interval the prover
# cannot hold, or an operand it cannot bound at all.

{.feature: "staticContracts".}

import std/syncio

proc product(x: uint64; a: array[256, int]): int =
  result = a[int((x * 3'u64) shr 56)]

proc sum(x: uint64; a: array[256, int]): int =
  result = a[int((x + 3'u64) shr 56)]

proc nested(x, y: uint64; a: array[256, int]): int =
  result = a[int((x * y + 7'u64) shr 56)]

proc shifted(x: uint64; a: array[16, int]): int =
  result = a[int((x shl 3) shr 60)]

proc complement(x: uint64; a: array[256, int]): int =
  result = a[int((not x) shr 56)]

proc quotient(x: uint64; a: array[256, int]): int =
  result = a[int((x * 3'u64) div 72057594037927936'u64)]

proc remainder(x: uint64; a: array[10, int]): int =
  result = a[int((x * 3'u64) mod 10'u64)]

proc narrow(x: uint32; a: array[256, int]): int =
  result = a[int((x * x + 1'u32) shr 24)]

proc generic[T](x: uint64; a: array[256, T]): T =
  result = a[int((x * 3'u64) shr 56)]

template viaTemplate(x: uint64; a: array[256, int]): int =
  a[int((x * 5'u64) shr 56)]

proc useTemplate(x: uint64; a: array[256, int]): int =
  result = viaTemplate(x, a)

proc negatedThenConverted(e: int32; a: array[10, int]): int =
  # `-e` is some `int32`, and `k` a `uint32` whatever that was: `k < 10` is
  # `0..9`
  result = 0
  let k = uint32(if e < 0: -e else: e)
  if k < 10:
    result = a[int(k)]

proc sumThenConverted(x, y: int32): char =
  # `uint32(t)` is not `t` (`-1` becomes `high(uint32)`), so `k` keeps no
  # fact of `t`'s, and `k < 10` is `0..9`
  result = '-'
  let t = x + y
  let k = uint32(t)
  if k < 10:
    result = chr(ord('0') + int(k))

proc widened(x: int32; y: uint8; a: array[10, int]): int =
  # a conversion to a type holding every value of the operand's keeps it: the
  # facts about `x` and `y` are facts about `k` and `m`
  result = 0
  let k = int64(x)
  let m = uint32(y)
  if x >= 0 and x <= 9 and y < 10:
    result = a[k] + a[m]

type Small = enum s0, s1, s2, s3, s4, s5

proc smallEnum(e: Small; a: array[10, int]): int =
  # every ordinal of `Small` is a `uint8`, so `k` is `e`
  result = 0
  let k = uint8(e)
  if e <= s5:
    result = a[k]

var a: array[256, int]
for i in 0 ..< 256: a[i] = i
var s: array[16, int]
for i in 0 ..< 16: s[i] = i
var d: array[10, int]
for i in 0 ..< 10: d[i] = i
let big = 0xFFFF_FFFF_FFFF_FFFF'u64
echo product(big, a), " ", product(1'u64 shl 60, a)
echo sum(big, a), " ", sum(0x7F00_0000_0000_0000'u64, a)
echo nested(1'u64 shl 32, 1'u64 shl 31, a)
echo shifted(1'u64 shl 60, s), " ", shifted(1'u64 shl 59, s)
echo complement(0'u64, a), " ", complement(big, a)
echo quotient(big, a)
echo remainder(big, d), " ", remainder(4'u64, d)
echo narrow(0x1000'u32, a), " ", narrow(0xFFFF'u32, a)
var b: array[256, uint8]
b[255] = 7'u8
echo generic(big, a), " ", generic(big, b)
echo useTemplate(1'u64 shl 58, a)
echo negatedThenConverted(-7'i32, d), " ", negatedThenConverted(12'i32, d)
echo sumThenConverted(2'i32, 3'i32), " ", sumThenConverted(-2'i32, 1'i32)
echo widened(3'i32, 4'u8, d), " ", widened(-1'i32, 4'u8, d)
echo smallEnum(s4, d)
