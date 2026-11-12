# A constant shift whose result is the minimum of its signed type keeps its
# sign: the evaluator masks the two's complement pattern to the type's width,
# not the magnitude. It published `(-128'i8) shr 0` as `128'i8`, which the C
# back end wrapped back to -128 and `nimony n` printed as 128, and refused
# `low(int) shr 0` as an overflow.
import std/syncio

const
  a = (-128'i8) shr 0
  b = (-1'i8) shl 7
  c = (-3'i8) shl 7
  d = (-32768'i16) shr 0
  e = (-1'i16) shl 15
  f = low(int32) shr 0
  g = low(int) shr 0
  h = (-5'i16) shl 3
  i = not 127'i8

var s0 = 0
var s3 = 3
var s7 = 7
var s15 = 15
echo a, " ", (-128'i8) shr s0
echo b, " ", (-1'i8) shl s7
echo c, " ", (-3'i8) shl s7
echo d, " ", (-32768'i16) shr s0
echo e, " ", (-1'i16) shl s15
echo f, " ", low(int32) shr s0
echo g, " ", low(int) shr s0
echo h, " ", (-5'i16) shl s3
echo i, " ", not 127'i8
