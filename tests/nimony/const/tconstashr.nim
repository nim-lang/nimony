# A signed `shr` is an arithmetic shift: it rounds toward negative infinity,
# so `(-1) shr 1` is `-1`, not `0`. Each constant must equal the value the
# same shift computes at run time (planetis-m/nimony-bugs-fuzzers#16).
import std/syncio

const
  a = (-1) shr 1
  b = (-2) shr 2
  c = (-7) shr 1
  d = ashr(-8'i16, 2)
  e = (-9'i16) shr 2
  f = (-5'i8) shr 1
  g = (-128'i8) shr 7
  h = (-1) shr 63
  i = low(int) shr 1
  j = (-6'i32) shr 1
  k = 7 shr 1
  l = 200'u8 shr 3
  m = c shr 1         # a folded negative, shifted again

var s1 = 1
var s2 = 2
var s3 = 3
var s7 = 7
var s63 = 63
echo a, " ", (-1) shr s1
echo b, " ", (-2) shr s2
echo c, " ", (-7) shr s1
echo d, " ", ashr(-8'i16, s2)
echo e, " ", (-9'i16) shr s2
echo f, " ", (-5'i8) shr s1
echo g, " ", (-128'i8) shr s7
echo h, " ", (-1) shr s63
echo i, " ", low(int) shr s1
echo j, " ", (-6'i32) shr s1
echo k, " ", 7 shr s1
echo l, " ", 200'u8 shr s3
echo m, " ", ((-7) shr s1) shr s1
