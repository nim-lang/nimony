# planetis-m/nimony-bugs-fuzzers#59: arkham took the int -> float conversion of a
# uint64 read from memory (element, field through a pointer) as signed, so a value >= 2^63
# became negative.
import std/syncio

type O = object
  a: uint64

proc t(p0: uint64) =
  var arr: array[2, uint64] = [p0, p0 + 1'u64]
  var o = O(a: p0)
  let r = addr o
  var i = 1
  echo float64(arr[i]) == 9223372036854775808.0
  echo float64(r.a) == 9223372036854775808.0

t(0x8000000000000005'u64)
