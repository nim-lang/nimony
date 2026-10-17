import std/assertions

# Integer -> decimal (`system/formatint`, a port of Rust's `core::fmt::num`).

proc viaBuffer(x: int64): string =
  var buf = default(NumBuffer)
  result = ""
  for c in buf.formatInto(x): result.add c

proc viaBuffer(x: uint64): string =
  var buf = default(NumBuffer)
  result = ""
  for c in buf.formatInto(x): result.add c

proc naive(x: uint64): string =
  # One digit at a time, as a reference.
  result = ""
  var y = x
  while true:
    result = char(int(y mod 10'u64) + ord('0')) & result
    y = y div 10'u64
    if y == 0'u64: break

block: # every digit count and the boundaries around it
  var p = 1'u64
  for digits in 1 .. 20:
    for x in [p - 1'u64, p, p + 1'u64, p * 2'u64, p * 9'u64]:
      assert $x == naive(x)
      assert viaBuffer(x) == naive(x)
    if digits < 20: p = p * 10'u64

block: # unsigned extremes
  assert $0'u64 == "0"
  assert $9'u64 == "9"
  assert $10'u64 == "10"
  assert $99'u64 == "99"
  assert $100'u64 == "100"
  assert $999'u64 == "999"
  assert $1000'u64 == "1000"
  assert $10000'u64 == "10000"
  assert $high(uint64) == "18446744073709551615"
  assert viaBuffer(high(uint64)) == "18446744073709551615"

block: # signed
  assert $0 == "0"
  assert $(-1) == "-1"
  assert $(-9) == "-9"
  assert $(-10) == "-10"
  assert $(-12345) == "-12345"
  assert $high(int64) == "9223372036854775807"
  assert $low(int64) == "-9223372036854775808"
  assert viaBuffer(low(int64)) == "-9223372036854775808"
  assert viaBuffer(-7'i64) == "-7"
  assert $(-2147483648'i32) == "-2147483648"
  assert $high(int32) == "2147483647"

block: # addInt appends without disturbing the prefix, short and long strings
  var s = "x="
  s.addInt(-42'i64)
  assert s == "x=-42"
  s.addInt(18446744073709551615'u64)
  assert s == "x=-4218446744073709551615"
  s.addInt(0'i64)
  assert s == "x=-42184467440737095516150"
  var t = ""
  for i in 0 .. 1000: t.addInt(int64(i))
  assert t.len == 10 + 90 * 2 + 900 * 3 + 4
  assert t[t.len - 4 .. t.len - 1] == "1000"

block: # several numbers through one buffer
  var buf = default(NumBuffer)
  var s = ""
  for x in [5'u64, 123456789'u64, 0'u64]:
    for c in buf.formatInto(x): s.add c
    s.add ','
  assert s == "5,123456789,0,"
