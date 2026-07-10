import std/[assertions, varints]

proc roundtrip(x: uint64) =
  var buf: array[maxVarIntLen, byte] = default(array[maxVarIntLen, byte])
  let n = writeVu64(buf, x)
  assert n >= 1 and n <= maxVarIntLen
  var got: uint64 = 0
  let m = readVu64(buf, got)
  assert m == n, "byte count mismatch"
  assert got == x, "value mismatch"

block: # boundary values across every varint width
  roundtrip(0'u64)
  roundtrip(240'u64)
  roundtrip(241'u64)
  roundtrip(2287'u64)
  roundtrip(2288'u64)
  roundtrip(67823'u64)
  roundtrip(67824'u64)
  roundtrip(16777215'u64)
  roundtrip(16777216'u64)
  roundtrip(0xffffffff'u64)
  roundtrip(0x1_0000_0000'u64)
  roundtrip(0xff_ffff_ffff'u64)
  roundtrip(0xffff_ffff_ffff'u64)
  roundtrip(0xff_ffff_ffff_ffff'u64)
  roundtrip(high(uint64))

block: # exact byte-length expectations
  var buf: array[maxVarIntLen, byte] = default(array[maxVarIntLen, byte])
  assert writeVu64(buf, 240'u64) == 1
  assert writeVu64(buf, 2287'u64) == 2
  assert writeVu64(buf, 67823'u64) == 3
  assert writeVu64(buf, 16777215'u64) == 4
  assert writeVu64(buf, 0xffffffff'u64) == 5
  assert writeVu64(buf, high(uint64)) == 9

block: # zigzag is a bijection (encode then decode restores the value)
  assert encodeZigzag(0) == 0'u64
  assert encodeZigzag(1) == 2'u64
  assert decodeZigzag(encodeZigzag(0)) == 0
  assert decodeZigzag(encodeZigzag(1)) == 1
  assert decodeZigzag(encodeZigzag(-1)) == -1
  assert decodeZigzag(encodeZigzag(-1234567)) == -1234567
  assert decodeZigzag(encodeZigzag(high(int64))) == high(int64)
  assert decodeZigzag(encodeZigzag(low(int64))) == low(int64)
