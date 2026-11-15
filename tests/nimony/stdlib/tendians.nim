import std/[assertions, endians, syncio]

var src = [1'u8, 2, 3, 4, 5, 6, 7, 8]

var swapped8: array[8, uint8]
swapEndian64(addr swapped8, addr src)
assert swapped8 == [8'u8, 7, 6, 5, 4, 3, 2, 1]

var swapped4: array[4, uint8]
swapEndian32(addr swapped4, addr src)
assert swapped4 == [4'u8, 3, 2, 1]

var swapped2: array[2, uint8]
swapEndian16(addr swapped2, addr src)
assert swapped2 == [2'u8, 1]

var le8: array[8, uint8]
var be8: array[8, uint8]
littleEndian64(addr le8, addr src)
bigEndian64(addr be8, addr src)

var le4: array[4, uint8]
var be4: array[4, uint8]
littleEndian32(addr le4, addr src)
bigEndian32(addr be4, addr src)

var le2: array[2, uint8]
var be2: array[2, uint8]
littleEndian16(addr le2, addr src)
bigEndian16(addr be2, addr src)

when defined(bigEndian):
  assert le8 == [8'u8, 7, 6, 5, 4, 3, 2, 1]
  assert be8 == src
  assert le4 == [4'u8, 3, 2, 1]
  assert be4 == [1'u8, 2, 3, 4]
  assert le2 == [2'u8, 1]
  assert be2 == [1'u8, 2]
else:
  assert le8 == src
  assert be8 == [8'u8, 7, 6, 5, 4, 3, 2, 1]
  assert le4 == [1'u8, 2, 3, 4]
  assert be4 == [4'u8, 3, 2, 1]
  assert le2 == [1'u8, 2]
  assert be2 == [2'u8, 1]

echo "endians: ok"
