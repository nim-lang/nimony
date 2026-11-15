# Every label of a `case` on a narrow selector is a constant of the
# selector's type (#52).
import std/syncio
proc i8(x: int8) =
  case x
  of -128: echo "i8 min"
  else: echo "i8 other"
proc u8(x: uint8) =
  case x
  of 255: echo "u8 max"
  else: echo "u8 other"
proc i16(x: int16) =
  case x
  of -300: echo "i16 -300"
  else: echo "i16 other"
proc u32(x: uint32) =
  case x
  of 4294967295'u32: echo "u32 max"
  else: echo "u32 other"
i8(-128); u8(255); i16(-300); u32(4294967295'u32); i8(0)
