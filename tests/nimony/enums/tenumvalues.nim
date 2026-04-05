import std / [syncio]

type
  SizeEnum {.size: sizeof(cint).} = enum
    a, b, c

  SmallEnum {.size: 1.} = enum
    p, q, r

  HoleyEnum = enum
    x = 0
    y = 3
    z = 5

echo sizeof(SizeEnum)
echo sizeof(SmallEnum)
echo ord(y)
echo ord(z)
echo $x
echo $z
