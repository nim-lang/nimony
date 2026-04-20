import std / [syncio]

type
  SizeEnum {.size: sizeof(cint).} = enum
    a = 0
    b = 1 shl 0
    c = 1 shl 1

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
echo ord(c)
