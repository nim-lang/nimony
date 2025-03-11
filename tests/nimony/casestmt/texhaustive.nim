
import std / syncio

type
  MyEnum = enum
    a, b, c

  HoleyEnum = enum
    Ha = 2, Hb = 4, Hc = 10


proc dispatch(e: MyEnum) =
  case e
  of a:
    echo "a"
  of c:
    echo "c"

proc ok(e: HoleyEnum) =
  case e
  of Ha:
    echo "Ha"
  of Hb:
    echo "Hb"
  of Hc:
    echo "Hc"

proc missing(e: HoleyEnum) =
  case e
  of Ha:
    echo "Ha"
  of Hb:
    echo "Hb"

dispatch(a)
dispatch(b)
dispatch(c)
