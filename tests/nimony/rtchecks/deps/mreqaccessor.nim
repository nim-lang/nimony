import std/syncio

type
  I* = object
    x*: int
  J* = object
    h*: I

func id*(s: I): I = s
func lo*(s: I): uint8 = uint8(id(s).x)
proc g*(b: J) {.requires: lo(b.h) < 10'u8.} = echo "g got ", lo(b.h)
