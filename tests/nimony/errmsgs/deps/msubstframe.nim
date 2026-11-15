type
  I* = object
    x*: int
  J* = object
    h*: I

func base(s: uint8): uint8 = s
func nxt*(s: uint8): uint8 = base(s) + 1'u8
func nxt2*(s: uint8): uint8 = base(base(s)) + 1'u8
proc g*(a: I; b: J) {.requires: a.x < 10.} = discard
func lo*(s: I): uint8 = uint8(s.x)
proc gl*(b: J) {.requires: lo(b.h) < 10'u8.} = discard
func pred*(s: I): int = s.x - 1
proc gp*(a: J; i: int) {.requires: i < pred(a.h).} = discard
