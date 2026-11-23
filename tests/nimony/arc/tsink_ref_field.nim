# A field read through a `sink` ref parameter, typed by a later pass: the
# operand of a conversion, of a subtype-to-base conversion and of a tuple
# subscript (nimony-bugs-llms#346).
import std/syncio

type
  Base = ref object of RootObj
    v: int
  Sub = ref object of Base
  Holder = ref object
    item: Sub
    height: float
    pos: (float, float)

proc use(b: Base): int = b.v

proc conv(h: sink Holder): float32 = float32(h.height)
proc toBase(h: sink Holder): int = use(h.item)
proc tupat(h: sink Holder): float = h.pos[0] + h.pos[1]
proc plain(h: sink Holder): float = h.height

let h = Holder(item: Sub(v: 7), height: 1.5, pos: (2.0, 3.0))
echo conv(h), " ", toBase(h), " ", tupat(h), " ", plain(h)
