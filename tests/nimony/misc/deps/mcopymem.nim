import std/typetraits

type
  Res* = object
    h: int
  Plain* = object
    h: int

proc `=destroy`*(r: Res) = discard
proc `=wasMoved`*(r: var Res) = r.h = 0

# `supportsCopyMem` called like a proc must still be folded by sem, in every
# position, not only where its argument happens to be checked a second time.
const resIsPlain* = supportsCopyMem(Res)

proc plainIsPlain*(): bool = supportsCopyMem(Plain)

proc viaWhen*(): string =
  when supportsCopyMem(Res): result = "copyMem"
  else: result = "hooks"
