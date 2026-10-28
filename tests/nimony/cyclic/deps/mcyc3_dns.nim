import "mcyc3_chan" {.cyclic.}

type Msg* = object
  x*: int

proc aVal*(): int = 1

proc use*(): bool =
  let s = Sender[Msg]()
  s.send(Msg(x: 1))
