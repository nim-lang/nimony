import "mcyc3_dns" {.cyclic.}

type
  Reader* = object
    pos: int
  Writer* = object
    pos: int

proc cProc*(): int = aVal()
