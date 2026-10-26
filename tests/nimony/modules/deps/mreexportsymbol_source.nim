type Conn* = ref object
  n: int

proc newConn*(): Conn = Conn(n: 7)
proc value*(c: Conn): int = c.n
