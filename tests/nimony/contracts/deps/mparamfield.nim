# issue #2944: an accessor-shaped routine read across a module boundary.
type Pair* = object
  a*, b*: int
proc total*(b: Pair): int = b.a + b.b

# issue #2994: an accessor with a `.requires`, whose body sem moves into a
# `\`body` routine the wrapper calls.
type
  Stamp* = distinct uint64
  Cur* = object
    ready: bool
    last: uint64
func `==`*(a, b: Stamp): bool = uint64(a) == uint64(b)
func initialized*(c: Cur): bool = c.ready
func lastStamp*(cursor: Cur): Stamp {.requires: cursor.ready.} =
  Stamp(cursor.last)
func initCur*(last: uint64): Cur = Cur(ready: true, last: last)
