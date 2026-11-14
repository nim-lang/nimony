# issue #2994: an accessor with a `.requires`, whose body sem moves into
# a `\`body` routine the wrapper calls.
type
  Ordinal* = distinct uint64
  Cursor* = object
    ready: bool
    last: uint64

func `==`*(a, b: Ordinal): bool = uint64(a) == uint64(b)
func initialized*(c: Cursor): bool = c.ready
func lastOrdinal*(cursor: Cursor): Ordinal {.requires: cursor.ready.} =
  Ordinal(cursor.last)
func initCursor*(last: uint64): Cursor = Cursor(ready: true, last: last)
