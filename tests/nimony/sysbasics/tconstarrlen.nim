const Len = 100
var a: array[Len, int]

type
  FooArray = array[Len, int]

var b: FooArray
if b.len == Len:
  discard
