proc foo(x: seq[int]) = discard

foo(@[])

proc bar[T](x: T, y: seq[T]) = discard

bar(1.23, @[])

var s: seq[uint8] = @[]
