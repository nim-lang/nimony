proc foo(x: seq[int]) = discard

foo(@[])

proc bar[T](x: T, y: seq[T]) = discard

bar(1.23, @[])

var s: seq[uint8] = @[]

# a `sink` parameter takes an empty literal like a plain one
proc sinkSeq(x: sink seq[int]) = discard
sinkSeq(@[])

proc sinkGeneric[T](x: sink seq[T], y: T) = discard
sinkGeneric(@[], 1)

proc sinkArray(x: sink array[0, int]) = discard
sinkArray([])

proc sinkSet(x: sink set[char]) = discard
sinkSet({})
