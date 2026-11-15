import std/syncio

# A value parameter bound from the length of an array that is NESTED inside
# another type (issue #2730): the formal's index slot is the canonical
# `0 .. N-1` and must be matched by length, not structurally.

type Obj[T] = object
  x: T

proc viaTypedesc[N: static int](x: typedesc[array[N, int32]]): int = N
proc viaPtr[N: static int](x: ptr array[N, int]): int = N
proc viaSeq[N: static int](x: seq[array[N, int]]): int = N
proc viaObj[N: static int](x: Obj[array[N, int]]): int = N
proc viaTuple[N: static int](x: (array[N, int], int)): int = N
proc viaNested[N, M: static int](x: ptr array[N, array[M, int]]): int = N * 10 + M
proc twice[N: static int](x: ptr array[N, int]; y: ptr array[N, int]): int = N

var a: array[3, int] = [1, 2, 3]
var b: array[3, int] = [4, 5, 6]
var c: array[2, int] = [7, 8]
var m: array[2, array[4, int]]

echo viaTypedesc(array[4, int32])
echo viaPtr(addr a)
echo viaSeq(@[[1, 2]])
echo viaObj(Obj[array[5, int]]())
echo viaTuple(([1], 42))
echo viaNested(addr m)
echo twice(addr a, addr b)
echo compiles(twice(addr a, addr c))
