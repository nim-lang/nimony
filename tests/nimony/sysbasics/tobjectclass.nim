# The `object` type class matches object types only: `seq`, `string` and
# `openArray` are kinds of their own, even though `lib/std/system` declares
# them as objects.
import std/syncio

type
  Obj = object
    a: int
  IntSeq = seq[int]

proc kind[T: object](x: T): string = "object"
proc kind[T: tuple](x: T): string = "tuple"
proc kind[T](x: T): string = "other"

proc kindOpen(x: openArray[int]): string = kind(x)

echo kind(Obj(a: 1))
echo kind((1, 2))
echo kind(@[1])
echo kind("ab")
echo kind(IntSeq(@[1]))
echo kindOpen([1, 2])
