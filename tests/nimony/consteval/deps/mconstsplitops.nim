# Helper module of tconstcontainer.nim: `len` and `[]` of a container
# declared in mconstsplittypes.nim.
import mconstsplittypes
export mconstsplittypes

proc len*[T](b: Buf[T]): int = b.n
proc `[]`*[T](b: Buf[T]; i: int): T = b.data[i]

proc mkb*(): Buf[int] =
  let p = cast[ptr UncheckedArray[int]](alloc(3 * sizeof(int)))
  p[0] = 4; p[1] = 5; p[2] = 6
  Buf[int](n: 3, data: p)
