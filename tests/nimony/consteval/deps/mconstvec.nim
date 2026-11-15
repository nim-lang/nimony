# Helper module of tconstcontainer.nim: a container with a
# `ptr UncheckedArray` payload whose `len` and `[]` live here.
type
  Vec*[T] = object
    n: int
    data: ptr UncheckedArray[T]

proc len*[T](b: Vec[T]): int = b.n
proc `[]`*[T](b: Vec[T]; i: int): T = b.data[i]

proc mkv*(): Vec[int] =
  let p = cast[ptr UncheckedArray[int]](alloc(3 * sizeof(int)))
  p[0] = 7; p[1] = 8; p[2] = 9
  Vec[int](n: 3, data: p)
