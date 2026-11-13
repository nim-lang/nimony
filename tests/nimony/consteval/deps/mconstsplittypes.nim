# Helper module of tconstcontainer.nim: the container type alone; its
# `len` and `[]` live in mconstsplitops.nim.
type
  Buf*[T] = object
    n*: int
    data*: ptr UncheckedArray[T]
