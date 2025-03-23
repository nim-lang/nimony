# issue #829

iterator `....`*(a, b: int): int {.inline.} =
  var i = a
  while i <= b:
    yield i
    inc i

type
  Iterable* = concept
    iterator `....`(a, b: Self): Self

proc default2*[I: Iterable; T: HasDefault](x: typedesc[array[I, T]]): array[I, T] {.inline, noinit.} =
  # Needs to call iterator `....`(a, b: I): I
  for i in low(array[I, T]) .... high(array[I, T]):
    result[i] = default(T)

var x = default2(array[2, int])
