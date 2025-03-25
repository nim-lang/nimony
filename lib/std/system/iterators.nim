type
  Countable = concept
    proc `+`(x, y: Self): Self # Incable
    proc `<`(x, y: Self): bool
    proc `<=`(x, y: Self): bool

iterator `..<`*[T: Countable](a, b: T): T {.inline.} =
  var i = a
  while i < b:
    yield i
    inc i

iterator `..`*[T: Countable](a, b: T): T {.inline.} =
  var i = a
  while i <= b:
    yield i
    inc i
