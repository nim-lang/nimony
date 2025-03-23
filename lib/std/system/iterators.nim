type
  Iterable* = concept
    iterator `..`(a, b: Self): Self

iterator `..<`*(a, b: int): int {.inline.} =
  var i = a
  while i < b:
    yield i
    inc i

iterator `..`*(a, b: int): int {.inline.} =
  var i = a
  while i <= b:
    yield i
    inc i
