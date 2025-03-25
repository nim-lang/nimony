iterator `..<`*[T: Ordinal](a, b: T): T {.inline.} =
  var i = a
  while i < b:
    yield i
    inc i

iterator `..`*[T: Ordinal](a, b: T): T {.inline.} =
  var i = a
  while i <= b:
    yield i
    inc i
