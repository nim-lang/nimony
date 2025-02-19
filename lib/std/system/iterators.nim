iterator `..<`*(a, b: int): int {.inline.} =
  var i = a
  while i < b:
    yield i
    inc i
