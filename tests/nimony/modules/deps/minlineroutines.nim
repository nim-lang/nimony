iterator bar(): int =
  yield 1
  yield 2

proc foo*(x: int) {.inline.} =
  for i in bar():
    let m = i

# foo(2)
