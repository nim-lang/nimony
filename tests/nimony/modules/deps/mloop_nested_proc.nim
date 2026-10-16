iterator two*(): int =
  yield 1
  yield 2

proc runA*(): int =
  result = 0
  for x in two():
    proc inner(): int = 10
    result += inner()
