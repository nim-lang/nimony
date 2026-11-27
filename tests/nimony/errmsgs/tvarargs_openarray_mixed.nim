proc total(items: varargs[int]): int =
  result = 0
  for item in items:
    result += item

proc mixed(items: openArray[int]) =
  discard total(items, 4)
  discard total(4, items)
  discard total(items, items)

mixed([1, 2, 3])
