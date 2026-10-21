# recursive inline iterators are rejected
iterator foo(n: int): int =
  if n > 0:
    for x in foo(n-1): yield x
  yield n
for x in foo(3): discard x
