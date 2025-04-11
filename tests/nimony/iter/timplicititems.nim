import std/assertions

iterator items(x: int): int =
  for i in 0 ..< x:
    yield i

var count = 0
for i in 5:
  assert i == count
  inc count
assert count == 5

type HasSelfItems = concept
  iterator `items`(x: Self): Self

proc foo[T: HasSelfItems](x: T): T =
  for i in x:
    result = i

assert foo(5) == 4
assert foo(7) == 6
