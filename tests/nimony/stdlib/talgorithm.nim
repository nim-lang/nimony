import std/[assertions, algorithm]

block:
  var d = ["boo", "fo", "barr", "qux"]
  proc myCmp(x, y: string): int =
    if x.len() > y.len() or x.len() == y.len(): 1
    else: -1
  sort(d, myCmp)
  assert d == ["fo", "qux", "boo", "barr"]

proc myCmp(x, y: int): int = cmp(x, y)

block:
  var d = [1, 11, 7, 3, 5]
  let d2 = sorted(d, myCmp)
  assert d2 == [1, 3, 5, 7, 11]

block:
  assert isSorted([1, 2, 3], myCmp)
  assert not isSorted([3, 2, 1], myCmp)
