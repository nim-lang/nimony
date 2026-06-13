import std/[assertions, algorithm]

proc myCmpStr(x, y: string): int =
  if x.len() > y.len() or x.len() == y.len(): 1 else: -1

proc myCmpInt(x, y: int): int = cmp(x, y)

block:
  var d = ["boo", "fo", "barr", "qux"]
  sort(d, myCmpStr)
  assert d == ["fo", "qux", "boo", "barr"]

  var di = [3, 1, 2, -1]
  sort(di, myCmpInt)
  assert di == [-1, 1, 2, 3]

block:
  var d = [1, 11, 7, 3, 5]
  let d2 = sorted(d, myCmpInt)
  assert d2 == [1, 3, 5, 7, 11]

  assert sorted(["boo", "fo", "barr", "qux"], myCmpStr) == ["fo", "qux", "boo", "barr"]

block:
  assert isSorted([1, 2, 3], myCmpInt)
  assert not isSorted([3, 2, 1], myCmpInt)
  assert isSorted(["a", "bc", "def", "ghij"], myCmpStr)
  assert not isSorted(["aaa", "bb", "c"], myCmpStr)
