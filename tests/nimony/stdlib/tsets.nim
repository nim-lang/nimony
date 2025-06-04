import std/[assertions, hashes, sets]

block:
  var s = initHashSet[int]()
  assert not s.contains(1)
  assert not s.contains(2)
  s.incl 2
  assert not s.contains(1)
  assert s.contains(2)
  s.incl 3
  assert not s.contains(1)
  assert s.contains(2)
  assert s.contains(3)
  assert s.containsOrIncl(2)
  assert not s.containsOrIncl(5)
  assert s.containsOrIncl(5)
  s.excl 5
  assert not s.contains(5)
  assert not s.containsOrIncl(5)
  assert s.contains(5)

  var p = 1
  for i in items(s):
    p = p * i
  assert p == 30
