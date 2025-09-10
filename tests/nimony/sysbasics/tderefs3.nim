
import std / [syncio]

type
  Table[K, V] = object
    data: seq[(K, V)]

iterator pairs*[K, V](t: Table[K, V]): (K, V) =
  # Test with (K, V) pairs
  for i in 0 ..< t.data.len:
    yield (t.data[i][0], t.data[i][1])

iterator lpairs*[K, V](t: Table[K, V]): (lent K, lent V) =
  # Test with (lent K, lent V) pairs
  for i in 0 ..< t.data.len:
    yield (t.data[i][0], t.data[i][1])

iterator mpairs*[K, V](t: Table[K, V]): (var K, var V) =
  # Test with (var K, var V) pairs
  for i in 0 ..< t.data.len:
    yield (t.data[i][0], t.data[i][1])

iterator mixedPairs*[K, V](t: Table[K, V]): (K, var V) =
  # Test with (K, var V) pairs
  for i in 0 ..< t.data.len:
    yield (t.data[i][0], t.data[i][1])

proc add*[K, V](t: var Table[K, V]; k: sink K; v: sink V) =
  t.data.add((k, v))

proc initTable*[K, V](): Table[K, V] =
  Table[K, V](data: @[])

var t = initTable[int, int]()
t.add(1, 2)
t.add(3, 4)
echo "---- test 1 ----"
for k, v in t.pairs:
  echo k, v

echo "---- test 3 ----"
for k, v in t.mpairs:
  echo k, v

echo "---- test 2 ----"
for k, v in t.lpairs:
  echo k, v

echo "---- test 4 ----"
for k, v in t.mixedPairs:
  echo k, v

block:
  iterator iter(s: seq[int]): int =
    for i in 0 ..< s.len:
      yield s[i]

  var s = newSeq[int](1)

  for i in iter(s):
    echo i

block:
  iterator iter(s: seq[int]): lent int =
    for i in 0 ..< s.len:
      yield s[i]

  var s = newSeq[int](1)

  for i in iter(s):
    echo i

block:
  iterator iter(s: var seq[int]): var int =
    for i in 0 ..< s.len:
      yield s[i]

  var s = newSeq[int](1)

  for i in iter(s):
    echo i
