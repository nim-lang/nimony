import std/[hashes, assertions, tables]

block:
  var t = initTable[int, int]()
  assert not t.contains(123)
  assert not t.hasKey(123)
  assert t.getOrDefault(123) == 0
  assert t.len == 0
  t[123] = 321
  try:
    assert t.len == 1
    assert t.contains(123)
    assert t.hasKey(123)
    assert t.getOrDefault(123) == 321
    assert t[123] == 321
    assert t.mgetOrPut(123, -1) == 321
    inc t[123]
    assert t[123] == 322
    assert t.mgetOrPut(456, 654) == 654
    assert t.len == 2
    assert t.contains(456)
    assert t.hasKey(456)
    assert t.getOrDefault(456) == 654
    assert t[456] == 654
  except:
    assert false


  let HC123 = (1 shl 31) + 123  # Hash collision to '123'
  assert not t.contains(HC123)
  assert t.getOrDefault(HC123) == 0

block:
  const Max = 1000

  var t = initTable[int, int]()
  for i in 1 ..< Max:
    t[i] = -i
  assert t.len == (Max - 1)
  for i in 1 ..< Max:
    try:
      assert t[i] == -i
    except:
      assert false

  for k, v in t.pairs:
    assert k == -v
    assert t.contains(k)
    assert not t.contains(-k)
    assert t.getOrDefault(k) == v
    assert t.mgetOrPut(k, 0) == v

  assert t.len == (Max - 1)
  assert not t.contains(0)

  for k, v in t.mpairs:
    v = -v
    assert k == v

  for k, v in t.pairs:
    assert k == v
