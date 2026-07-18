import std/[hashes, assertions]

proc compare[T: Hashable](x, y: T): bool =
  hash(x) == hash(y)

assert compare(123, 123)
assert compare('a', 'a')
assert compare(true, true)
assert compare(false, false)
assert compare("test", "test")

type
  FooEnum = enum
    foo

assert compare(foo, foo)

block seqHash:
  assert compare(@[1, 2, 3], @[1, 2, 3])
  assert compare(@["a", "b"], @["a", "b"])
  assert hash(@[1, 2, 3]) != hash(@[3, 2, 1])
  assert hash(newSeq[int]()) == hash(newSeq[int]())

block sameButDifferent:
  assert hashIgnoreCase("aA bb aAAa1234") == hashIgnoreCase("aa bb aaaa1234")
  assert hashIgnoreStyle("aa_bb_AAaa1234") == hashIgnoreCase("aaBBAAAa1234")
