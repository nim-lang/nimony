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
