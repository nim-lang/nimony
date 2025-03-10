import std/[hashes, syncio]

proc cmp[T: Hashable](x, y: T): bool =
  hash(x) == hash(y)

assert cmp(123, 123)
assert cmp('a', 'a')
assert cmp(true, true)
assert cmp(false, false)

type
  FooEnum = enum
    foo

assert cmp(foo, foo)
