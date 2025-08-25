import std/[assertions]

assert typeof(1) is int
assert typeof(0.1) is float
assert typeof(1 + 1) is int
assert typeof("a") is string
assert typeof("a"[0]) is char
assert typeof([0, 1]) is array[2, int]
assert typeof((1, "a")) is (int, string)

block:
  var x = 1
  assert typeof(x) is int
  var str = "foo"
  assert typeof(str) is string

  const X = 2
  assert typeof(X) is int
  const Str = "bar"
  assert typeof(Str) is string

block:
  type
    Foo = object
      x: int
      y: string

  let f = Foo()
  assert typeof(f) is Foo
  assert typeof(f.x) is int
  assert typeof(f.y) is string
