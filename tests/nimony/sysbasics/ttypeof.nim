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

proc foo(): int = discard
iterator foo(): float = discard

assert typeof(foo(), typeOfProc) is int
assert typeof(foo(), typeOfIter) is float
const TypeofModeConst1 = typeOfProc
assert typeof(foo(), TypeofModeConst1) is int
const TypeofModeConst2 = typeOfIter
assert typeof(foo(), TypeofModeConst2) is float

proc foo(x: int): string = discard
iterator foo(x: int): char = discard

assert typeof(foo(0), typeOfProc) is string
assert typeof(foo(0), typeOfIter) is char
