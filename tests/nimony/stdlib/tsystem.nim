import std/[assertions]

proc testnotin(s: string): bool =
  for c in items s:
    if c notin {'A'..'Z', 'a'..'z'}:
      return false

  return true

assert testnotin "abc"
assert testnotin "AZaz"
assert not testnotin "1"
assert not testnotin "abc "

# generic min/max:
assert min("a", "b") == "a"
assert min("b", "a") == "a"
assert max("a", "b") == "b"
assert max("b", "a") == "b"

assert abs(0) == 0
assert abs(1) == 1
assert abs(-1) == 1
# TODO: Test overflow checks
#abs(int.low)

assert abs(0.0) == 0.0
assert abs(0.1) == 0.1
assert abs(-0.1) == 0.1


let guten = astToStr:
  let s = 12
  let m = 23

assert guten == """

let s: int = 12
let m: int = 23"""

assert not compiles(3 + "1" + 4)
assert compiles(3 + 4)

block:  # `==` for tuple|object
  var a = (1, true, "foo")
  var b = (1, true, "foo")
  var c = (1, true, "bar")

  assert a == b
  assert a != c

  type
    Foo = object
      x: int
      y: tuple[z: bool, w: string]

  var f = Foo(x: 1, y: (true, "foo"))
  var g = Foo(x: 1, y: (true, "foo"))
  var h = Foo(x: 1, y: (true, "bar"))

  assert f == g
  assert f != h

  type
    Base = ref object of RootObj
      x: int

    Bar = ref object of Base
      y: bool
      z: string

  var i = Bar(x: 1, y: true, z: "foo")
  var j = Bar(x: 1, y: true, z: "foo")
  var k = Bar(x: 2, y: true, z: "foo")
  var l = Bar(x: 1, y: false, z: "foo")

  assert i[] == j[]
  assert i[] != k[]
  assert i[] != l[]
