import std/[syncio, assertions]


type
  Foo = object
    case kind: bool
    of true:
      x: int
    of false:
      y: int


var f = Foo(x: 1, y: 1)