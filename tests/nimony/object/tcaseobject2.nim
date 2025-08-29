import std/[assertions, syncio]

block:
  type
    Foo = object
      case kind: bool
      of true:
        x: int
        m: float
      of false:
        y: int

  block:
    var a = default(Foo)
    assert a.x == 0
    assert a.m == 0.0

  block:
    var a = Foo()
    assert a.x == 0
    assert a.m == 0.0