import std/assertions

type
  Outer = object
    case a: bool
    of true:
      case b: bool
      of true: inner: int
      of false: nil
    of false:
      other: string

var o1: Outer
var o2 = default(Outer)
var o3 = Outer(a: true, b: true, inner: 5)
var o4 = Outer(a: false, other: "x")

assert o3.inner == 5
assert o4.other == "x"
