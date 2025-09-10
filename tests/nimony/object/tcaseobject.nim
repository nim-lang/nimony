import std/[assertions, syncio]


type Foo = object
  case x: bool
  of false:
    y: int
  of true:
    z: string
    t: bool

type Bar = object
  case x: bool
  of false, true: discard

var foo = Foo()
foo = Foo(x: false, y: 123)
let a: bool = foo.x
assert a == false
let b: int = foo.y
assert b == 123
foo = Foo(x: true, z: "abc", t: false)
assert foo.x == true
let c: string = foo.z
assert c == "abc"
let d: bool = foo.t
assert d == false

var bar = Bar()
bar = Bar(x: false)
assert bar.x == false
bar = Bar(x: true)
let e: bool = bar.x
assert e == true

var bar2 = bar
assert bar2.x == true

proc fx(x: Foo): Foo =
  let s = x
  return s

block:
  var x = Foo(x: true, z: "abc", t: false)
  discard fx(x)

block:
  type
    TKind = enum ka, kb, kc
    TA = object
      case k: TKind
      of ka:
        x: int
        y: int
      of kb: a, b: string
      else: c, d: float

  var s = ka
  case s
  of ka:
    var x, y = 1
  of kb:
    var a, b = ""
  of kc: echo 3

