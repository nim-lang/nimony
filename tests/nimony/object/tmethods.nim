import std/[assertions, syncio]

type
  TestObj = object of RootObj
    t: int

  SubObject = object of TestObj

method test(t: ptr TestObj) {.base.} =
  echo "test called"

method test(t: ptr SubObject) =
  t.t = 5

block:
  var a: SubObject = SubObject()

  test(addr a)
  assert a.t == 5

method foo(t: var TestObj) {.base.} =
  echo "test called"

method foo(t: var SubObject) =
  t.t = 5

block:
  var a: SubObject = SubObject()

  foo(a)
  assert a.t == 5


type
    Base = ref object of RootObj
    Derived = ref object of Base

method inner(obj: Base) {.base.} =
    quit "to override"

method outer(obj: Base) {.base.} =
    echo "outer"
    obj.inner()

method inner(obj: Derived) =
    echo "inner Derived"

var x: Derived = Derived()
x.outer()
