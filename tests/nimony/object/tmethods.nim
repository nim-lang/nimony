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