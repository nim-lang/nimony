import std/assertions

proc f(s: string): string = s

proc main =
  var a = "abc"
  var b = "de"
  `=copy`(a, b)
  a = `=dup`(b)
  assert a == b
  var c = f "edg"
  `=sink`(c, a)
  assert a == "de"
  `=wasMoved`(a)

main()


block:
  type
    TestObj = object
      id: string

    myseq = object
      f: TestObj

  var
    x: myseq = myseq()
  x.f = TestObj(id: "12")

  assert x.f.id == "12"

  `=wasMoved`(x.f)
  assert x.f.id == ""

type
  Foo = object
proc `=destroy`(x: var Foo) = discard
proc `=dup`(x: Foo): Foo = x
