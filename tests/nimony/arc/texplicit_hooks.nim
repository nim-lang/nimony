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

block:
  type
    Object = object
      id: int

    myseq = object
      f: Object

  var
    x: myseq = myseq(f: Object(id: 12))

  `=wasMoved`(x.f)
  assert x.f.id == 0

block:
  type
    myseq = object
      f: int

  var
    x: myseq = myseq(f: 12)

  `=wasMoved`(x.f)
  assert x.f == 0

block:
  type
    myseq = object
      f: float

  var
    x: myseq = myseq(f: 1.2)

  `=wasMoved`(x.f)
  assert x.f == 0.0

block:
  type
    myseq = object
      f: array[3, int]

  var
    x: myseq = myseq(f: [1, 2, 3])

  `=wasMoved`(x.f)
  assert x.f == [0, 0, 0]

block:
  type
    myseq = object
      f: (int, float, bool, char)

  var
    x: myseq = myseq(f: (1, 6.6, true, 'a'))

  `=wasMoved`(x.f)
  assert x.f[0] == 0
  assert x.f[1] == 0.0
  assert x.f[2] == false
  assert x.f[3] == '\0'
