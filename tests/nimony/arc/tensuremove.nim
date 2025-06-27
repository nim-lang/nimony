import std/[syncio, assertions]

block:
  block:
    type
      JsonNode = ref object

    proc foo(d: JsonNode) =
      discard

    proc test_something()=
      var a = JsonNode()
      foo ensureMove(a)

    test_something()

  block:
    type
      JsonNode = object
        data: int

    proc foo(d: JsonNode) =
      discard

    proc test_something()=
      var a = JsonNode()
      foo ensureMove(a)

    test_something()

  block:
    type
      JsonNode = object
        data: int

    proc `=destroy`(x: JsonNode) = discard

    proc foo(d: JsonNode) =
      discard

    proc test_something()=
      var a = JsonNode()
      foo ensureMove(a)

    test_something()


# TODO: move it into the block below
type
  X = object
    s: string

block:
  proc `=copy`(x: var X, y: X) =
    x.s = "copied " & y.s

  proc `=sink`(x: var X, y: X) =
    `=destroy`(x)
    `=wasMoved`(x)
    x.s = "moved " & y.s

  proc consume(x: sink X) =
    discard x.s

  proc main =
    let m = "abcdefg"
    var x = X(s: ensureMove m)
    consume(ensureMove x)

  main()

block:
  type
    String = object
      id: string

  proc hello =
    var s = String(id: "1")
    var m = ensureMove s
    assert m.id == "1"

  hello()

block:
  type
    String = object
      id: string

  proc hello =
    var n = "1"
    var s = String(id: ensureMove n)
    var m = ensureMove s
    assert m.id == "1"

  hello()

block:
  type
    String = object
      id: string

  proc hello =
    var n = "1"
    var s = [ensureMove n]
    var m = ensureMove s
    assert m[0] == "1"

  hello()

block:
  type
    String = object
      id: string

  proc hello =
    var n = "1"
    var s = @[ensureMove n]
    var m = ensureMove s
    assert m[0] == "1"

  hello()

block:
  type
    String = object
      id: string

  proc hello =
    var s = String(id: "1")
    var m = ensureMove s.id
    assert m == "1"

  hello()

block:
  proc foo =
    var x = 1
    let y = ensureMove x # move

    assert y == 1

  foo()

block:
  proc foo =
    var x = 1
    let y = ensureMove x # move
    assert y == 1
  foo()

block:
  proc foo =
    var x = @[1, 2, 3]
    let y = ensureMove x[0] # move
    assert y == 1
    # when not defined(js):
    #   assert x == @[0, 2, 3]
  foo()

block:
  proc foo =
    var x = [1, 2, 3]
    let y = ensureMove x[0] # move
    assert y == 1
    # when not defined(js):
    #   assert x == @[0, 2, 3]
  foo()

block:
  proc foo =
    var x = @["1", "2", "3"]
    let y = ensureMove x[0] # move
    assert y == "1"
  foo()

