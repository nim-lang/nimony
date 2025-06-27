import std / syncio


template renderTree(s: typed) {.plugin: "deps/mrender".}

renderTree:
  let x = 1
  var y = 12

  proc foo(x: int): int =
    var s = x
    s = 12
    result = s

  discard foo(2)

  var s = @[1, 2, 3]
  echo s[0]

  type
    Foo = object
      id: int

  proc `=destroy`(x: Foo) =
    discard

  block:
    var m = Foo(id: 12)
    `=destroy`(m)
