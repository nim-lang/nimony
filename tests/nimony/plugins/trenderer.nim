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
