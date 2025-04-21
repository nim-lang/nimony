import std/assertions

proc foo =
  var x = block: "?"

  assert x == "?"

foo()