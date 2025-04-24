import std/assertions

proc foo =
  var x = block: "?"

  assert x == "?"

foo()

proc main =
  block endLess:
    let s = 12
    break

  block endLess:
    let s = 12
    break endLess

main()
