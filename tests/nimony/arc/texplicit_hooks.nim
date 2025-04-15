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