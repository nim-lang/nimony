import std/syncio

proc f(s: string): string =
  s

proc main =
  var a = f "abc"
  var b = "de"
  `=sink`(a, b)
  echo a

main()
