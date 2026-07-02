import std/syncio

type
  TT = ref object
    x: int

proc main() =
  var a: nil TT = TT()
  if a != nil:
    a.x += 1
    var b = move(a)
    a.x += 1

main()
