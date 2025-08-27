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

block:
  type
    Bytes = distinct int

  var s = 22
  var m = Bytes(s)
  assert int(m) == 22

  proc check(s: Bytes) =
    discard

  check(m)


proc test(): int {.discardable.} =
  discard

block:
  test()
