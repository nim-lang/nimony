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


for i in 1..2: # works
  break

block: # works
  for i in 1..2:
    break

block: # works
  block:
    discard 12 + 3
  for i in 1..2:
    break

block named: # works
  if true:
    break named
