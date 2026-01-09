import std/[syncio, strutils]

proc hello() {.noreturn.} =
  discard

proc foo =
  var x: int
  if true:
    x = 12
  else:
    hello()
  echo x

foo()
