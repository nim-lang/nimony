import std/[syncio, assertions]

proc classify(s: string) =
  case s[0]
  of '_': echo "an identifier"
  of '0', '9': echo "a number"
  else: discard

classify("9123345")

proc classify1(s: string) {.requires: s.len > 0.} =
  case s[0]
  of '_': echo "an identifier"
  of '0' .. '9': echo "a number"
  else: discard

classify1("9123345")

block:
  proc foo(x: int): string =
    case x
    of 1: "digit"
    else: "number"


  var r = foo(10)
  assert r == "number"

block:
  proc main(a: uint32) =
    case a
    of 777u32: echo 12
    else:
      discard

  main(10)
