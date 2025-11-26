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

block:
  var x = 'x'
  var value = 0

  case x
  of 'a':
    value = 1
  of {'c', 'b', 'g'..'z'}:
    value = 2
  else:
    value = 3

  assert value == 2

const toEscapedChars = {'\32'} + {'\\', '\'', '\"'}

block:
  var x = '\"'
  var branch = 0
  case x
  of toEscapedChars:
    branch = 1
  else:
    branch = 2
  assert branch == 1


proc hoo =
  var x = 'a'
  var branch = 0
  case x
  of toEscapedChars:
    branch = 1
  else:
    branch = 2
  assert branch == 2
hoo()
