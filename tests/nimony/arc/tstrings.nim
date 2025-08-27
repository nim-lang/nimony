import std/[syncio, assertions]

const
  x = "abc"

const s: cstring = "135"

proc foo =
  var m = "12432"
  var v: string = x

  var n = "123" & x & m
  echo v
  echo n

  var m1 = s

foo()

proc myStr(a: string): string =
  ## strips the last two chars, see test below
  a

let m = myStr"AnB"
echo m


proc cho(x: string): string =
  result = x

proc bar(): string =
  var x = "1212334"
  x.add "123"
  return cho(x)

assert bar() == "1212334123"

type
  SafeCString = object
    raw: string
    data: int

proc foo2(): SafeCString =
  result = SafeCString(raw: "123", data: 1)
  result.raw.add "4"


proc bar2(): string =
  # var s = foo().raw
  # let s = foo1()
  return foo2().raw
  # cho(foo())

assert bar2() == "1234"
