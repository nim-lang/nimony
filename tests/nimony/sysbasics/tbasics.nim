import std/[syncio, assertions]

type
  Array = array[5, int]

var s = [1, 2, 3]
var s2: Array
var s3: Array = [1, 2, 3, 4, 5]

proc foo =
  var x: array[3, int] = [5, 6, 7]
  var m = [5, 6, 7, 8]

foo()

proc foo2(m: int) =
  var x = 1
  var s = (x, 1, 2)
  let m1 = (m, 6)
  const y = 12.3
  let z1 = (y, s, m1)

foo2(12)

proc foo3(x: pointer) =
  let m = x
  let s2 = cast[ptr int](x)
  let s3 = s2[]

block:
  var s = 2
  let m = addr s
  foo3(m)
  let m2 = cast[pointer](m)
  foo3(m2)

type
  Foo1314 = object
    a, b, c, d, e: int


proc foo1314(x: Foo1314) =
  let s = x.a

var a = Foo1314()
foo1314(a)

proc foo2233() {.nimcall.} =
  var x: proc () {.cdecl.}

# Compile error
  var y: proc () {.nimcall.}

foo2233()

proc basictuple() =
  var t: (int, int)
  t = (1, 2)
  t = (a: 1, b: 2)
  t = (x: 1, y: 2)
  discard t[0]
  discard t[1]

basictuple()

discard "11223"

discard """
  dw3euuyghid23u23dhiw
"""

discard r"""136t54321"""

import std/syncio

proc classify1(s: string) =
  echo s

classify1("9123345")


block:
  proc classify(s: string) =
    echo s

  classify("9123345")
  block:
    proc classify2(s: string) =
      echo s

    classify2("9123345")

block:
  proc classify2(s: string) =
    echo s

  classify2("9123345")

block:
  proc classify(s: string) =
    echo s

  classify("9123345")

block:
  block:
    proc classify(s: string) =
      echo s

    classify("9123345")

proc foo2() =
  proc classify(s: string) =
    echo s

  classify("9123345")

foo2()

proc bar(): string =
  return (let x = "1212334"; x)

assert bar() == "1212334"
