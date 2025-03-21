import std/syncio

type Int = int


type Object = object
  a, b: int

block:
  type Int = int


  type Object = object
    a, b: int

  var s: Int = 10
  var obj = Object(a: 1, b: 2)

proc foo =
  type Int2 = int

  type Object2 = object
    a, b: int
  const Six = 6
  var a: Int2 = Six
  var s: Int = 10
  var obj = Object(a: 1, b: 2)
  var obj2 = Object2(a: 1, b: 2)

foo()
