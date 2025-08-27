import std/assertions

discard default(string)
discard default(int)

discard default(ptr int)
type Enum = enum a, b, c
discard default(Enum)

type Obj = object
  x: int
  y: string
  z: tuple[a: bool, b: Enum]

discard default(Obj)

proc foo(x: int; y: string): int =
  var data = Obj()
  var x = "abc"
  result = 4

type
  MyObject = object
    x, y: int

var global: MyObject

global.x = 45
discard foo(global.x, "123")
discard global.x.foo("123")
global = MyObject(x: 123)
global = MyObject(x: 123, y: 456)
template resem() =
  global = MyObject(x: 123)
  global = MyObject(x: 123, y: 456)
resem()

type GenericObj[T] = object
  x: T

var generic: GenericObj[int]
generic.x = 45
generic = GenericObj[int](x: 123)
proc genericProc[T](x: T): GenericObj[T] =
  result = GenericObj[T](x: x)
  result.x = x
generic = genericProc(456)

type EmptyObj = object
let empty = EmptyObj()
type EmptyGenericObj[T] = object
let emptyGeneric = EmptyGenericObj[int]()

block:
  var x = default(array[2, int])
  assert x[0] == 0
  assert x[1] == 0

  var y = default(array[2, array[2, int]])
  assert y[0][0] == 0
  assert y[0][1] == 0
  assert y[1][0] == 0
  assert y[1][1] == 0

  type
    ObjWithArray = object
      x: array[2, int]

  var obj = default(ObjWithArray)
  assert obj.x[0] == 0
  assert obj.x[1] == 0

  var objs = default(array[2, MyObject])
  assert objs[0].x == 0
  assert objs[0].y == 0
  assert objs[1].x == 0
  assert objs[1].y == 0

block:
  var p = default(pointer)
  assert p == nil

  type
    Foo = object
      p: pointer

  var foo = default(Foo)
  assert foo.p == nil

block:
  type
    Rune = distinct int

  proc foo() =
    var res = newSeq[Rune](1)
    assert res[0].int == 0

    var des = default(Rune)
    assert des.int == 0

  foo()
