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
