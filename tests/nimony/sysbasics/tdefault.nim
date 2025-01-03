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
# type is left as InvokeT for now because requestRoutineInstance doesn't semcheck the return type:
discard genericProc(456)
