type
  MyObjectBase = object
    x, y: int

  MyObject[T, Y] = object
    x, y: int

proc `=trace`(x: var MyObjectBase, p: pointer) =
  discard

proc `=wasMoved`(x: var MyObjectBase) =
  discard

proc `=copy`(x: var MyObjectBase, y: MyObjectBase) =
  discard

proc `=sink`(x: var MyObjectBase, y: MyObjectBase) =
  discard

proc `=destroy`(x: MyObjectBase) =
  discard

proc `=wasMoved`[T, Y](x: var MyObject[T, Y]) =
  let x = 12
  let y = 4

proc `=copy`[T, Y](x: var MyObject[T, Y], y: MyObject[T, Y]) =
  discard

proc `=sink`[T, Y](x: var MyObject[T, Y], y: MyObject[T, Y]) =
  discard

proc `=destroy`[T, Y](x: MyObject[T, Y]) =
  let x = 12

proc foo =
  var obj = default MyObject[int, float]

  var obj2 = default MyObject[float, int]

  `=destroy`(obj)

  var objbase = default MyObjectBase
  var objbase2 = default MyObjectBase
  `=destroy`(objbase2)
  `=destroy`(objbase)

foo()

proc `=checkHook`[T, Y](x:  MyObject[T, Y]) =
  discard

var obj: MyObject[int, float]
`=checkHook`(obj)
