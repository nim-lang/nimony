type Foo[T] = object
  val: T

proc invoke[T](x: ptr Foo[T]): T = x[].val

var concrete = Foo[int](val: 123)
let x = invoke(addr concrete)
let y: int = x

proc dummy(x: int): int = discard
proc procType(p: ptr proc (y: int): int) = discard
var p: proc (z: int): int = dummy
procType(addr p)

proc intbit(i: ptr int64) = discard
var i: int = 123
intbit(addr i)

# issue #969
discard default(cint)
