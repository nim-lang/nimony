type Foo[T] = object
  val: T

type Addable = concept
  proc `+`(a, b: Self): Self

proc foo[T: Addable](a: T): Foo[T] =
  var x = Foo[T](val: a + a)
  discard x.val + x.val
  discard x.val + a
  discard a + x.val
  x.val = a + a
  x.val = x.val + x.val
  x.val = x.val + a
  x.val = a + x.val
  result = x

let x = foo(123)
let y: Foo[int] = x

type Bar[T] = object
  field: ptr T

proc bar[U](a: ptr U) =
  var x = Bar[U](field: a)
  if x.field == nil:
    # typed magic here: needs to produce `(ptr U)` and not `(ptr T)`
    discard

bar(addr x)
