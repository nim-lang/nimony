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
