type
  Foo[T] = object
  Bar[T] = object

proc foo[T](x: Foo[Bar[T]]) = discard
var x = Foo[Bar[int]]()
foo x
