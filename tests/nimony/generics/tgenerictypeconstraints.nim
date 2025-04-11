type Foo[T: SomeInteger] = object
  x: T

var a: Foo[int]
var b: Foo[uint8]
var c: Foo[float]
var d: Foo[int, int]
