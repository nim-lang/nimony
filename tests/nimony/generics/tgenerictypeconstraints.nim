type Foo[T: SomeInteger] = object
  x: T

var a: Foo[int]
var b: Foo[uint8]
var c: Foo[float]
var d: Foo[int, int]

# bug #1339

proc bar[T: SomeNumber](x: T; y: string) = discard

bar(1.0, false) # Type mismatch error with wrong position
