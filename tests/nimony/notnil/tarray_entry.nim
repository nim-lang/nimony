type Foo = object
  x: int

var a: array[8, ref Foo]

a[0] = (ref Foo)(x: 3)
a[1] = nil
