type Foo = ref object
  x: int

var a: array[8, Foo]

a[0] = Foo(x: 3)
a[1] = nil
