type Foo* = object
  value*: float

func newFoo*(x: float): Foo =
  Foo(value: x)
