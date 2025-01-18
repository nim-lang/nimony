type Foo = object
  val: int

converter toFoo(x: int): Foo = Foo(val: x)

proc bar(x: Foo) = discard

bar(123)
