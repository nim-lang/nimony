type Foo* = object
  x*: int

proc mk*(): seq[Foo] = @[Foo(x: 1)]
